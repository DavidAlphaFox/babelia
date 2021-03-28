#!chezscheme
(library (arew untangle)

  (export make-untangle
          make-untangle-channel
          untangle-accept
          untangle-bind
          untangle-channel?
          untangle-channel-recv
          untangle-channel-recv*
          untangle-channel-send
          untangle-closing?
          untangle-listen
          untangle?
          untangle-sleep
          untangle-status
          untangle-socket-accumulator
          untangle-socket-generator
          untangle-spawn
          untangle-stop
          untangle-time)

  (import (only (chezscheme)
                box unbox set-box! box-cas!
                make-mutex with-mutex
                current-time make-time add-duration time<=?
                make-condition condition-wait condition-signal)
          (scheme base)
          (srfi srfi-145)
          (arew entangle)
          (arew socket))

  ;;; box helpers that use box-cas!
  ;;
  ;; TODO: try to box-cas! a magic N number of times and raise an
  ;; exception.
  (define (box-cons! box item)
    ;; Add ITEM to the front of the list that is in BOX
    (unless (box-cas! box lst (cons item (unbox box)))
      (box-cons! box item)))

  (define (box-adjoin! box lst)
    ;; Add the elements of LST of the list that is in BOX without
    ;; preserving the order.
    (let loop ((lst lst))
      (unless (null? lst)
        (box-cons box (car lst))
        (loop (cdr lst)))))

  (define (box-uncons! box default)
    ;; Remove the first item from the front of the list that is in BOX
    ;; and return it. If the list is empty, return DEFAULT.
    (define lst (unbox box))
    (if (null? lst)
        default
        (let ((item (car lst))
              (tail (cdr lst)))
          (if (box-cas! box lst tail)
              item
              (box-uncons! box default)))))

  (define (unbox-and-swap box new)
    ;; Retrieve the value in the BOX and replace it with NEW.
    (define old (unbox box))
    (if (box-cas! box old new)
        old
        (unbox-and-swap box new)))

  ;;
  ;; XXX: Inside call/ec, escapade-singleton allows to tell
  ;; how call/ec thunk continuation is called:
  ;;
  ;; - nominal case: thunk returned.
  ;;
  ;; - thunk called untangle-escapade, in that case escapade-singleton
  ;; is the first argument of the continuation of thunk inside
  ;; call/ec.
  ;;
  ;; escapade-singleton is a singleton, user code can not create it.
  ;;

  (define escapade-singleton '(escapade-singleton))

  ;; call/ec = call-with-escape-continuation
  (define (call/ec untangle thunk)
    (call-with-values (lambda ()
                        (call/1cc
                         (lambda (k)
                           ;; K will allow to escape THUNK.
                           (assume (not (untangle-escapade untangle)))
                           (untangle-escapade! untangle k)
                           (thunk))))
      (lambda args
        ;; args may be the empty list if THUNK returns nothing.
        (if (and (pair? args) (eq? (car args) escapade-singleton))
            ;; XXX: The following code is the escapade handler! That
            ;; is always a proc and a continuation, because of how
            ;; escape is implemented. Racket call/ec has an optional
            ;; argument called handler that allows to change that
            ;; behavior.
            (let ((proc (cadr args))
                  (k (caddr args)))
              ;; call the procedure proc passed to untangle-escapade
              ;; with its continuation called k. That is, k, is what
              ;; follow escapade call inside THUNK. K will allow to
              ;; resume THUNK.
              (proc k))
            (apply values args)))))

  (define (escape untangle proc)
    ;; XXX: Capture the continuation and call it later, that is why it
    ;; use call/cc instead of call/1cc.
    (call/cc
     (lambda (k)
       ;; save escapade
       (define escapade (untangle-escapade untangle))
       ;; The escapade is a call/1cc continuation, no need to keep it
       ;; around, and it might lead to strange bugs.
       (untangle-escapade! untangle #f)
       ;; XXX: escapade is the continuation of a thunk inside call/ec
       ;; whereas k is the continuation of the caller of escape inside
       ;; thunk.

       ;; XXX: Continue with thunk's continuation inside call/ec as
       ;; known as escapade. Inside call/ec, proc and k are used to
       ;; build the escape handler.
       (escapade escapade-singleton proc k))))

  (define-record-type <untangle>
    (make-untangle% status escape time queue entangle)
    untangle?
    (status untangle-status untangle-status!)
    (escape untangle-escapade untangle-escapade!)
    (time untangle-time untangle-time!)
    (queue untangle-queue untangle-queue!)
    (entangle untangle-entangle))

  (define untangled (make-parameter #f))

  (define (make-untangle)
    (make-untangle% 'init
                    #f
                    0
                    (box '())
                    (make-entangle)))

  (define (untangle-spawn% untangle thunk)
    (assume untangle)
    ;; THUNK will be executed at the next tick. See
    ;; untangle-exec-expired-continuations.
    (define when (untangle-time untangle))
    ;; Since there is no fast lock-free or thread-safe priority queue,
    ;; then the code rely on a list of pairs. At least, with Babelia,
    ;; the lock-free or thread-safe priority queue, MIGHT NOT be worth
    ;; the effort.
    (define item (cons when thunk))
    (box-cons! (untangle-queue untangle) item))

  (define untangle-spawn
    (case-lambda
     ((thunk) (untangle-spawn% (untangled) thunk))
     ((untangle thunk) (untangle-spawn% untangle thunk))))

  (define (untangle-stop% untangle)
    (assume untangle)
    (untangle-status! untangle 'stopping))

  (define untangle-stop
    (case-lambda
     (() (untangle-stop% (untangled)))
     ((untangle) (untangle-stop% untangle))))

  (define untangle-stopping?
    (case-lambda
     (() (untangle-stopping? (untangled)))
     ((obj)
      (or (eq? obj 'stopping-singleton)
          (eq? (untangle-status untangle) 'stopping)))))

  (define (untangle-start untangle)
    ;; Only one instance of <untangle> can be active per POSIX thread.
    ;; untangle-start can not be called from a green thread, or a
    ;; POSIX thread forked in green thread. POSIX thread be must setup
    ;; before calling untangle-start in the main thread.
    (assume? (not (untangled)))
    (untangled untangle)
    (untangle-status! untangle 'running)
    (let loop ()
      (when (untangle-continue? untangle)
        (untangle-tick untangle)
        (loop)))
    (untangle-status! untangle 'stopped)
    (untangled? #f))

  (define (untangle-continue? untangle)
    (not (untangle-stopping? untangle)))

  (define (untangle-tick untangle)
    (untangle-time! untangle (current-time 'time-monotonic))
    (untangle-exec-expired-continuations untangle)
    (untangle-exec-network-continuations untangle))

  (define (untangle-sleep nanoseconds seconds)

    (define untangle (untangled))

    (define (on-sleep resume)
      ;; RESUME is untangle-sleep continuation.
      (define delta (make-time 'time-duration nanoseconds seconds))
      (define when (add-duration (untangle-time untangle) delta))
      (define item (list (cons when resume)))
      (box-cons! (untangle-queue untangle) item))

    ;; untangle-sleep is necessarily called while untangle is running,
    ;; where untangled is true, otherwise user code need to call POSIX
    ;; sleep.
    (assume untangle)
    (escape untangle on-sleep))

  (define (untangle-exec-expired-continuations untangle)

    (define time (untangle-time untangle))

    (define (call-or-keep time+thunk)
      (if (time<=? (car time+thunk) time)
          ;; A green thread wants to wake up!
          (begin
            ;; Call the registred thunk
            ((cdr time+thunk))
            ;; There is no need to call this thunk in the future,
            ;; because it was already called.
            #f)
          time+thunk))

    (define (filter-map proc lst)
      (let loop ((lst lst)
                 (out '()))
        (if (null? lst)
            out
            (let ((keep? (proc (car lst))))
              (if keep?
                  (loop (cdr lst) (cons (car lst) out))
                  (loop (cdr lst) out))))))

    (define waiting (unbox-and-swap (untangle-queue untangle) '()))
    (define pending (filter-map call-or-keep waiting))

    (box-adjoin! (untangle-queue untangle) pending))

  (define (untangle-exec-network-continuations untangle)
    (entangle-continue (untangle-entangle untangle)))

  ;; channel

  (define-record-type <untangle-channel>
    (make-untangle-channel% untangle inbox resumers)
    untangle-channel?
    (untangle channel-untangle)
    (inbox channel-inbox)
    (resumers channel-resumers))

  (define untangle-make-channel
    (case-lambda
     (() (untangle-make-channel (untangled)))
     ((untangle)
      (assume untangle)
      (make-untangle-channel% untangle (box '()) (box '())))))

  (define stopping-singleton '(stopping-singleton))

  (define (untangle-channel-send channel obj)

    (define resumer (box-uncons! (channel-resumers channel) #f))

    (if resumer
        (resumer obj)
        (box-cons! (channel-inbox channel) obj)))

  (define (untangle-channel-recv channel)
    ;; User can only receive inside the current event-loop for
    ;; entanglements look into channels...
    (define untangle (untangled))
    (if untangle
        (channel-recv-with-untangle untangle channel)
        (channel-recv-without-untangle channel)))

  (define inbox-empty '(inbox-empty))

  (define (channel-recv-with-untangle untangle channel)

    (define (on-pause k)
      (define (resumer obj)
        ;; When box-remove! find resumer in channel-resumers, it
        ;; returns true. Otherwise, it means some other POSIX thread
        ;; acquired the resumer and already resumed it.
        (when (box-remove! (channel-resumers channel) resumer)
          ;; Resume the green thread with K in the POSIX thread that
          ;; is receiving and not in the POSIX thread that created
          ;; CHANNEL.
          (if (untangle-stopping? untangle)
              (untangle-spawn untangle
                              (lambda ()
                                (k stopping-singleton)))
              (untangle-spawn untangle (lambda () (k obj))))))

      (box-cons! (channel-resumers channel) resumer))

    (define (pause)
      ;; Mind the fact that the escapade is bound to the calling
      ;; <untangle> instance that is UNTANGLE.
      (escape untangle on-pause))

    (if (untangle-stopping? untangle)
        stopping-singleton
        (let ((obj (box-uncons! (channel-inbox channel)
                                obj
                                inbox-empty)))
          (if (eq? obj inbox-empty)
              (pause)
              obj))))

  (define (channel-recv-without-untangle channel)

    (define (wait-and-return!)

      (define out #f)
      (define mutex (make-mutex))
      (define condition (make-condition))

      (define (resumer obj)
        (when (box-remove! (channel-resumers channel) resumer)
          ;; There is no need to protect RESUMER with mutex, since it
          ;; was removed from channel-resumers atomically, there can
          ;; be no race conditions.
          (set! out obj)
          (condition-signal condition)))

      (box-cons! (channel-resumers channel) resumer)

      (with-mutex mutex
        (condition-wait condition mutex))

      out)

    (define obj (box-uncons! (channel-inbox channel) obj inbox-empty))

    (if (untangle-stopping? (channel-untangle channel))
        stopping-singleton
        (if (eq? obj inbox-empty)
            (wait-and-return!)
            obj)))

  (define (list->generator lst)
    (lambda ()
      (if (null? lst)
          (eof-object)
          (let ((head (car lst)))
            (set! lst (cdr lst))
            head))))

  (define (generator-any? generator)
    (let ((item (generator)))
      (if (eof-object? item)
          #f
          (if item
              #t
              (generator-any? generator)))))

  (define (generator-map proc generator)
    (lambda ()
      (let ((item (generator)))
        (if (eof-object? item)
            item
            (proc item)))))

  (define (untangle-channel-recv* channels)
    ;; Since untangle-channel-send will not pause the calling thread,
    ;; there does not seem to be a point in building equivalent to
    ;; select, epoll, or kqueue. Untangle user only need to be
    ;; notified when one channel among other channels has an obj
    ;; ready.

    (define untangle (untangled))

    (if untangle
        (channel-recv-with-untangle* untangle channels)
        (channel-recv-without-untangle* channels)))

  (define (maybe-for-each? proc a b)
    ;; similar to for-each, except it stops as soon as PROC
    ;; returns #f.
    (let loop ((a a)
               (b b))
      (if (null? a)
          #t
          (if (proc (car a) (car b))
              (loop (cdr a) (cdr b))
              #f))))

  (define (channel-stopping? channel)
    (untangle-stopping? (channel-untangle channel)))

  (define (channel-recv-with-untangle* untangle channels)

    (define mutex (make-mutex))
    (define pool '())

    (define (inboxes-pop!)
      (let loop ((channels channels))
        (if (null? channels)
            (values #f #f)
            (let* ((channel (car channels))
                   (out (box-uncons! (channel-inbox channel)
                                     inbox-empty)))
              (if (eq? out inbox-empty)
                  (loop (cdr channels))
                  (values channel out))))))

    (define (make-resumer channel k)

      (define (maybe-remove!? channel resumer)

        (define box (channel-resumers channel))

        (define resumers (unbox-and-swap box '()))

        (define (adjoin! out)
          (channel-resumers! channel
                             (box-adjoin! box out)))

        (let loop ((resumers resumers)
                   (out '()))
          (if (null? resumers)
              #f ;; RESUMER was not found channel-resumers
              (if (eq? (car resumers) resumer)
                  (begin
                    (adjoin! out)
                    #t)
                  (loop (cdr resumers)
                        (cons (car resumers) out))))))

      (define (resumer obj)
        ;; first try to remove all resumers from the pool, to avoid
        ;; that this procedure is called twice.
        (with-mutex mutex
          (let ((continue? (apply maybe-for-each?
                                  maybe-remove!?
                                  poll)))
            ;; When maybe-for-each? returns #t, it means all the
            ;; resumers were found in the registred channels in POOL.
            ;; Otherwise, one was not found, hence none is present.
            ;; That is none, because the expression is protected with
            ;; a mutex, hence if the code went through it, it removed
            ;; everything ie. it can not be the partial of a
            ;; concurrent excecution of the procedure resume.

            ;; When two POSIX threads, race to send on CHANNELS,
            ;; resume might be called twice, but only one will win,
            ;; because the resumer should only be resumed once.

            ;; At this point the mutex has done its job. The code
            ;; is wrapped inside with-mutex to avoid set!...
            (when continue?
              ;; continuation is the same whatever the resumer, since
              ;; they represent the same recv call.
              (if (or (untangle-stopping? untangle)
                      (untangle-stopping? (channel-untangle channel)))
                  (untangle-spawn untangle
                                  (lambda () (k channel
                                                stopping-singleton)))
                  (untangle-spawn untangle
                                  (lambda () (k channel obj))))))))

      (set! pool (cons (list channel resumer) pool))

      resumer))

  (define (on-pause k)
    (for-each (lambda (c)
                (box-cons! (channel-resumers c) (make-resumer c k)))
              channels))

  (define (pause)
    (escape untangle on-pause))

  (define (stopping?)
    (or (untangle-stopping untangle)
        (generator-any?
         (generator-map channel-stopping?
                        (list->generator channels)))))

  (if (stopping?)
      stopping-singleton
      (call-with-values inboxes-pop!
        (lambda (channel obj)
          (if channel
              (values channel obj)
              (pause)))))

  (define (channel-recv-without-untangle* channels)

    (define out #f)
    (define mutex (make-mutex))
    (define condition (make-condition))
    (define pool '())

    (define (inboxes-pop!)
      (let loop ((channels channels))
        (if (null? channels)
            (values #f #f)
            (let* ((channel (car channels))
                   (out (box-uncons! (channel-inbox channel)
                                     inbox-empty)))
              (if (eq? out inbox-empty)
                  (loop (cdr channels))
                  (values channel out))))))

    (define (make-resumer channel)

      (define (maybe-remove!? channel resumer)

        (define box (channel-resumers channel))

        (define resumers (unbox-and-swap box '()))

        (define (adjoin! out)
          (channel-resumers! channel
                             (box-adjoin! box out)))

        (let loop ((resumers resumers)
                   (out '()))
          (if (null? resumers)
              #f ;; RESUMER was not found channel-resumers
              (if (eq? (car resumers) resumer)
                  (begin
                    (adjoin! out)
                    #t)
                  (loop (cdr resumers)
                        (cons (car resumers) out))))))

      (define (resumer obj)
        (with-mutex mutex
          (let ((continue? (apply maybe-for-each?
                                  maybe-remove!?
                                  pool)))
            (when continue?
              (if (channel-stopping channel)
                  (set! out (list channel stopping-singleton))
                  (set! out (list channel obj)))
              (condition-signal condition)))))

      (set! pool (cons (list channel resumer) pool))

      resumer)

    (define (stopping?)
      (or (untangle-stopping untangle)
          (generator-any?
           (generator-map channel-stopping?
                        (list->generator channels)))))

    (for-each (lambda (c)
                (box-cons! (channel-resumers c) (make-resumer c)))
              channels)

    (if (stopping?)
        stopping-singleton
        (with-mutex mutex
          (condition-wait condition
                          mutex)
          (apply values out)))))
