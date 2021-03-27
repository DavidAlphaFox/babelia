#!chezscheme
(library (arew untangle)

  (export make-untangle
          make-untangle-channel
          untangle-accept
          untangle-bind
          untangle-channel?
          untangle-channel-recv
          untangle-channel-select
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
  ;; TODO: try to box-cas! a number of magic N number of times and
  ;; raise an exception.
  (define (box-cons! box item)
    ;; Add ITEM to the front of the list that is in BOX
    (unless (box-cas! box lst (cons item (unbox box)))
      (box-cons! box item)))

  (define (box-append! box lst)
    ;; Add LST to the front of the list that is in BOX

    ;; TODO: Benchmark against an implementation that rely on
    ;; box-cons!
    (define old (unbox box))
    (unless (box-cas! box box (append lst old))
      (box-append! box lst)))

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

  (define untangled? (make-parameter #f))

  (define (make-untangle)
    (make-untangle% 'init
                    #f
                    0
                    (box '())
                    (make-entangle)))

  (define (untangle-spawn untangle thunk)
    ;; THUNK will be executed at the next tick. See
    ;; untangle-exec-expired-continuations.
    (define when (untangle-time untangle))
    ;; Since there is no fast lock-free or thread-safe priority queue,
    ;; then the code rely on a list of pairs. At least, with Babelia,
    ;; the lock-free or thread-safe priority queue, MIGHT NOT be worth
    ;; the effort.
    (define item (cons when thunk))
    (box-cons! (untangle-queue untangle) item))

  (define (untangle-stop untangle)
    (untangle-status! untangle 'stopping))

  (define (untangle-start untangle)
    ;; Only one instance of <untangle> can be active per POSIX thread.
    (assume? (not (untangled?)))
    (untangled? #t)
    (untangle-status! untangle 'running)
    (let loop ()
      (when (and (untangle-busy? untangle)
                 (not (eq? (untangle-status untangle)
                           'stopping)))
        (untangle-tick untangle)
        (loop)))
    (untangle-status! untangle 'stopped)
    (untangled? #f))

  (define (untangle-busy? untangle)
    ;; TODO: rename to untangle-continue? and include the check on
    ;; status.
    (raise 'not-implemented))

  (define (untangle-tick untangle)
    (untangle-time! untangle (current-time 'time-monotonic))
    (untangle-exec-expired-continuations untangle)
    (untangle-exec-network-continuations untangle))

  (define (untangle-sleep untangle nanoseconds seconds)

    (define (on-sleep resume)
      ;; RESUME is untangle-sleep continuation.
      (define delta (make-time 'time-duration nanoseconds seconds))
      (define when (add-duration (untangle-time untangle) delta))
      (define item (list (cons when resume)))
      (box-cons! (untangle-queue untangle) item))

    ;; untangle-sleep is necessarily called while untangle is running,
    ;; where untangled? returns #t, otherwise user code need to call
    ;; POSIX sleep.
    (assume (untangled?))
    (escape untangle on-sleep))

  (define (untangle-exec-expired-continuations untangle)

    (define (queue-snapshot-and-nullify)
      (unbox-and-swap (untangle-queue untangle) '()))

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

    (define waiting (queue-snapshot-and-nullify))
    (define pending (filter-map call-or-keep waiting))

    (box-append! (untangle-queue untangle) pending))

  (define (untangle-exec-network-continuations untangle)
    (entangle-continue (untangle-entangle untangle)))

  ;; channel

  (define-record-type <untangle-channel>
    (make-untangle-channel% untangle inbox waiters)
    untangle-channel?
    (untangle channel-untangle)
    (inbox channel-inbox)
    (waiters channel-waiters))

  (define-record-type <waiter>
    (make-waiter resume obj mutex condition)
    waiter?
    (resume waiter-resume%)
    (obj waiter-obj waiter-obj!)
    (mutex waiter-mutex)
    (condition waiter-condition))

  (define (waiter-resume obj)
    ((waiter-resume% waiter) obj))

  (define (untangle-make-channel untangle)
    (make-untangle-channel% untangle (box '()) (box '())))

  (define untangle-closing-singleton '(untangle-closing-singleton))

  (define (untangle-channel-send channel obj)

    (define waiter (box-uncons! (channel-waiters channel) #f))

    (if waiter
        (waiter-resume obj)
        (box-cons! (channel-inbox channel) obj)))

  (define (untangle-channel-recv-one channel)
    (define untangle (untangle))
    (if (eq? (untangle-status (channel-untangle)) 'stopping)
        untangle-closing-singleton
        (if untangle
            (channel-recv-one-with-untangle untangle channel)
            (channel-recv-one-without-untangle channel))))

  (define inbox-empty '(inbox-empty))

  (define (channel-recv-one-with-untangle untangle channel)

    (define (resume waiter obj k)
      ;; When box-remove! find waiter in channel-waiters, it returns
      ;; true. Otherwise, it means some other POSIX thread acquired
      ;; the waiter and already resumed the waiter.
      (when (box-remove! (channel-waiters channel) waiter)
        ;; Resume the green thread with K in the POSIX thread that is
        ;; receiving and not in the POSIX thread that created CHANNEL.
        (untangle-spawn untangle (lambda () (k obj)))))

    (define (pause!! k)
      (define waiter (make-waiter (lambda (obj) (resume waiter obj k))
                                  #f #f #f #f))
      (box-cons! (channel-waiters channel) waiter))

    (define (pause!)
      ;; Mind the fact that the escapade is bound to the calling
      ;; <untangle> instance that is UNTANGLE.
      (escape untangle pause!!))

    (if (eq? (untangle-status untangle) 'stopping)
        untangle-closing-singleton
        (let ((obj (box-uncons! (channel-inbox channel)
                                obj
                                inbox-empty)))
          (if (eq? obj inbox-empty)
              (pause!)
              obj))))

  (define (channel-recv-one-without-untangle channel)

    (define (wait-and-return!)

      (define (resume waiter obj)
        (when (box-remove! (channel-waiters channel) waiter)
          ;; there is no need to protect WAITER with mutex, since it
          ;; was removed from channel-waiters atomically, there can be
          ;; no race conditions.
          (waiter-obj! waiter obj)
          (condition-signal (waiter-condition channel))))

      (define waiter (make-waiter (lambda (obj) (resume waiter obj))
                                  #f
                                  #f
                                  (make-mutex)
                                  #f
                                  (make-condition)))

      (box-cons! (channel-waiters channel) waiter)

      (with-mutex (waiter-mutex waiter)
        (condition-wait (waiter-condition waiter)
                        (waiter-mutex waiter)))

      (waiter-obj waiter))

    (define obj (box-uncons! (channel-inbox channel) obj inbox-empty))

    (if (eq? obj inbox-empty)
        (wait-and-return!)
        obj))

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

  (define (untangle-channel-recv channels)
    ;; Since untangle-channel-send will not pause the calling thread,
    ;; there does not seem to be a point in building equivalent to
    ;; select, epoll, or kqueue. Untangle user only need to be
    ;; notified when a channel among several others can be read.

    (define (channel-stopping? channel)
      (eq? (untangle-status (channel-untangle)) 'stopping))

    (define (stopping?)
      (generator-any?
       (generator-map channel-stopping?
                      (list->generator channels))))

    (define untangle (untangle))

    (if (or (stopping?) (eq? (untangle-status untangle)
                             'stopping))
        untangle-closing-singleton
        (if untangle
            (channel-recv-with-untangle untangle channels)
            (channel-recv-without-untangle channels))))

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

  (define (channel-recv-with-untangle untangle channels)

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

    (define (make-shared-waiter channel pool mutex k)

      (define (maybe-remove!? channel waiter)
        (let loop ((waiters (channel-waiters channel))
                   (out '()))
          (if (null? waiters)
              #f ;; WAITER was not found channel-waiters
              (if (eq? (car waiters) waiter)
                  (and (channel-waiters! (append out (cdr waiters)))
                       #t)
                  (loop (cdr waiters)
                        (cons (car waiters) out))))))

      (define (resume obj)
        ;; first try to remove all waiters from the pool, to avoid
        ;; that this procedures is called twice.
        (with-mutex mutex
          (let ((continue? (apply maybe-for-each?
                                  maybe-remove!?
                                  (unbox poll))))
          ;; When maybe-for-each? returns #t, it means all the waiters
          ;; were found in registred channels in POOL.  Otherwise, one
          ;; was not found, hence none is present.  That is none,
          ;; because the expression is protected with a mutex, hence
          ;; if the code went through it it removed everything ie. it
          ;; can not be the partial of a concurrent excecution of
          ;; resume.

          ;; When two POSIX threads, race to send on CHANNELS, resume
          ;; might be called twice, but only one will win, because the
          ;; waiter should only be resumed once.

          ;; At this point the mutex has done its job. The code
          ;; is wrapped inside with-mutex to avoid set!...
          (when continue?
            ;; continuation is the same whatever the waiter, since
            ;; they reprenst the same recv call.
            (untangle-spawn untangle (lambda () (k obj)))))))

      (define waiter (make-waiter resume #f #f #t #f))

      (set-box! pool (cons (list channel waiter) (unbox pool)))

      waiter)

    (define (make-waiters k)
      (define mutex (make-mutex))
      (define pool (box '()))

      (map (lambda (channel)
             (make-shared-waiter channel pool mutex k))
           channels))

    (define (pause!! k)
      (define waiters (make-waiters k))
      (for-each (lambda (c w) (box-cons! (channel-waiters c) w))
                channels waiters))

    (define (pause!)
      (escape untangle pause!!))

    (call-with-values inboxes-pop!
      (lambda (channel obj)
        (if channel
            (values channel obj)
            (pause!)))))

  (define (channel-recv-without-untangle channels)

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

    (define (make-shared-waiter channel out mutex condition pool)

      (define (maybe-remove!? channel waiter)
        (let loop ((waiters (channel-waiters channel))
                   (out '()))
          (if (null? waiters)
              #f ;; WAITER was not found channel-waiters
              (if (eq? (car waiters) waiter)
                  (and (channel-waiters! (append out (cdr waiters)))
                       #t)
                  (loop (cdr waiters)
                        (cons (car waiters) out))))))

      (define (resume obj)
        (with-mutex mutex
          (let ((continue? (apply maybe-for-each?
                                  maybe-remove!?
                                  (unbox poll))))
            (when continue?
              (set-box! out obj)
              (condition-signal condition)))))

      (define waiter (make-waiter resume #f #f #t #f))

      (set-box! pool (cons (list channel waiter) (unbox pool)))

      waiter)

    (define (make-waiters out mutex condition)

      (define pool (box '()))

      (map (lambda (channel)
             (make-shared-waiter channel
                                 out
                                 mutex
                                 condition
                                 pool))
           channels))

    (define (wait-and-return!)

      (define out (box #f))
      (define mutex (make-mutex))
      (define condition (make-condition))

      (define waiters (make-waiters out mutex condition))

      (for-each (lambda (c w) (box-cons! (channel-waiters c) w))
                channels waiters))

      (with-mutex (waiter-mutex waiter)
        (condition-wait (waiter-condition waiter)
                        (waiter-mutex waiter)))

      (waiter-obj waiter))

    (call-with-values inboxes-pop!
      (lambda (channel obj)
        (if channel
            (values channel obj)
            (wait-and-return!)))))
