(define-module (babelia crawler tests))


(import (babelia testing))
(import (babelia crawler robots))


(define-public test-000
  (test
   '("https://hyper.dev/other.xml"
     "https://hyper.dev/sitemap.xml")
   (robots.txt-sitemaps "sitemap: https://hyper.dev/sitemap.xml

# some comment

sitemap: https://hyper.dev/other.xml


")))

(define robots.txt->scm (@@ (babelia crawler robots) robots.txt->scm))

(define robots.txt (make-robots.txt "babelia" "sitemap: foobar

user-agent: babelia
crawl-delay: 666

user-agent: babelia
#  comment
disallow: /nobot
crawl-delay: 5
disallow: /cgi/*/script
"))

(define-public test-001
  (test 5
        (robots.txt-delay robots.txt)))

(define-public test-002
  (test #t
        (robots.txt-allow? robots.txt "/path/to/somewhere")))

(define-public test-003
  (test #f
        (robots.txt-allow? robots.txt "/nobot")))

(define-public test-004
  (test #f
        (robots.txt-allow? robots.txt "/nobot/sub/component")))

(define-public test-005
  (test #f
        (robots.txt-allow? robots.txt "/cgi/foo/bar")))

(define-public test-006
  (test #t
        (robots.txt-allow? robots.txt "/cgi")))

(define robots.txt2 (make-robots.txt "babelia" "sitemap: foobar

user-agent: *
#  comment
disallow: /nobot
crawl-delay: 5
disallow: /cgi/*/script
"))

(define-public test-007
  (test 5
        (robots.txt-delay robots.txt2)))

(define-public test-008
  (test #t
        (robots.txt-allow? robots.txt2 "/path/to/somewhere")))

(define-public test-009
  (test #f
        (robots.txt-allow? robots.txt2 "/nobot")))

(define-public test-010
  (test #f
        (robots.txt-allow? robots.txt2 "/nobot/sub/component")))

(define-public test-011
  (test #f
        (robots.txt-allow? robots.txt2 "/cgi/foo/bar")))

(define-public test-012
  (test #t
        (robots.txt-allow? robots.txt2 "/cgi")))
