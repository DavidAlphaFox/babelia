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

(define-public test-001
  (test '(("babelia" 5 ("/cgi/" "/nobot")) (default #f ()))
        (robots.txt->scm "sitemap: foobar

user-agent: babelia
#  comment
disallow: /nobot
crawl-delay: 5
disallow: /cgi/*/script")))
