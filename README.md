# guile-babelia

Wanna be search engine with federation support

[![babel tower beamed by an alien spaceship](https://cdn.dribbble.com/users/2441249/screenshots/4890251/babeldrbl.jpg)](https://dribbble.com/shots/4890251-Babel)

(artwork by [mildtravis](https://dribbble.com/mildtravis))

## Dependencies

- guile 2.9.4
- guile-fibers 1.0.0
- guile-gcrypt 0.2.0
- wiredtiger 3.0.0
- snowball stemmer, see [my guix channel](https://git.sr.ht/~amz3/guix-amz3-channel).

## v0.1.0

- [x] wiredtiger bindings
- [x] ~~srfi-128 (comparators)~~, not required since `(mapping hash)`
      was replaced with `fash`
- [x] ~~srfi-146 (mappings hash)~~, use
      [fash](https://www.wingolog.org/pub/fash.scm)
- [x] srfi-158 (generators)
- [x] web server
- [x] theme
- [x] api stub
- [x] pool of workers to execute blocking operations
- [x] snowball stemmer bindings
- [x] html2text
- [ ] okvs abstractions
  - [x] okvs (srfi-167)
  - [x] `pack` and `unpack`
  - [x] `<engine>` type class object
  - [x] wiredtiger backend
  - [x] nstore (srfi-168)
  - [x] [ulid](https://github.com/ulid/spec)
  - [x] make `thread-index` a global
  - [x] move okvs abstractions inside okvs directory (fts, counter,
        nstore, ulid...)
  - [x] ulid store, rename object.scm to okvs/ustore.scm
  - [x] add tests to ulid.scm
  - [x] clean up: use with-directory from babelia/testing.scm
  - [x] mapping
  - [x] pack: support nested list
  - [x] multimap
  - [x] counter, requires mapping and thread-index
  - [x] crawl scheme world
  - [ ] full-text search
    - [ ] index
      - [x] replace anything that is not alphanumeric with a space, and
            filter out words strictly smaller than 2 or strictly bigger
            than 64,
      - [x] store each stem once in the index,
      - [x] every known stem is associated with a count, and sum to be
            able to compute tf-idf,
      - [x] every known word is associated with a count, and sum to be
            able to compute tf-idf,
      - [x] every stem is associated with the ulid.
    - [ ] query
      - [ ] parse query: KEY WORD -MINUS,
      - [ ] validate that query is not only negation,
      - [ ] seed with most discriminant stem,
      - [ ] in parallel, compute score against bag of word
            (term-frequency inverse-document-frequency)
      - [ ] keep top 30 results (no pagination),
- [ ] in the nstore, save ulid, url, title, and preview
- [ ] logging
- [ ] guix package definition


Future documentation:

- type your search query in the input box
- type an url to index it:
  - if the url has a path, it will only index the given url page, it
    will not follow redirections,
  - if the url has no path, it will index the domain.

## TODO

- [ ] move to R7RS
- [ ] wet/wat/warc file crawler
- [ ] wet/wat/warc file consumer
- [ ] search pad
- [ ] spell checking
- [ ] more-like-this
- [ ] sensimark
- [ ] federation
- [ ] nstore's prefix: make it a bytevecor
- [ ] okvs memory backend, requires r7rs or standalone redblack-tree
- [ ] okvs foundationdb backend
- [ ] okvs sqlite backend
- [ ] rankedset
- [ ] okvs/pack: optimize algorithm of nested list to rely on a single
      pass
- [ ] okvs/nstore: improve prefix handling.
