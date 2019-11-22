# guile-babelia

Wanna be search engine with federation support

[![babel tower beamed by an alien
spaceship](https://cdn.dribbble.com/users/2441249/screenshots/4890251/babeldrbl.jpg)](https://dribbble.com/shots/4890251-Babel)

(artwork by [mildtravis](https://dribbble.com/mildtravis))

## Dependencies

- guile 2.9.4
- guile-fibers 1.0.0
- guile-bytestructures 1.0.6
- guile-gcrypt 0.2.0
- wiredtiger 3.0.0
- stemmer, see [my guix channel](https://git.sr.ht/~amz3/guix-amz3-channel).

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
- [x] okvs abstractions
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
  - [x] full-text search
    - [x] index
      - [x] replace anything that is not alphanumeric with a space, and
            filter out words strictly smaller than 2 or strictly bigger
            than 64,
      - [x] store each stem once in the index,
      - [x] every known stem is associated with a count, and sum to be
            able to compute tf-idf,
      - [x] every known word is associated with a count, and sum to be
            able to compute tf-idf,
      - [x] every stem is associated with the ulid.
    - [x] query
      - [x] parse query: KEY WORD -MINUS,
      - [x] validate that query is not only negation,
      - [x] seed with most discriminant stem,
      - [x] in parallel, compute score against bag of word
      - [x] keep top 30 results (configurable)
 - [x] add `babelia index PATH` command to index html files
 - [x] add `babelia search KEY WORD -MINUS` to search them

## v0.2.0

- [ ] move to R7RS https://git.sr.ht/~amz3/guile-arew:
  - [ ] scheme bitwise
  - [ ] scheme bytevector
  - [ ] scheme generator
  - [ ] scheme hash-table
  - [ ] scheme list
  - [ ] scheme mapping
  - [ ] scheme mapping hash
  - [ ] scheme set
  - [ ] guix: guile-build-system
- [ ] improve query support
- [ ] add stop words
- [ ] add recent queries cache
- [ ] add frequent queries cache
- [ ] optim: single term queries where stem(term) = term?
- [ ] babelia api secret show: add it
- [ ] babelia api secret generate: add it
- [ ] one crawler per domain as a separate process:
  - [ ] BABELIA_SECRET environment variable,
  - [ ] robots.txt-parse => robots.txt,
  - [ ] robots.txt-delay => #t, #f or seconds,
  - [ ] keep track of what is done and what is todo,
- [ ] preview: in the nstore, save ulid, url, title,
- [ ] guix package definition for dependencies,
- [ ] benchmark with scheme world dump, and commit the resulting,
- [ ] need to split the number of cores between wiredtiger and the
      app. Make thread-pool size configureable,
- [ ] upgrade wiredtiger,
- [ ] okvs fts: add td-idf scoring without cache,
- [ ] okvs fts: maybe-index and reindex (delete + add)
- [ ] logging
- [ ] federation
- [ ] okvs foundationdb backend
- [ ] okvs memory backend, requires r7rs or standalone redblack-tree
- [ ] okvs nstore: improve prefix handling.
- [ ] okvs pack: optimize algorithm of nested list with a single pass
- [ ] okvs sqlite backend
- [ ] rankedset
- [ ] search pad
- [ ] sensimark
- [ ] spell checking
- [ ] babelia index: warc file input
- [ ] babelia crawler: output warc file
