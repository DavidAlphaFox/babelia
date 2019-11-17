# guile-babelia

Wanna be search engine with federation support

[![babel tower beamed by an alien spaceship](https://cdn.dribbble.com/users/2441249/screenshots/4890251/babeldrbl.jpg)](https://dribbble.com/shots/4890251-Babel)

(artwork by [mildtravis](https://dribbble.com/mildtravis))

## Dependencies

- guile 2.2 (but tested with 2.9.4)
- guile-fibers 1.0.0
- guile-gcrypt 0.2.0
- wiredtiger 3.0.0
- snowball stemmer

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
  - [ ] move okvs abstractions inside okvs directory (fts, counter,
        nstore, ulid...)
  - [ ] memory backend, requires r7rs (or redblack-tree)
  - [ ] foundationdb backend
  - [ ] ulid store, rename object.scm to ustore.scm
  - [ ] map
  - [ ] multimap
  - [ ] counter, requires map and thread-index
  - [ ] rankedset
  - [ ] full-text search
    - [ ] index
      - [x] replace anything that is not alphanumeric with a space, and
            filter out words strictly smaller than 2 or strictly bigger
            than 256,
      - [x] store each stem once in the index,

      - [x] every known stem is associated with a count, and sum to be
            able to compute tf-idf,
      - [x] every known word is associated with a count, and sum to be
            able to compute tf-idf.
    - [ ] query
      - [ ] parse query: KEY WORD -MINUS,
      - [ ] validate that query is not only negation,
      - [ ] seed with most discriminant stem,
      - [ ] in parallel, compute score and cache,
      - [ ] keep top 30 results (no pagination),
      - [ ] count queries (analytics that will later help drive the
            crawler?),
      - [ ] query results (LRU?) cache.
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
