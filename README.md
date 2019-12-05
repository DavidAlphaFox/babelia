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
- guile-gnutls 3.6.9
- guile-arew
- wiredtiger 3.2.0
- stemmer 0.0.0

See [my guix channel](https://git.sr.ht/~amz3/guix-amz3-channel).

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

- [x] logging library with colored output
- [x] okvs/fts: consider all keywords
- [x] okvs/wiredtiger: move the lock to the record
- [x] parse query into a closure
- [x] babelia words counter: sorted by count
  - [x] counter-fold
- [x] babelia stem counter: sorted by count
- [x] babelia stem stop update FILENAME: input text file with stop
      word that must be ignored as seed candidates.
  - [x] mapping-clear via okvs-range-remove
- [x] babelia stem stop guess DIRECTORY SECONDS: benchmark using a
      fresh database connection each stem from frequent to infrequent
      until it takes less than SECONDS to query. Output the stems that
      are slow.
- [x] reject queries which seed is a stop word
- [x] babelia web api secret generate --force: create file with the
      hex string of the secret.
- [x] okvs abstraction: record store (rstore)
- [x] web: guard all exception and return 500,
- [x] okvs fts fts-index:
  - [x] input: html (with possibly microformats)
  - [x] output: three values: uid, title, and preview
  - [x] can raise babelia/index error with a reason.
  - [x] title: min 3, max 100 truncated
  - [x] text: min 280 chars, max ???
  - [x] create small preview: max 280 chars
- [x] babelia web /api/index
- [x] babelia web /api/search
- [ ] crawler:
  - [x] (make-robots.txt user-agent string)
  - [x] (robots.txt-delay robots.txt path) => #f or seconds,
  - [x] (robots.txt-allow? robots.txt path) => #f or #t,
  - [x] use nstore in separate directory
  - [x] babelia crawler run: same command but another processus.
  - [x] fiber main thread + workers
  - [x] babelia crawler add REMOTE URL:
    - [x] if has a path, if it is html and utf8 and not a redirection,
          index only the given URL
    - [x] otherwise, it is a domain:
      - [x] check that it is not a redirection,
      - [x] check that it is html and utf8,
      - [x] add linked pages to todo,
      - [x] index the given page,
      - [x] extract links and add to todo with domain,
  - [x] keep track of what is done and what is todo,
  - [x] add to the todo only if is html and utf8,
- [x] web: input query
- [x] web: display results

## v0.3.0

- [ ] move to R7RS https://git.sr.ht/~amz3/guile-arew
  - [x] scheme bitwise
  - [x] scheme bytevector
  - [x] scheme comparator
  - [x] scheme generator
  - [x] scheme hash-table
  - [x] scheme list
  - [x] scheme mapping
  - [ ] scheme mapping hash
  - [x] scheme set
  - [x] guix: guile-build-system
- [ ] normalize query: remove useless whitespace to play nice with the cache
- [ ] log queries that take more that 5 seconds (configurable),
- [ ] babelia queries show: output slow queries,
- [ ] babelia cache update FILENAME
- [ ] babelia cache refresh
- [ ] babelia web api secret generate: encrypt the secret
- [ ] babelia web api secret show
- [ ] index: support structured documents
- [ ] guix package definition for dependencies,
- [ ] benchmark with scheme world dump, and commit the resulting,
- [ ] need to split the number of cores between wiredtiger and the
      app.
- [ ] Make thread-pool size configureable,
- [ ] okvs fts: maybe-index and reindex (delete + add)
- [ ] federation
- [ ] search pad
- [ ] babelia api secret show: add it
- [ ] babelia api secret generate: add it
- [ ] okvs/fts:
  - [ ] OR support
  - [ ] proximity bonus
  - [ ] keyword weight
  - [ ] one way synonyms
  - [ ] two way synonyms
  - [ ] phrase matching
  - [ ] td-idf
- [ ] babelia crawler sitemap support
- [ ] babelia crawler wikimedia: use rest api, otherwise fallback to
      wiki.

## TODO

- [ ] okvs nstore: improve prefix handling.
- [ ] spell checking
- [ ] sensimark
- [ ] okvs pack: optimize algorithm of nested list with a single pass
- [ ] okvs pack: past argument as a list instead of rest
- [ ] babelia index: warc file input
- [ ] babelia crawler: output warc file
- [ ] check.scm: make it possible to execute tests from low level to
      high level (or high level to low level)
- [ ] more validation, use R7RS raise and guard (to make the
      transaction fail),
- [ ] entity recognition,
- [ ] inbound links,
- [ ] domain or page outbound links,
- [ ] page rank.
- [ ] gumbo bindings https://github.com/google/gumbo-parser
