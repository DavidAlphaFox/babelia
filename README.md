# guile-babelia

Wanna be search engine with federation support

[![babel tower beamed by an alien spaceship](https://cdn.dribbble.com/users/2441249/screenshots/4890251/babeldrbl.jpg)](https://dribbble.com/shots/4890251-Babel)

(artwork by [mildtravis](https://dribbble.com/mildtravis))

## Dependencies

- guile 2.2 (but tested with 2.9.4)
- guile-fibers 1.0.0
- guile-gcrypt 0.2.0
- wiredtiger 3.0.0

## TODO

- [x] wiredtiger bindings
- [ ] foundationdb bindings
- [x] ~~srfi-128 (comparators)~~, not required since `(mapping hash)`
      was replaced with `fash`
- [x] ~~srfi-146 (mappings hash)~~, use
      [fash](https://www.wingolog.org/pub/fash.scm)
- [x] srfi-158 (generators)
- [x] srfi-167 (okvs)
  - [x] `pack` and `unpack`
  - [x] `<engine>` type class object
  - [x] wiredtiger backend
  - [ ] memory backend
  - [ ] foundationdb backend
- [x] srfi-168 (nstore)
- [x] [ulid](https://github.com/ulid/spec)
- [x] object store
- [ ] web server
- [ ] thread pool
- [ ] index
- [ ] query
- [ ] wet/wat/warc file crawler
- [ ] wet/wat/warc file consumer
- [ ] search pad
- [ ] spell checking
- [ ] more-like-this
- [ ] sensimark
- [ ] federation
