# guile-babelia

Wanna be search engine with federation support

[![babel tower beamed by an alien spaceship](https://cdn.dribbble.com/users/2441249/screenshots/4890251/babeldrbl.jpg)](https://dribbble.com/shots/4890251-Babel)

(artwork by [mildtravis](https://dribbble.com/mildtravis))

## TODO

- [ ] wiredtiger bindings
- [ ] foundationdb bindings
- [ ] ~~srfi-128 (comparators)~~, not required since `(mapping hash)`
      was replaced with `fash`
- [ ] ~~srfi-146 (mappings hash)~~, instead use
      [fash](https://www.wingolog.org/pub/fash.scm)
- [ ] srfi-158 (generators)
- [ ] srfi-167 (okvs)
- [ ] srfi-168 (nstore)
- [ ] ~~srfi-173 (hooks)~~, instead use guile's hooks
- [ ] sculid, similar to [ulid](https://github.com/ulid/spec) with a
      16bits to represent thread index that will avoid identifier
      collisions and increase concurrency
- [ ] object store
- [ ] map-reduce
- [ ] wet/wat/warc file crawler
- [ ] wet/wat/warc file consumer
- [ ] sensimark
- [ ] federation
