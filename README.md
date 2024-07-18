Similar to `Dict`/`Set` except its variants are exposed for you to mess with
without running into an invalid state.
As a cherry on top you also get a safe non-empty type.

```elm
import DictSafe exposing (DictSafe)
import Bits

type Job
    = Job String

queue : DictSafe Job
queue =
    DictSafe.fromListMap
        (\( prio, job ) -> { key = prio |> Bits.fromIntUnsigned 32, value = job })
        [ ( 3, Job "Shave the yak" )
        , ( 5, Job "Reticulate splines" )
        , ( 1, Job "Feed the gremlins" )
        ]

queue |> DictSafe.minimum |> .value
--> Job "Feed the gremlins" (no maybe)
```
  - 🧩 `Bits` is from [`elm-bits`](https://dark.elm.dmy.fr/packages/lue-bird/elm-bits/latest/)

## be aware

Pragmatically speaking, the core `Dict` should be faster than `DictSafe`.
So the only use cases I can roughly see for `DictSafe` are
  - algorithms that take a binary trie as input (e.g. binary parsers that parse an enum)
  - if you need certain functionality like a non-empty dict or a map2/justsMap2 and are unhappy with the alternatives out there
  - very specific situations where `DictSafe` could be faster (verify this for your specific codebase!)
    - you already have `List Bit` or a wrapper as your lookup key and conversions to e.g. `List Int` as a comparable for `Dict` would be more costly
    - you only use very few bits to identify values (e.g. for small indexes)
    - you need fast access (log n compares would be too slow)

But honestly, this package is mostly a mental exercise to prove this is possible in theory
and to explore performance in comparison to `Dict` for large sizes.
