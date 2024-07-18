Similar to `Dict/Set` except its variants are exposed for you to safely mess with.

```elm
import LookupSafe exposing (LookupSafe)
import StructuredId exposing (StructuredId)

type Job
    = Job String

queue : LookupSafe Job
queue =
    LookupSafe.fromListMap
        (\( prio, job ) -> { key = prio |> StructuredId.ofInt, value = job })
        [ ( 3, Job "Shave the yak" )
        , ( 5, Job "Reticulate splines" )
        , ( 1, Job "Feed the gremlins" )
        ]
```
