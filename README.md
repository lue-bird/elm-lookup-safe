Similar to `Dict/Set` except its variants are exposed for you to safely mess with.

```elm
import DictSafe exposing (DictSafe)
import StructuredId exposing (StructuredId)

type Priority
    = Priority Int

type Job
    = Job String

queue : DictSafe Job
queue =
    DictSafe.fromListMap
        (\( prio, job ) -> { key = prio |> priorityToStructuredId, value = job })
        [ ( Priority 3, Job "Shave the yak" )
        , ( Priority 5, Job "Reticulate splines" )
        , ( Priority 1, Job "Feed the gremlins" )
        ]

priorityToStructuredId : Priority -> StructuredId
priorityToStructuredId (Priority prioInt) =
    StructuredId.ofInt prioInt
```
