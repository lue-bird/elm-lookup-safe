module DictSafe exposing
    ( DictSafe, Filled(..)
    , empty, one, fromListMap
    , valueAtKey, minimum, maximum
    , set, alterAtKey, map, justsMap
    , toList, foldOnto
    , map2, justsMap2, fold2Onto
    )

{-|

@docs DictSafe, Filled


## create

@docs empty, one, fromListMap


## observe

@docs valueAtKey, minimum, maximum

You can check for emptiness with for example `== DictSafe.empty`


## alter

@docs set, alterAtKey, map, justsMap


## transform

@docs toList, foldOnto


## combine

@docs map2, justsMap2, fold2Onto

-}

import AndOr
import Bit exposing (Bit)
import Or


{-| A lookup that maps unique keys to values.

Note: calling `==` on 2 of these will work correctly (and be roughly `O(size)`).

Internally, it's a binary trie.
The cool thing about that is that there are no "invariants"
aka states that the type allows but that fail assumptions like order or balance.

-}
type alias DictSafe value =
    Maybe (Filled value)


{-| [`DictSafe`](#DictSafe) with ≥ 1 entries.
-}
type Filled value
    = Next0 (Filled value)
    | Next1 (Filled value)
    | Value value
    | Next0AndNext1
        { next0 : Filled value
        , next1 : Filled value
        }
    | ValueAndNext0
        { value : value
        , next0 : Filled value
        }
    | ValueAndNext1
        { value : value
        , next1 : Filled value
        }
    | ValueAndNext0AndNext1
        { value : value
        , next0 : Filled value
        , next1 : Filled value
        }


{-| Everything starts somewhere. Start filling it up using [`set`](#set)
-}
empty : DictSafe value_
empty =
    Nothing


{-| Convert from a `List` by converting each element to key-value entries.

`O(length * key info density)` (roughly `O(length * log(length))`)

-}
fromListMap :
    (element -> { key : List Bit, value : value })
    -> (List element -> DictSafe value)
fromListMap elementToEntry list =
    list
        |> List.foldl
            (\element soFar ->
                soFar |> set (elementToEntry element)
            )
            empty


{-| Create or overwrite the value associated with a given key.

`O(key info density)` (roughly `O(log(size))`)

-}
set :
    { key : List Bit, value : value }
    -> (DictSafe value -> DictSafe value)
set entry dictSafe =
    dictSafe
        |> alterAtKey entry.key
            (\_ -> entry.value |> Just)


filledAlterAtKey :
    List Bit
    -> (Maybe value -> Maybe value)
    -> (Filled value -> DictSafe value)
filledAlterAtKey key maybeValueChange dictSafeFilled =
    case key of
        [] ->
            case dictSafeFilled of
                Next0 next0 ->
                    case maybeValueChange Nothing of
                        Nothing ->
                            Next0 next0 |> Just

                        Just newValueHere ->
                            ValueAndNext0 { value = newValueHere, next0 = next0 }
                                |> Just

                Next1 next1 ->
                    case maybeValueChange Nothing of
                        Nothing ->
                            Next1 next1 |> Just

                        Just newValueHere ->
                            ValueAndNext1 { value = newValueHere, next1 = next1 }
                                |> Just

                Next0AndNext1 next01 ->
                    case maybeValueChange Nothing of
                        Nothing ->
                            Next0AndNext1 next01 |> Just

                        Just newValueHere ->
                            ValueAndNext0AndNext1
                                { value = newValueHere
                                , next0 = next01.next0
                                , next1 = next01.next1
                                }
                                |> Just

                Value value ->
                    case value |> Just |> maybeValueChange of
                        Nothing ->
                            Nothing

                        Just newValueHere ->
                            Value newValueHere |> Just

                ValueAndNext0 valueNext0 ->
                    case valueNext0.value |> Just |> maybeValueChange of
                        Nothing ->
                            Next0 valueNext0.next0 |> Just

                        Just changedValue ->
                            ValueAndNext0 { value = changedValue, next0 = valueNext0.next0 }
                                |> Just

                ValueAndNext1 valueNext1 ->
                    case valueNext1.value |> Just |> maybeValueChange of
                        Nothing ->
                            Next1 valueNext1.next1 |> Just

                        Just changedValue ->
                            ValueAndNext1 { value = changedValue, next1 = valueNext1.next1 }
                                |> Just

                ValueAndNext0AndNext1 valueNext01 ->
                    case valueNext01.value |> Just |> maybeValueChange of
                        Nothing ->
                            Next0AndNext1 { next0 = valueNext01.next0, next1 = valueNext01.next1 }
                                |> Just

                        Just changedValue ->
                            ValueAndNext1 { value = changedValue, next1 = valueNext01.next1 }
                                |> Just

        firstBit :: secondBitUp ->
            case dictSafeFilled of
                Next0 next0 ->
                    case firstBit of
                        Bit.O ->
                            case filledAlterAtKey secondBitUp maybeValueChange next0 of
                                Nothing ->
                                    Nothing

                                Just alteredNext0 ->
                                    Next0 alteredNext0 |> Just

                        Bit.I ->
                            case maybeValueChange Nothing of
                                Nothing ->
                                    Next0 next0 |> Just

                                Just newValue ->
                                    Next0AndNext1 { next0 = next0, next1 = one secondBitUp newValue }
                                        |> Just

                Next1 next1 ->
                    case firstBit of
                        Bit.O ->
                            case maybeValueChange Nothing of
                                Nothing ->
                                    Next1 next1 |> Just

                                Just newValue ->
                                    Next0AndNext1 { next0 = one secondBitUp newValue, next1 = next1 }
                                        |> Just

                        Bit.I ->
                            case filledAlterAtKey secondBitUp maybeValueChange next1 of
                                Nothing ->
                                    Nothing

                                Just altered1 ->
                                    Next1 altered1 |> Just

                Next0AndNext1 next01 ->
                    case firstBit of
                        Bit.O ->
                            case filledAlterAtKey secondBitUp maybeValueChange next01.next0 of
                                Nothing ->
                                    Next1 next01.next1 |> Just

                                Just alteredNext0 ->
                                    Next0AndNext1 { next0 = alteredNext0, next1 = next01.next1 }
                                        |> Just

                        Bit.I ->
                            case filledAlterAtKey secondBitUp maybeValueChange next01.next1 of
                                Nothing ->
                                    Next0 next01.next0 |> Just

                                Just alteredNext1 ->
                                    Next0AndNext1 { next0 = next01.next0, next1 = alteredNext1 }
                                        |> Just

                Value value ->
                    case firstBit of
                        Bit.O ->
                            case maybeValueChange Nothing of
                                Nothing ->
                                    Value value |> Just

                                Just newNext0Value ->
                                    ValueAndNext0 { value = value, next0 = one secondBitUp newNext0Value }
                                        |> Just

                        Bit.I ->
                            case maybeValueChange Nothing of
                                Nothing ->
                                    Value value |> Just

                                Just newNext0Value ->
                                    ValueAndNext1 { value = value, next1 = one secondBitUp newNext0Value }
                                        |> Just

                ValueAndNext0 valueNext0 ->
                    case firstBit of
                        Bit.O ->
                            case filledAlterAtKey secondBitUp maybeValueChange valueNext0.next0 of
                                Nothing ->
                                    Value valueNext0.value |> Just

                                Just alteredNext0 ->
                                    ValueAndNext0 { value = valueNext0.value, next0 = alteredNext0 }
                                        |> Just

                        Bit.I ->
                            case maybeValueChange Nothing of
                                Nothing ->
                                    ValueAndNext0 valueNext0 |> Just

                                Just newValue ->
                                    ValueAndNext0AndNext1
                                        { value = valueNext0.value
                                        , next0 = valueNext0.next0
                                        , next1 = one secondBitUp newValue
                                        }
                                        |> Just

                ValueAndNext1 valueNext1 ->
                    case firstBit of
                        Bit.O ->
                            case maybeValueChange Nothing of
                                Nothing ->
                                    ValueAndNext1 valueNext1 |> Just

                                Just newValue ->
                                    ValueAndNext0AndNext1
                                        { value = valueNext1.value
                                        , next0 = one secondBitUp newValue
                                        , next1 = valueNext1.next1
                                        }
                                        |> Just

                        Bit.I ->
                            case filledAlterAtKey secondBitUp maybeValueChange valueNext1.next1 of
                                Nothing ->
                                    Value valueNext1.value |> Just

                                Just altered1 ->
                                    ValueAndNext1 { value = valueNext1.value, next1 = altered1 } |> Just

                ValueAndNext0AndNext1 valueNext01 ->
                    case firstBit of
                        Bit.O ->
                            case filledAlterAtKey secondBitUp maybeValueChange valueNext01.next0 of
                                Nothing ->
                                    ValueAndNext1 { value = valueNext01.value, next1 = valueNext01.next1 }
                                        |> Just

                                Just alteredNext0 ->
                                    ValueAndNext0AndNext1
                                        { value = valueNext01.value
                                        , next0 = alteredNext0
                                        , next1 = valueNext01.next1
                                        }
                                        |> Just

                        Bit.I ->
                            case filledAlterAtKey secondBitUp maybeValueChange valueNext01.next1 of
                                Nothing ->
                                    ValueAndNext0 { value = valueNext01.value, next0 = valueNext01.next0 }
                                        |> Just

                                Just alteredNext1 ->
                                    ValueAndNext0AndNext1
                                        { value = valueNext01.value
                                        , next0 = valueNext01.next0
                                        , next1 = alteredNext1
                                        }
                                        |> Just


{-| Change the contained value at a given key.
`Nothing` if the key wasn't associated with a value previously, `Just` if it was.

The given change function should return `Just` to set a value at the key and `Nothing` to remove it.

`O(key info density)` (roughly `O(log(size))`)

-}
alterAtKey :
    List Bit
    -> (Maybe value -> Maybe value)
    -> (DictSafe value -> DictSafe value)
alterAtKey key maybeValueChange maybeDictSafe =
    case maybeDictSafe of
        Nothing ->
            case maybeValueChange Nothing of
                Nothing ->
                    Nothing

                Just newValueHere ->
                    one key newValueHere |> Just

        Just dictSafeFilled ->
            dictSafeFilled |> filledAlterAtKey key maybeValueChange


{-| `O(key info density)`
-}
one : List Bit -> value -> Filled value
one entryKey entryValue =
    case entryKey of
        [] ->
            Value entryValue

        firstBit :: secondBitUp ->
            case firstBit of
                Bit.O ->
                    Next0 (one secondBitUp entryValue)

                Bit.I ->
                    Next1 (one secondBitUp entryValue)


filledValueAtKeyBits : List Bit -> Filled value -> Maybe value
filledValueAtKeyBits key dictSafeFilled =
    case key of
        [] ->
            case dictSafeFilled of
                Next0 next0 ->
                    Nothing

                Next1 next1 ->
                    Nothing

                Value value ->
                    Just value

                Next0AndNext1 next01 ->
                    Nothing

                ValueAndNext0 valueNext0 ->
                    Just valueNext0.value

                ValueAndNext1 valueNext1 ->
                    Just valueNext1.value

                ValueAndNext0AndNext1 valueNext01 ->
                    Just valueNext01.value

        firstBit :: secondBitUp ->
            case dictSafeFilled of
                Next0 next0 ->
                    case firstBit of
                        Bit.O ->
                            filledValueAtKeyBits secondBitUp next0

                        Bit.I ->
                            Nothing

                Next1 next1 ->
                    case firstBit of
                        Bit.O ->
                            Nothing

                        Bit.I ->
                            filledValueAtKeyBits secondBitUp next1

                Value _ ->
                    Nothing

                Next0AndNext1 next01 ->
                    case firstBit of
                        Bit.O ->
                            filledValueAtKeyBits secondBitUp next01.next0

                        Bit.I ->
                            filledValueAtKeyBits secondBitUp next01.next1

                ValueAndNext0 valueNext0 ->
                    case firstBit of
                        Bit.O ->
                            filledValueAtKeyBits secondBitUp valueNext0.next0

                        Bit.I ->
                            Nothing

                ValueAndNext1 valueNext1 ->
                    case firstBit of
                        Bit.O ->
                            Nothing

                        Bit.I ->
                            filledValueAtKeyBits secondBitUp valueNext1.next1

                ValueAndNext0AndNext1 valueNext01 ->
                    case firstBit of
                        Bit.O ->
                            filledValueAtKeyBits secondBitUp valueNext01.next0

                        Bit.I ->
                            filledValueAtKeyBits secondBitUp valueNext01.next1


{-| `Just` the value associated with a given key, or `Nothing` if it's not a member.
-}
valueAtKey : List Bit -> DictSafe value -> Maybe value
valueAtKey key dictSafe =
    case dictSafe of
        Nothing ->
            Nothing

        Just dictSafeFilled ->
            filledValueAtKeyBits key dictSafeFilled


{-| Convert its values to a `List`.

`O(size)`

Implementation detail: The resulting elements are ordered from highest to lowest key

-}
toList : DictSafe value -> List value
toList dictSafe =
    dictSafe |> toListMap Basics.identity


{-| Convert its values to a `List`, transformed in a given way.

`O(size)`

-}
toListMap : (value -> element) -> (DictSafe value -> List element)
toListMap valueToElement dictSafe =
    dictSafe |> foldOnto [] (\value soFar -> (value |> valueToElement) :: soFar)


{-| Change each value.

`O(size)`

-}
map : (value -> valueChanged) -> (DictSafe value -> DictSafe valueChanged)
map valueChange dictSafe =
    dictSafe |> Maybe.map (\dictSafeFilled -> dictSafeFilled |> filledMap valueChange)


filledMap : (value -> valueChanged) -> (Filled value -> Filled valueChanged)
filledMap valueChange dictSafeFilled =
    case dictSafeFilled of
        Next0 next0 ->
            Next0 (next0 |> filledMap valueChange)

        Next1 next1 ->
            Next1 (next1 |> filledMap valueChange)

        Value value ->
            Value (value |> valueChange)

        Next0AndNext1 next01 ->
            Next0AndNext1
                { next0 = next01.next0 |> filledMap valueChange
                , next1 = next01.next1 |> filledMap valueChange
                }

        ValueAndNext0 valueNext0 ->
            ValueAndNext0
                { value = valueNext0.value |> valueChange
                , next0 = valueNext0.next0 |> filledMap valueChange
                }

        ValueAndNext1 valueNext1 ->
            ValueAndNext1
                { value = valueNext1.value |> valueChange
                , next1 = valueNext1.next1 |> filledMap valueChange
                }

        ValueAndNext0AndNext1 valueNext01 ->
            ValueAndNext0AndNext1
                { value = valueNext01.value |> valueChange
                , next0 = valueNext01.next0 |> filledMap valueChange
                , next1 = valueNext01.next1 |> filledMap valueChange
                }


{-| At each entry, either return `Just` a changed value or `Nothing` to delete.

`O(size)`

-}
justsMap : (value -> Maybe valueChanged) -> (DictSafe value -> DictSafe valueChanged)
justsMap valueChange dictSafe =
    dictSafe |> Maybe.andThen (\dictSafeFilled -> dictSafeFilled |> filledJustsMap valueChange)


filledJustsMap : (value -> Maybe valueChanged) -> (Filled value -> DictSafe valueChanged)
filledJustsMap valueToMaybe dictSafeFilled =
    case dictSafeFilled of
        Next0 next0 ->
            Maybe.map Next0 (next0 |> filledJustsMap valueToMaybe)

        Next1 next1 ->
            Maybe.map Next1 (next1 |> filledJustsMap valueToMaybe)

        Value value ->
            Maybe.map Value (value |> valueToMaybe)

        Next0AndNext1 next01 ->
            maybeNext0AndMaybeNext1
                { next0 = next01.next0 |> filledJustsMap valueToMaybe
                , next1 = next01.next1 |> filledJustsMap valueToMaybe
                }

        ValueAndNext0 valueNext0 ->
            maybeValueAndMaybeNext0
                { value = valueNext0.value |> valueToMaybe
                , next0 = valueNext0.next0 |> filledJustsMap valueToMaybe
                }

        ValueAndNext1 valueNext1 ->
            maybeValueAndMaybeNext1
                { value = valueNext1.value |> valueToMaybe
                , next1 = valueNext1.next1 |> filledJustsMap valueToMaybe
                }

        ValueAndNext0AndNext1 valueNext01 ->
            maybeValueAndMaybeNext0AndMaybeNext1
                { value = valueNext01.value |> valueToMaybe
                , next0 = valueNext01.next0 |> filledJustsMap valueToMaybe
                , next1 = valueNext01.next1 |> filledJustsMap valueToMaybe
                }


minimum : DictSafe value -> Maybe { key : List Bit, value : value }
minimum =
    \dictSafe ->
        dictSafe |> Maybe.map filledMinimum


filledMinimum : Filled value -> { key : List Bit, value : value }
filledMinimum =
    \dictSafeFilled ->
        dictSafeFilled |> filledMinimumWithinPath []


filledMinimumWithinPath : List Bit -> Filled value -> { key : List Bit, value : value }
filledMinimumWithinPath path =
    \dictSafeFilled ->
        case dictSafeFilled of
            Next0 next0 ->
                next0 |> filledMinimumWithinPath (Bit.O :: path)

            Next1 next1 ->
                next1 |> filledMinimumWithinPath (Bit.I :: path)

            Value value ->
                { key = path |> List.reverse, value = value }

            Next0AndNext1 next01 ->
                next01.next0 |> filledMinimumWithinPath (Bit.O :: path)

            ValueAndNext0 valueNext0 ->
                valueNext0.next0 |> filledMinimumWithinPath (Bit.O :: path)

            ValueAndNext1 valueNext1 ->
                { key = path |> List.reverse, value = valueNext1.value }

            ValueAndNext0AndNext1 valueNext01 ->
                valueNext01.next0 |> filledMinimumWithinPath (Bit.O :: path)


maximum : DictSafe value -> Maybe { key : List Bit, value : value }
maximum =
    \dictSafe ->
        dictSafe |> Maybe.map filledMaximum


filledMaximum : Filled value -> { key : List Bit, value : value }
filledMaximum =
    \dictSafeFilled ->
        dictSafeFilled |> filledMaximumWithinPath []


filledMaximumWithinPath : List Bit -> Filled value -> { key : List Bit, value : value }
filledMaximumWithinPath path =
    \dictSafeFilled ->
        case dictSafeFilled of
            Next0 next0 ->
                next0 |> filledMaximumWithinPath (Bit.O :: path)

            Next1 next1 ->
                next1 |> filledMaximumWithinPath (Bit.I :: path)

            Value value ->
                { key = path |> List.reverse, value = value }

            Next0AndNext1 next01 ->
                next01.next1 |> filledMaximumWithinPath (Bit.I :: path)

            ValueAndNext0 valueNext0 ->
                { key = path |> List.reverse, value = valueNext0.value }

            ValueAndNext1 valueNext1 ->
                valueNext1.next1 |> filledMaximumWithinPath (Bit.I :: path)

            ValueAndNext0AndNext1 valueNext01 ->
                valueNext01.next1 |> filledMaximumWithinPath (Bit.I :: path)


maybeFoldOnto :
    folded
    -> (value -> (folded -> folded))
    -> (Maybe (DictSafe value) -> folded)
maybeFoldOnto soFar reduceInValue maybeDictSafe =
    case maybeDictSafe of
        Nothing ->
            soFar

        Just dictSafe ->
            dictSafe |> foldOnto soFar reduceInValue


{-| Condense all values by reducing value after value into a given initial thing.

`O(size)`

Implementation detail: Traverses from highest to lowest key.

-}
foldOnto :
    folded
    -> (value -> (folded -> folded))
    -> (DictSafe value -> folded)
foldOnto initialFolded reduceInValue dictSafe =
    case dictSafe of
        Nothing ->
            initialFolded

        Just dictSafeFilled ->
            dictSafeFilled |> filledFoldOnto initialFolded reduceInValue


filledFoldOnto :
    folded
    -> (value -> (folded -> folded))
    -> (Filled value -> folded)
filledFoldOnto initialFolded reduceInValue dictSafeFilled =
    case dictSafeFilled of
        Next0 next0 ->
            next0 |> filledFoldOnto initialFolded reduceInValue

        Next1 next1 ->
            next1 |> filledFoldOnto initialFolded reduceInValue

        Value value ->
            initialFolded |> reduceInValue value

        Next0AndNext1 next01 ->
            next01.next0
                |> filledFoldOnto
                    (next01.next1 |> filledFoldOnto initialFolded reduceInValue)
                    reduceInValue

        ValueAndNext0 valueNext0 ->
            valueNext0.next0
                |> filledFoldOnto
                    (initialFolded |> reduceInValue valueNext0.value)
                    reduceInValue

        ValueAndNext1 valueNext1 ->
            valueNext1.next1
                |> filledFoldOnto initialFolded reduceInValue
                |> reduceInValue valueNext1.value

        ValueAndNext0AndNext1 valueNext01 ->
            valueNext01.next0
                |> filledFoldOnto
                    (valueNext01.next1
                        |> filledFoldOnto initialFolded reduceInValue
                        |> reduceInValue valueNext01.value
                    )
                    reduceInValue


{-| Combine the values of the 2 [`DictSafe`](#DictSafe)s depending on where keys are present in either or both
into a new value or `Nothing`,
see [`AndOr`](https://dark.elm.dmy.fr/packages/lue-bird/elm-and-or/latest/).

    import AndOr
    import Or

    intersection : { a : DictSafe aValue, b : DictSafe bValue } -> DictSafe value
    intersection =
        DictSafe.justsMap2
            (\value2 ->
                case value2 of
                    AndOr.Both ( a, _ ) ->
                        a |> Just

                    AndOr.Only either ->
                        Nothing
            )

    symmetricalExclusion : { a : DictSafe aValue, b : DictSafe bValue } -> DictSafe value
    symmetricalExclusion =
        DictSafe.justsMap2
            (\value2 ->
                case value2 of
                    AndOr.Both _ ->
                        Nothing

                    AndOr.Only either ->
                        either |> Or.value |> Just
            )

If you need it even more flexible, use [`fold2Onto`](#fold2Onto),
if you always return `Just` in the given function, switch to [`map2`](#map2)

roughly `O(max size)`

-}
justsMap2 :
    (AndOr.AndOr aValue bValue -> Maybe value)
    ->
        ({ a : DictSafe aValue, b : DictSafe bValue }
         -> DictSafe value
        )
justsMap2 aAndOrBToValue dictSafes =
    case dictSafes.a of
        Nothing ->
            case dictSafes.b of
                Nothing ->
                    Nothing

                Just onlyB ->
                    onlyB
                        |> filledJustsMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)

        Just aDictSafeFilled ->
            case dictSafes.b of
                Nothing ->
                    aDictSafeFilled
                        |> filledJustsMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)

                Just bDictSafeFilled ->
                    { a = aDictSafeFilled, b = bDictSafeFilled } |> filledJustsMap2 aAndOrBToValue


maybeNext0AndMaybeNext1 : { next0 : DictSafe value, next1 : DictSafe value } -> DictSafe value
maybeNext0AndMaybeNext1 next01 =
    case next01.next0 of
        Nothing ->
            case next01.next1 of
                Nothing ->
                    Nothing

                Just next1 ->
                    Next1 next1 |> Just

        Just next0 ->
            case next01.next1 of
                Nothing ->
                    Next0 next0 |> Just

                Just next1 ->
                    Next0AndNext1 { next0 = next0, next1 = next0 } |> Just


maybeValueAndMaybeNext0 : { value : Maybe value, next0 : DictSafe value } -> DictSafe value
maybeValueAndMaybeNext0 valueNext0 =
    case valueNext0.value of
        Nothing ->
            case valueNext0.next0 of
                Nothing ->
                    Nothing

                Just next0 ->
                    Next0 next0 |> Just

        Just value ->
            case valueNext0.next0 of
                Nothing ->
                    Value value |> Just

                Just next0 ->
                    ValueAndNext0 { value = value, next0 = next0 }
                        |> Just


maybeValueAndMaybeNext1 : { value : Maybe value, next1 : DictSafe value } -> DictSafe value
maybeValueAndMaybeNext1 valueNext1 =
    case valueNext1.value of
        Nothing ->
            case valueNext1.next1 of
                Nothing ->
                    Nothing

                Just next1 ->
                    Next1 next1 |> Just

        Just value ->
            case valueNext1.next1 of
                Nothing ->
                    Value value |> Just

                Just next1 ->
                    ValueAndNext1 { value = value, next1 = next1 }
                        |> Just


maybeValueAndMaybeNext0AndMaybeNext1 :
    { value : Maybe value, next0 : DictSafe value, next1 : DictSafe value }
    -> DictSafe value
maybeValueAndMaybeNext0AndMaybeNext1 next01 =
    case next01.next0 of
        Nothing ->
            case next01.next1 of
                Nothing ->
                    case next01.value of
                        Nothing ->
                            Nothing

                        Just value ->
                            Value value |> Just

                Just next1 ->
                    case next01.value of
                        Nothing ->
                            Next1 next1 |> Just

                        Just value ->
                            ValueAndNext1 { value = value, next1 = next1 }
                                |> Just

        Just next0 ->
            case next01.next1 of
                Nothing ->
                    case next01.value of
                        Nothing ->
                            Next0 next0 |> Just

                        Just value ->
                            ValueAndNext0 { value = value, next0 = next0 }
                                |> Just

                Just next1 ->
                    case next01.value of
                        Nothing ->
                            Next0AndNext1 { next0 = next0, next1 = next1 }
                                |> Just

                        Just value ->
                            ValueAndNext0AndNext1
                                { value = value, next0 = next0, next1 = next1 }
                                |> Just


filledJustsMap2 :
    (AndOr.AndOr aValue bValue -> Maybe value)
    ->
        ({ a : Filled aValue, b : Filled bValue }
         -> DictSafe value
        )
filledJustsMap2 aAndOrBToValue dictSafes =
    case dictSafes.a of
        Next0 aNext0 ->
            case dictSafes.b of
                Next0 bNext0 ->
                    Maybe.map Next0 (filledJustsMap2 aAndOrBToValue { a = aNext0, b = bNext0 })

                Next1 bNext1 ->
                    maybeNext0AndMaybeNext1
                        { next0 = aNext0 |> filledJustsMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        , next1 = bNext1 |> filledJustsMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)
                        }

                Value bValue ->
                    maybeValueAndMaybeNext0
                        { next0 = aNext0 |> filledJustsMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        , value = AndOr.Only (Or.Second bValue) |> aAndOrBToValue
                        }

                Next0AndNext1 bNext01 ->
                    maybeNext0AndMaybeNext1
                        { next0 = { a = aNext0, b = bNext01.next0 } |> filledJustsMap2 aAndOrBToValue
                        , next1 = bNext01.next1 |> filledJustsMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)
                        }

                ValueAndNext0 bValueNext0 ->
                    maybeValueAndMaybeNext0
                        { value = AndOr.Only (Or.Second bValueNext0.value) |> aAndOrBToValue
                        , next0 = { a = aNext0, b = bValueNext0.next0 } |> filledJustsMap2 aAndOrBToValue
                        }

                ValueAndNext1 bValueNext1 ->
                    maybeValueAndMaybeNext0AndMaybeNext1
                        { value = AndOr.Only (Or.Second bValueNext1.value) |> aAndOrBToValue
                        , next0 = aNext0 |> filledJustsMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        , next1 = bValueNext1.next1 |> filledJustsMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)
                        }

                ValueAndNext0AndNext1 bValueNext01 ->
                    maybeValueAndMaybeNext0AndMaybeNext1
                        { value = AndOr.Only (Or.Second bValueNext01.value) |> aAndOrBToValue
                        , next0 = { a = aNext0, b = bValueNext01.next0 } |> filledJustsMap2 aAndOrBToValue
                        , next1 = bValueNext01.next1 |> filledJustsMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)
                        }

        Next1 aNext1 ->
            case dictSafes.b of
                Next0 bNext0 ->
                    maybeNext0AndMaybeNext1
                        { next0 = bNext0 |> filledJustsMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)
                        , next1 = aNext1 |> filledJustsMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        }

                Next1 bNext1 ->
                    Maybe.map Next1 (filledJustsMap2 aAndOrBToValue { a = aNext1, b = bNext1 })

                Value bValue ->
                    maybeValueAndMaybeNext1
                        { value = AndOr.Only (Or.Second bValue) |> aAndOrBToValue
                        , next1 = aNext1 |> filledJustsMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        }

                Next0AndNext1 bNext01 ->
                    maybeNext0AndMaybeNext1
                        { next0 = bNext01.next0 |> filledJustsMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)
                        , next1 = { a = aNext1, b = bNext01.next1 } |> filledJustsMap2 aAndOrBToValue
                        }

                ValueAndNext0 bValueNext0 ->
                    maybeValueAndMaybeNext0AndMaybeNext1
                        { value = AndOr.Only (Or.Second bValueNext0.value) |> aAndOrBToValue
                        , next0 = bValueNext0.next0 |> filledJustsMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)
                        , next1 = aNext1 |> filledJustsMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        }

                ValueAndNext1 bValueNext1 ->
                    maybeValueAndMaybeNext1
                        { value = AndOr.Only (Or.Second bValueNext1.value) |> aAndOrBToValue
                        , next1 = aNext1 |> filledJustsMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        }

                ValueAndNext0AndNext1 bValueNext01 ->
                    maybeValueAndMaybeNext0AndMaybeNext1
                        { value = AndOr.Only (Or.Second bValueNext01.value) |> aAndOrBToValue
                        , next0 = bValueNext01.next0 |> filledJustsMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)
                        , next1 = aNext1 |> filledJustsMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        }

        Value aValue ->
            case dictSafes.b of
                Next0 bNext0 ->
                    maybeValueAndMaybeNext0
                        { value = AndOr.Only (Or.First aValue) |> aAndOrBToValue
                        , next0 = bNext0 |> filledJustsMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)
                        }

                Next1 bNext1 ->
                    maybeValueAndMaybeNext1
                        { value = AndOr.Only (Or.First aValue) |> aAndOrBToValue
                        , next1 = bNext1 |> filledJustsMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)
                        }

                Value bValue ->
                    Maybe.map Value (AndOr.Both ( aValue, bValue ) |> aAndOrBToValue)

                Next0AndNext1 bNext01 ->
                    maybeValueAndMaybeNext0AndMaybeNext1
                        { value = AndOr.Only (Or.First aValue) |> aAndOrBToValue
                        , next0 = bNext01.next0 |> filledJustsMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)
                        , next1 = bNext01.next1 |> filledJustsMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)
                        }

                ValueAndNext0 bValueNext0 ->
                    maybeValueAndMaybeNext0
                        { value = AndOr.Both ( aValue, bValueNext0.value ) |> aAndOrBToValue
                        , next0 = bValueNext0.next0 |> filledJustsMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)
                        }

                ValueAndNext1 bValueNext1 ->
                    maybeValueAndMaybeNext1
                        { value = AndOr.Both ( aValue, bValueNext1.value ) |> aAndOrBToValue
                        , next1 = bValueNext1.next1 |> filledJustsMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)
                        }

                ValueAndNext0AndNext1 bValueNext01 ->
                    maybeValueAndMaybeNext0AndMaybeNext1
                        { value = AndOr.Both ( aValue, bValueNext01.value ) |> aAndOrBToValue
                        , next0 = bValueNext01.next0 |> filledJustsMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)
                        , next1 = bValueNext01.next1 |> filledJustsMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)
                        }

        Next0AndNext1 aNext01 ->
            case dictSafes.b of
                Next0 bNext0 ->
                    maybeNext0AndMaybeNext1
                        { next0 = { a = aNext01.next0, b = bNext0 } |> filledJustsMap2 aAndOrBToValue
                        , next1 = aNext01.next1 |> filledJustsMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        }

                Next1 bNext1 ->
                    maybeNext0AndMaybeNext1
                        { next0 = aNext01.next0 |> filledJustsMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        , next1 = { a = aNext01.next1, b = bNext1 } |> filledJustsMap2 aAndOrBToValue
                        }

                Value bValue ->
                    maybeValueAndMaybeNext0AndMaybeNext1
                        { value = AndOr.Only (Or.Second bValue) |> aAndOrBToValue
                        , next0 = aNext01.next0 |> filledJustsMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        , next1 = aNext01.next1 |> filledJustsMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        }

                Next0AndNext1 bNext01 ->
                    maybeNext0AndMaybeNext1
                        { next0 = { a = aNext01.next0, b = bNext01.next0 } |> filledJustsMap2 aAndOrBToValue
                        , next1 = { a = aNext01.next1, b = bNext01.next1 } |> filledJustsMap2 aAndOrBToValue
                        }

                ValueAndNext0 bValueNext0 ->
                    maybeValueAndMaybeNext0AndMaybeNext1
                        { value = AndOr.Only (Or.Second bValueNext0.value) |> aAndOrBToValue
                        , next0 = { a = aNext01.next0, b = bValueNext0.next0 } |> filledJustsMap2 aAndOrBToValue
                        , next1 = aNext01.next1 |> filledJustsMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        }

                ValueAndNext1 bValueNext1 ->
                    maybeValueAndMaybeNext0AndMaybeNext1
                        { value = AndOr.Only (Or.Second bValueNext1.value) |> aAndOrBToValue
                        , next0 = aNext01.next0 |> filledJustsMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        , next1 = { a = aNext01.next1, b = bValueNext1.next1 } |> filledJustsMap2 aAndOrBToValue
                        }

                ValueAndNext0AndNext1 bValueNext01 ->
                    maybeValueAndMaybeNext0AndMaybeNext1
                        { value = AndOr.Only (Or.Second bValueNext01.value) |> aAndOrBToValue
                        , next0 = { a = aNext01.next0, b = bValueNext01.next0 } |> filledJustsMap2 aAndOrBToValue
                        , next1 = { a = aNext01.next1, b = bValueNext01.next1 } |> filledJustsMap2 aAndOrBToValue
                        }

        ValueAndNext0 aValueNext0 ->
            case dictSafes.b of
                Next0 bNext0 ->
                    maybeValueAndMaybeNext0
                        { value = AndOr.Only (Or.First aValueNext0.value) |> aAndOrBToValue
                        , next0 = { a = aValueNext0.next0, b = bNext0 } |> filledJustsMap2 aAndOrBToValue
                        }

                Next1 bNext1 ->
                    maybeValueAndMaybeNext0AndMaybeNext1
                        { value = AndOr.Only (Or.First aValueNext0.value) |> aAndOrBToValue
                        , next0 = aValueNext0.next0 |> filledJustsMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        , next1 = bNext1 |> filledJustsMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)
                        }

                Value bValue ->
                    maybeValueAndMaybeNext0
                        { value = AndOr.Both ( aValueNext0.value, bValue ) |> aAndOrBToValue
                        , next0 = aValueNext0.next0 |> filledJustsMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        }

                Next0AndNext1 bNext01 ->
                    maybeValueAndMaybeNext0AndMaybeNext1
                        { value = AndOr.Only (Or.First aValueNext0.value) |> aAndOrBToValue
                        , next0 = { a = aValueNext0.next0, b = bNext01.next0 } |> filledJustsMap2 aAndOrBToValue
                        , next1 = bNext01.next1 |> filledJustsMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)
                        }

                ValueAndNext0 bValueNext0 ->
                    maybeValueAndMaybeNext0
                        { value = AndOr.Both ( aValueNext0.value, bValueNext0.value ) |> aAndOrBToValue
                        , next0 = { a = aValueNext0.next0, b = bValueNext0.next0 } |> filledJustsMap2 aAndOrBToValue
                        }

                ValueAndNext1 bValueNext1 ->
                    maybeValueAndMaybeNext0AndMaybeNext1
                        { value = AndOr.Both ( aValueNext0.value, bValueNext1.value ) |> aAndOrBToValue
                        , next0 = aValueNext0.next0 |> filledJustsMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        , next1 = bValueNext1.next1 |> filledJustsMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)
                        }

                ValueAndNext0AndNext1 bValueNext01 ->
                    maybeValueAndMaybeNext0AndMaybeNext1
                        { value = AndOr.Both ( aValueNext0.value, bValueNext01.value ) |> aAndOrBToValue
                        , next0 = { a = aValueNext0.next0, b = bValueNext01.next0 } |> filledJustsMap2 aAndOrBToValue
                        , next1 = bValueNext01.next1 |> filledJustsMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)
                        }

        ValueAndNext1 aValueNext1 ->
            case dictSafes.b of
                Next0 bNext0 ->
                    maybeValueAndMaybeNext0AndMaybeNext1
                        { value = AndOr.Only (Or.First aValueNext1.value) |> aAndOrBToValue
                        , next0 = bNext0 |> filledJustsMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)
                        , next1 = aValueNext1.next1 |> filledJustsMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        }

                Next1 bNext1 ->
                    maybeValueAndMaybeNext1
                        { value = AndOr.Only (Or.First aValueNext1.value) |> aAndOrBToValue
                        , next1 = { a = aValueNext1.next1, b = bNext1 } |> filledJustsMap2 aAndOrBToValue
                        }

                Value bValue ->
                    maybeValueAndMaybeNext1
                        { value = AndOr.Both ( aValueNext1.value, bValue ) |> aAndOrBToValue
                        , next1 = aValueNext1.next1 |> filledJustsMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        }

                Next0AndNext1 bNext01 ->
                    maybeValueAndMaybeNext0AndMaybeNext1
                        { value = AndOr.Only (Or.First aValueNext1.value) |> aAndOrBToValue
                        , next0 = bNext01.next0 |> filledJustsMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)
                        , next1 = { a = aValueNext1.next1, b = bNext01.next1 } |> filledJustsMap2 aAndOrBToValue
                        }

                ValueAndNext0 bValueNext0 ->
                    maybeValueAndMaybeNext0AndMaybeNext1
                        { value = AndOr.Both ( aValueNext1.value, bValueNext0.value ) |> aAndOrBToValue
                        , next0 = bValueNext0.next0 |> filledJustsMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)
                        , next1 = aValueNext1.next1 |> filledJustsMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        }

                ValueAndNext1 bValueNext1 ->
                    maybeValueAndMaybeNext1
                        { value = AndOr.Both ( aValueNext1.value, bValueNext1.value ) |> aAndOrBToValue
                        , next1 = { a = aValueNext1.next1, b = bValueNext1.next1 } |> filledJustsMap2 aAndOrBToValue
                        }

                ValueAndNext0AndNext1 bValueNext01 ->
                    maybeValueAndMaybeNext0AndMaybeNext1
                        { value = AndOr.Both ( aValueNext1.value, bValueNext01.value ) |> aAndOrBToValue
                        , next0 = bValueNext01.next0 |> filledJustsMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)
                        , next1 = { a = aValueNext1.next1, b = bValueNext01.next1 } |> filledJustsMap2 aAndOrBToValue
                        }

        ValueAndNext0AndNext1 aValueNext01 ->
            case dictSafes.b of
                Next0 bNext0 ->
                    maybeValueAndMaybeNext0AndMaybeNext1
                        { value = AndOr.Only (Or.First aValueNext01.value) |> aAndOrBToValue
                        , next0 = { a = aValueNext01.next0, b = bNext0 } |> filledJustsMap2 aAndOrBToValue
                        , next1 = aValueNext01.next1 |> filledJustsMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        }

                Next1 bNext1 ->
                    maybeValueAndMaybeNext0AndMaybeNext1
                        { value = AndOr.Only (Or.First aValueNext01.value) |> aAndOrBToValue
                        , next0 = aValueNext01.next0 |> filledJustsMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        , next1 = { a = aValueNext01.next1, b = bNext1 } |> filledJustsMap2 aAndOrBToValue
                        }

                Value bValue ->
                    maybeValueAndMaybeNext0AndMaybeNext1
                        { value = AndOr.Both ( aValueNext01.value, bValue ) |> aAndOrBToValue
                        , next0 = aValueNext01.next0 |> filledJustsMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        , next1 = aValueNext01.next1 |> filledJustsMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        }

                Next0AndNext1 bNext01 ->
                    maybeValueAndMaybeNext0AndMaybeNext1
                        { value = AndOr.Only (Or.First aValueNext01.value) |> aAndOrBToValue
                        , next0 = { a = aValueNext01.next0, b = bNext01.next0 } |> filledJustsMap2 aAndOrBToValue
                        , next1 = { a = aValueNext01.next1, b = bNext01.next1 } |> filledJustsMap2 aAndOrBToValue
                        }

                ValueAndNext0 bValueNext0 ->
                    maybeValueAndMaybeNext0AndMaybeNext1
                        { value = AndOr.Both ( aValueNext01.value, bValueNext0.value ) |> aAndOrBToValue
                        , next0 = { a = aValueNext01.next0, b = bValueNext0.next0 } |> filledJustsMap2 aAndOrBToValue
                        , next1 = aValueNext01.next1 |> filledJustsMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        }

                ValueAndNext1 bValueNext1 ->
                    maybeValueAndMaybeNext0AndMaybeNext1
                        { value = AndOr.Both ( aValueNext01.value, bValueNext1.value ) |> aAndOrBToValue
                        , next0 = aValueNext01.next0 |> filledJustsMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        , next1 = { a = aValueNext01.next1, b = bValueNext1.next1 } |> filledJustsMap2 aAndOrBToValue
                        }

                ValueAndNext0AndNext1 bValueNext01 ->
                    maybeValueAndMaybeNext0AndMaybeNext1
                        { value = AndOr.Both ( aValueNext01.value, bValueNext01.value ) |> aAndOrBToValue
                        , next0 = { a = aValueNext01.next0, b = bValueNext01.next0 } |> filledJustsMap2 aAndOrBToValue
                        , next1 = { a = aValueNext01.next1, b = bValueNext01.next1 } |> filledJustsMap2 aAndOrBToValue
                        }


{-| Combine the values of the 2 [`DictSafe`](#DictSafe)s depending on where keys are present in either or both,
see [`AndOr`](https://dark.elm.dmy.fr/packages/lue-bird/elm-and-or/latest/).

    import AndOr
    import Or

    union : { a : DictSafe aValue, b : DictSafe bValue } -> DictSafe value
    union =
        DictSafe.map2
            (\value2 ->
                case value2 of
                    AndOr.Both ( a, _ ) ->
                        a

                    AndOr.Only either ->
                        either |> Or.value
            )

If you want to drop some elements, use [`justsMap2`](#justsMap2),
if you need it even more flexible, use [`fold2Onto`](#fold2Onto)

roughly `O(max size)`

-}
map2 :
    (AndOr.AndOr aValue bValue -> value)
    ->
        ({ a : DictSafe aValue, b : DictSafe bValue }
         -> DictSafe value
        )
map2 aAndOrBToValue dictSafes =
    case dictSafes.a of
        Nothing ->
            case dictSafes.b of
                Nothing ->
                    Nothing

                Just onlyB ->
                    onlyB
                        |> filledMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)
                        |> Just

        Just aDictSafeFilled ->
            case dictSafes.b of
                Nothing ->
                    aDictSafeFilled
                        |> filledMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        |> Just

                Just bDictSafeFilled ->
                    { a = aDictSafeFilled, b = bDictSafeFilled }
                        |> filledMap2 aAndOrBToValue
                        |> Just


filledMap2 :
    (AndOr.AndOr aValue bValue -> value)
    ->
        ({ a : Filled aValue, b : Filled bValue }
         -> Filled value
        )
filledMap2 aAndOrBToValue dictSafes =
    case dictSafes.a of
        Next0 aNext0 ->
            case dictSafes.b of
                Next0 bNext0 ->
                    Next0 (filledMap2 aAndOrBToValue { a = aNext0, b = bNext0 })

                Next1 bNext1 ->
                    Next0AndNext1
                        { next0 = aNext0 |> filledMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        , next1 = bNext1 |> filledMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)
                        }

                Value bValue ->
                    ValueAndNext0
                        { next0 = aNext0 |> filledMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        , value = AndOr.Only (Or.Second bValue) |> aAndOrBToValue
                        }

                Next0AndNext1 bNext01 ->
                    Next0AndNext1
                        { next0 = { a = aNext0, b = bNext01.next0 } |> filledMap2 aAndOrBToValue
                        , next1 = bNext01.next1 |> filledMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)
                        }

                ValueAndNext0 bValueNext0 ->
                    ValueAndNext0
                        { value = AndOr.Only (Or.Second bValueNext0.value) |> aAndOrBToValue
                        , next0 = { a = aNext0, b = bValueNext0.next0 } |> filledMap2 aAndOrBToValue
                        }

                ValueAndNext1 bValueNext1 ->
                    ValueAndNext0AndNext1
                        { value = AndOr.Only (Or.Second bValueNext1.value) |> aAndOrBToValue
                        , next0 = aNext0 |> filledMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        , next1 = bValueNext1.next1 |> filledMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)
                        }

                ValueAndNext0AndNext1 bValueNext01 ->
                    ValueAndNext0AndNext1
                        { value = AndOr.Only (Or.Second bValueNext01.value) |> aAndOrBToValue
                        , next0 = { a = aNext0, b = bValueNext01.next0 } |> filledMap2 aAndOrBToValue
                        , next1 = bValueNext01.next1 |> filledMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)
                        }

        Next1 aNext1 ->
            case dictSafes.b of
                Next0 bNext0 ->
                    Next0AndNext1
                        { next0 = bNext0 |> filledMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)
                        , next1 = aNext1 |> filledMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        }

                Next1 bNext1 ->
                    Next1 (filledMap2 aAndOrBToValue { a = aNext1, b = bNext1 })

                Value bValue ->
                    ValueAndNext1
                        { value = AndOr.Only (Or.Second bValue) |> aAndOrBToValue
                        , next1 = aNext1 |> filledMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        }

                Next0AndNext1 bNext01 ->
                    Next0AndNext1
                        { next0 = bNext01.next0 |> filledMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)
                        , next1 = { a = aNext1, b = bNext01.next1 } |> filledMap2 aAndOrBToValue
                        }

                ValueAndNext0 bValueNext0 ->
                    ValueAndNext0AndNext1
                        { value = AndOr.Only (Or.Second bValueNext0.value) |> aAndOrBToValue
                        , next0 = bValueNext0.next0 |> filledMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)
                        , next1 = aNext1 |> filledMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        }

                ValueAndNext1 bValueNext1 ->
                    ValueAndNext1
                        { value = AndOr.Only (Or.Second bValueNext1.value) |> aAndOrBToValue
                        , next1 = aNext1 |> filledMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        }

                ValueAndNext0AndNext1 bValueNext01 ->
                    ValueAndNext0AndNext1
                        { value = AndOr.Only (Or.Second bValueNext01.value) |> aAndOrBToValue
                        , next0 = bValueNext01.next0 |> filledMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)
                        , next1 = aNext1 |> filledMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        }

        Value aValue ->
            case dictSafes.b of
                Next0 bNext0 ->
                    ValueAndNext0
                        { value = AndOr.Only (Or.First aValue) |> aAndOrBToValue
                        , next0 = bNext0 |> filledMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)
                        }

                Next1 bNext1 ->
                    ValueAndNext1
                        { value = AndOr.Only (Or.First aValue) |> aAndOrBToValue
                        , next1 = bNext1 |> filledMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)
                        }

                Value bValue ->
                    Value (AndOr.Both ( aValue, bValue ) |> aAndOrBToValue)

                Next0AndNext1 bNext01 ->
                    ValueAndNext0AndNext1
                        { value = AndOr.Only (Or.First aValue) |> aAndOrBToValue
                        , next0 = bNext01.next0 |> filledMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)
                        , next1 = bNext01.next1 |> filledMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)
                        }

                ValueAndNext0 bValueNext0 ->
                    ValueAndNext0
                        { value = AndOr.Both ( aValue, bValueNext0.value ) |> aAndOrBToValue
                        , next0 = bValueNext0.next0 |> filledMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)
                        }

                ValueAndNext1 bValueNext1 ->
                    ValueAndNext1
                        { value = AndOr.Both ( aValue, bValueNext1.value ) |> aAndOrBToValue
                        , next1 = bValueNext1.next1 |> filledMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)
                        }

                ValueAndNext0AndNext1 bValueNext01 ->
                    ValueAndNext0AndNext1
                        { value = AndOr.Both ( aValue, bValueNext01.value ) |> aAndOrBToValue
                        , next0 = bValueNext01.next0 |> filledMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)
                        , next1 = bValueNext01.next1 |> filledMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)
                        }

        Next0AndNext1 aNext01 ->
            case dictSafes.b of
                Next0 bNext0 ->
                    Next0AndNext1
                        { next0 = { a = aNext01.next0, b = bNext0 } |> filledMap2 aAndOrBToValue
                        , next1 = aNext01.next1 |> filledMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        }

                Next1 bNext1 ->
                    Next0AndNext1
                        { next0 = aNext01.next0 |> filledMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        , next1 = { a = aNext01.next1, b = bNext1 } |> filledMap2 aAndOrBToValue
                        }

                Value bValue ->
                    ValueAndNext0AndNext1
                        { value = AndOr.Only (Or.Second bValue) |> aAndOrBToValue
                        , next0 = aNext01.next0 |> filledMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        , next1 = aNext01.next1 |> filledMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        }

                Next0AndNext1 bNext01 ->
                    Next0AndNext1
                        { next0 = { a = aNext01.next0, b = bNext01.next0 } |> filledMap2 aAndOrBToValue
                        , next1 = { a = aNext01.next1, b = bNext01.next1 } |> filledMap2 aAndOrBToValue
                        }

                ValueAndNext0 bValueNext0 ->
                    ValueAndNext0AndNext1
                        { value = AndOr.Only (Or.Second bValueNext0.value) |> aAndOrBToValue
                        , next0 = { a = aNext01.next0, b = bValueNext0.next0 } |> filledMap2 aAndOrBToValue
                        , next1 = aNext01.next1 |> filledMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        }

                ValueAndNext1 bValueNext1 ->
                    ValueAndNext0AndNext1
                        { value = AndOr.Only (Or.Second bValueNext1.value) |> aAndOrBToValue
                        , next0 = aNext01.next0 |> filledMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        , next1 = { a = aNext01.next1, b = bValueNext1.next1 } |> filledMap2 aAndOrBToValue
                        }

                ValueAndNext0AndNext1 bValueNext01 ->
                    ValueAndNext0AndNext1
                        { value = AndOr.Only (Or.Second bValueNext01.value) |> aAndOrBToValue
                        , next0 = { a = aNext01.next0, b = bValueNext01.next0 } |> filledMap2 aAndOrBToValue
                        , next1 = { a = aNext01.next1, b = bValueNext01.next1 } |> filledMap2 aAndOrBToValue
                        }

        ValueAndNext0 aValueNext0 ->
            case dictSafes.b of
                Next0 bNext0 ->
                    ValueAndNext0
                        { value = AndOr.Only (Or.First aValueNext0.value) |> aAndOrBToValue
                        , next0 = { a = aValueNext0.next0, b = bNext0 } |> filledMap2 aAndOrBToValue
                        }

                Next1 bNext1 ->
                    ValueAndNext0AndNext1
                        { value = AndOr.Only (Or.First aValueNext0.value) |> aAndOrBToValue
                        , next0 = aValueNext0.next0 |> filledMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        , next1 = bNext1 |> filledMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)
                        }

                Value bValue ->
                    ValueAndNext0
                        { value = AndOr.Both ( aValueNext0.value, bValue ) |> aAndOrBToValue
                        , next0 = aValueNext0.next0 |> filledMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        }

                Next0AndNext1 bNext01 ->
                    ValueAndNext0AndNext1
                        { value = AndOr.Only (Or.First aValueNext0.value) |> aAndOrBToValue
                        , next0 = { a = aValueNext0.next0, b = bNext01.next0 } |> filledMap2 aAndOrBToValue
                        , next1 = bNext01.next1 |> filledMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)
                        }

                ValueAndNext0 bValueNext0 ->
                    ValueAndNext0
                        { value = AndOr.Both ( aValueNext0.value, bValueNext0.value ) |> aAndOrBToValue
                        , next0 = { a = aValueNext0.next0, b = bValueNext0.next0 } |> filledMap2 aAndOrBToValue
                        }

                ValueAndNext1 bValueNext1 ->
                    ValueAndNext0AndNext1
                        { value = AndOr.Both ( aValueNext0.value, bValueNext1.value ) |> aAndOrBToValue
                        , next0 = aValueNext0.next0 |> filledMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        , next1 = bValueNext1.next1 |> filledMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)
                        }

                ValueAndNext0AndNext1 bValueNext01 ->
                    ValueAndNext0AndNext1
                        { value = AndOr.Both ( aValueNext0.value, bValueNext01.value ) |> aAndOrBToValue
                        , next0 = { a = aValueNext0.next0, b = bValueNext01.next0 } |> filledMap2 aAndOrBToValue
                        , next1 = bValueNext01.next1 |> filledMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)
                        }

        ValueAndNext1 aValueNext1 ->
            case dictSafes.b of
                Next0 bNext0 ->
                    ValueAndNext0AndNext1
                        { value = AndOr.Only (Or.First aValueNext1.value) |> aAndOrBToValue
                        , next0 = bNext0 |> filledMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)
                        , next1 = aValueNext1.next1 |> filledMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        }

                Next1 bNext1 ->
                    ValueAndNext1
                        { value = AndOr.Only (Or.First aValueNext1.value) |> aAndOrBToValue
                        , next1 = { a = aValueNext1.next1, b = bNext1 } |> filledMap2 aAndOrBToValue
                        }

                Value bValue ->
                    ValueAndNext1
                        { value = AndOr.Both ( aValueNext1.value, bValue ) |> aAndOrBToValue
                        , next1 = aValueNext1.next1 |> filledMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        }

                Next0AndNext1 bNext01 ->
                    ValueAndNext0AndNext1
                        { value = AndOr.Only (Or.First aValueNext1.value) |> aAndOrBToValue
                        , next0 = bNext01.next0 |> filledMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)
                        , next1 = { a = aValueNext1.next1, b = bNext01.next1 } |> filledMap2 aAndOrBToValue
                        }

                ValueAndNext0 bValueNext0 ->
                    ValueAndNext0AndNext1
                        { value = AndOr.Both ( aValueNext1.value, bValueNext0.value ) |> aAndOrBToValue
                        , next0 = bValueNext0.next0 |> filledMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)
                        , next1 = aValueNext1.next1 |> filledMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        }

                ValueAndNext1 bValueNext1 ->
                    ValueAndNext1
                        { value = AndOr.Both ( aValueNext1.value, bValueNext1.value ) |> aAndOrBToValue
                        , next1 = { a = aValueNext1.next1, b = bValueNext1.next1 } |> filledMap2 aAndOrBToValue
                        }

                ValueAndNext0AndNext1 bValueNext01 ->
                    ValueAndNext0AndNext1
                        { value = AndOr.Both ( aValueNext1.value, bValueNext01.value ) |> aAndOrBToValue
                        , next0 = bValueNext01.next0 |> filledMap (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue)
                        , next1 = { a = aValueNext1.next1, b = bValueNext01.next1 } |> filledMap2 aAndOrBToValue
                        }

        ValueAndNext0AndNext1 aValueNext01 ->
            case dictSafes.b of
                Next0 bNext0 ->
                    ValueAndNext0AndNext1
                        { value = AndOr.Only (Or.First aValueNext01.value) |> aAndOrBToValue
                        , next0 = { a = aValueNext01.next0, b = bNext0 } |> filledMap2 aAndOrBToValue
                        , next1 = aValueNext01.next1 |> filledMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        }

                Next1 bNext1 ->
                    ValueAndNext0AndNext1
                        { value = AndOr.Only (Or.First aValueNext01.value) |> aAndOrBToValue
                        , next0 = aValueNext01.next0 |> filledMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        , next1 = { a = aValueNext01.next1, b = bNext1 } |> filledMap2 aAndOrBToValue
                        }

                Value bValue ->
                    ValueAndNext0AndNext1
                        { value = AndOr.Both ( aValueNext01.value, bValue ) |> aAndOrBToValue
                        , next0 = aValueNext01.next0 |> filledMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        , next1 = aValueNext01.next1 |> filledMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        }

                Next0AndNext1 bNext01 ->
                    ValueAndNext0AndNext1
                        { value = AndOr.Only (Or.First aValueNext01.value) |> aAndOrBToValue
                        , next0 = { a = aValueNext01.next0, b = bNext01.next0 } |> filledMap2 aAndOrBToValue
                        , next1 = { a = aValueNext01.next1, b = bNext01.next1 } |> filledMap2 aAndOrBToValue
                        }

                ValueAndNext0 bValueNext0 ->
                    ValueAndNext0AndNext1
                        { value = AndOr.Both ( aValueNext01.value, bValueNext0.value ) |> aAndOrBToValue
                        , next0 = { a = aValueNext01.next0, b = bValueNext0.next0 } |> filledMap2 aAndOrBToValue
                        , next1 = aValueNext01.next1 |> filledMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        }

                ValueAndNext1 bValueNext1 ->
                    ValueAndNext0AndNext1
                        { value = AndOr.Both ( aValueNext01.value, bValueNext1.value ) |> aAndOrBToValue
                        , next0 = aValueNext01.next0 |> filledMap (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue)
                        , next1 = { a = aValueNext01.next1, b = bValueNext1.next1 } |> filledMap2 aAndOrBToValue
                        }

                ValueAndNext0AndNext1 bValueNext01 ->
                    ValueAndNext0AndNext1
                        { value = AndOr.Both ( aValueNext01.value, bValueNext01.value ) |> aAndOrBToValue
                        , next0 = { a = aValueNext01.next0, b = bValueNext01.next0 } |> filledMap2 aAndOrBToValue
                        , next1 = { a = aValueNext01.next1, b = bValueNext01.next1 } |> filledMap2 aAndOrBToValue
                        }


{-| Fold the 2 [`DictSafe`](#DictSafe)s depending on where keys are present in either or both,
see [`AndOr`](https://dark.elm.dmy.fr/packages/lue-bird/elm-and-or/latest/).
The idea is the same as [`Dict.merge`](https://dark.elm.dmy.fr/packages/elm/core/latest/Dict#merge)

roughly `O(max size)`

Implementation detail: Traverses from highest to lowest key

-}
fold2Onto :
    folded
    -> (AndOr.AndOr aValue bValue -> (folded -> folded))
    ->
        ({ a : DictSafe aValue, b : DictSafe bValue }
         -> folded
        )
fold2Onto initialFolded reduceInAAndOrB dictSafes =
    let
        aDictSafe =
            dictSafes.a

        bDictSafe =
            dictSafes.b

        reduceInMiddleOnto : folded -> folded
        reduceInMiddleOnto fromFolded =
            case aDictSafe.value of
                Nothing ->
                    case bDictSafe.value of
                        Nothing ->
                            fromFolded

                        Just bValue ->
                            fromFolded |> reduceInAAndOrB (AndOr.Only (Or.Second bValue))

                Just aValue ->
                    case bDictSafe.value of
                        Nothing ->
                            fromFolded |> reduceInAAndOrB (AndOr.Only (Or.First aValue))

                        Just bValue ->
                            fromFolded |> reduceInAAndOrB (AndOr.Both ( aValue, bValue ))

        next1AndInitialFolded : folded
        next1AndInitialFolded =
            { a = aDictSafe.next1, b = bDictSafe.next1 }
                |> maybeFold2Onto initialFolded reduceInAAndOrB
    in
    { a = aDictSafe.next0, b = bDictSafe.next0 }
        |> maybeFold2Onto
            (reduceInMiddleOnto next1AndInitialFolded)
            reduceInAAndOrB


maybeFold2Onto :
    folded
    -> (AndOr.AndOr aValue bValue -> (folded -> folded))
    ->
        ({ a : Maybe (DictSafe aValue), b : Maybe (DictSafe bValue) }
         -> folded
        )
maybeFold2Onto initialFolded reduceInAAndOrB dictSafes =
    case dictSafes.a of
        Nothing ->
            dictSafes.b
                |> maybeFoldOnto initialFolded
                    (\bValue soFar -> soFar |> reduceInAAndOrB (AndOr.Only (Or.Second bValue)))

        Just aDictSafe ->
            case dictSafes.b of
                Nothing ->
                    aDictSafe
                        |> foldOnto initialFolded
                            (\aValue soFar -> soFar |> reduceInAAndOrB (AndOr.Only (Or.First aValue)))

                Just bDictSafe ->
                    { a = aDictSafe, b = bDictSafe } |> fold2Onto initialFolded reduceInAAndOrB
