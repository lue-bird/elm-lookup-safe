module LookupSafe exposing
    ( LookupSafe(..)
    , empty, one, fromListMap
    , valueAtKey
    , set, alterAtKey, map
    , toList, foldOnto
    , map2, fold2Onto
    )

{-|

@docs LookupSafe


## create

@docs empty, one, fromListMap


## observe

@docs valueAtKey

You can check for emptiness with for example `== LookupSafe.empty`


## alter

@docs set, alterAtKey, map


## transform

@docs toList, foldOnto


## combine

@docs map2, fold2Onto

-}

import AndOr
import Bit exposing (Bit)
import Or
import StructuredId exposing (StructuredId)


{-| A lookup that maps unique keys to values.

Note: calling `==` on 2 of these will work correctly (and be roughly `O(size)`).

Internally, it's a binary trie.

-}
type LookupSafe value
    = Branch
        { value : Maybe value
        , next0 : Maybe (LookupSafe value)
        , next1 : Maybe (LookupSafe value)
        }


{-| Everything starts somewhere. Start filling it up using [`set`](#set)
-}
empty : LookupSafe value_
empty =
    Branch { value = Nothing, next0 = Nothing, next1 = Nothing }


{-| Convert from a `List` by converting each element to key-value entries.

`O(length * key info density)` (roughly `O(length * log(length))`)

-}
fromListMap :
    (element -> { key : StructuredId, value : value })
    -> (List element -> LookupSafe value)
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
    { key : StructuredId, value : value }
    -> (LookupSafe value -> LookupSafe value)
set entry lookupSafe =
    lookupSafe
        |> alterAtKey entry.key
            (\_ -> entry.value |> Just)


alterAtKeyBits :
    List Bit
    -> (Maybe value -> Maybe value)
    -> (LookupSafe value -> LookupSafe value)
alterAtKeyBits key maybeValueChange (Branch lookupSafe) =
    case key of
        [] ->
            Branch
                { value = maybeValueChange Nothing
                , next0 = lookupSafe.next0
                , next1 = lookupSafe.next1
                }

        firstBit :: secondBitUp ->
            case firstBit of
                Bit.O ->
                    Branch
                        { value = lookupSafe.value
                        , next0 = lookupSafe.next0 |> maybeAlterBits secondBitUp maybeValueChange
                        , next1 = lookupSafe.next1
                        }

                Bit.I ->
                    Branch
                        { value = lookupSafe.value
                        , next1 = lookupSafe.next1 |> maybeAlterBits secondBitUp maybeValueChange
                        , next0 = lookupSafe.next0
                        }


{-| Change the contained value at a given key.
`Nothing` if the key wasn't associated with a value previously, `Just` if it was.

The given change function should return `Just` to set a value at the key and `Nothing` to remove it.

`O(key info density)` (roughly `O(log(size))`)

-}
alterAtKey :
    StructuredId
    -> (Maybe value -> Maybe value)
    -> (LookupSafe value -> LookupSafe value)
alterAtKey entryKey entryValue lookupSafe =
    lookupSafe
        |> alterAtKeyBits
            (entryKey |> StructuredId.toBits)
            entryValue


oneBits : List Bit -> value -> LookupSafe value
oneBits key value =
    case key of
        [] ->
            Branch { value = Just value, next0 = Nothing, next1 = Nothing }

        firstBit :: secondBitUp ->
            case firstBit of
                Bit.O ->
                    Branch
                        { value = Nothing
                        , next0 = Just (oneBits secondBitUp value)
                        , next1 = Nothing
                        }

                Bit.I ->
                    Branch
                        { value = Nothing
                        , next0 = Nothing
                        , next1 = Just (oneBits secondBitUp value)
                        }


{-| `O(key info density)`
-}
one : { key : StructuredId, value : value } -> LookupSafe value
one entry =
    oneBits
        (entry.key |> StructuredId.toBits)
        entry.value


maybeAlterBits :
    List Bit
    -> (Maybe value -> Maybe value)
    -> (Maybe (LookupSafe value) -> Maybe (LookupSafe value))
maybeAlterBits key maybeValueChange maybeLookupSafe =
    case maybeLookupSafe of
        Nothing ->
            case maybeValueChange Nothing of
                Nothing ->
                    Nothing

                Just value ->
                    Just (oneBits key value)

        Just lookupSafe ->
            Just (lookupSafe |> alterAtKeyBits key maybeValueChange)


valueAtKeyBits : List Bit -> LookupSafe value -> Maybe value
valueAtKeyBits key (Branch lookupSafe) =
    case key of
        [] ->
            lookupSafe.value

        firstBit :: secondBitUp ->
            case firstBit of
                Bit.O ->
                    maybeValueAtKeyBits secondBitUp lookupSafe.next0

                Bit.I ->
                    maybeValueAtKeyBits secondBitUp lookupSafe.next1


{-| `Just` the value associated with a given key, or `Nothing` if it's not a member.
-}
valueAtKey : StructuredId -> LookupSafe value -> Maybe value
valueAtKey key lookupSafe =
    lookupSafe |> valueAtKeyBits (key |> StructuredId.toBits)


maybeValueAtKeyBits : List Bit -> Maybe (LookupSafe value) -> Maybe value
maybeValueAtKeyBits key maybeLookupSafe =
    case maybeLookupSafe of
        Nothing ->
            Nothing

        Just lookupSafe ->
            lookupSafe |> valueAtKeyBits key


{-| Convert its values to a `List`.

`O(size)`

Implementation detail: The resulting elements are ordered from highest to lowest key

-}
toList : LookupSafe value -> List value
toList lookupSafe =
    lookupSafe |> toListMap Basics.identity


{-| Convert its values to a `List`, transformed in a given way.

`O(size)`

-}
toListMap : (value -> element) -> (LookupSafe value -> List element)
toListMap valueToElement lookupSafe =
    lookupSafe |> foldOnto [] (\value soFar -> (value |> valueToElement) :: soFar)


{-| Change each value.

`O(size)`

-}
map : (value -> valueChanged) -> (LookupSafe value -> LookupSafe valueChanged)
map valueChange (Branch lookupSafe) =
    Branch
        { next0 = lookupSafe.next0 |> maybeMap valueChange
        , value = lookupSafe.value |> Maybe.map valueChange
        , next1 = lookupSafe.next1 |> maybeMap valueChange
        }


maybeMap :
    (value -> valueChanged)
    -> (Maybe (LookupSafe value) -> Maybe (LookupSafe valueChanged))
maybeMap valueChange maybeLookupSafe =
    case maybeLookupSafe of
        Nothing ->
            Nothing

        Just lookupSafe ->
            lookupSafe |> map valueChange |> Just


maybeFoldOnto :
    folded
    -> (value -> (folded -> folded))
    -> (Maybe (LookupSafe value) -> folded)
maybeFoldOnto soFar reduceInValue maybeLookupSafe =
    case maybeLookupSafe of
        Nothing ->
            soFar

        Just lookupSafe ->
            lookupSafe |> foldOnto soFar reduceInValue


{-| Condense all values by reducing value after value into a given initial thing.

`O(size)`

Implementation detail: Traverses from highest to lowest key.

-}
foldOnto :
    folded
    -> (value -> (folded -> folded))
    -> (LookupSafe value -> folded)
foldOnto initialFolded reduceInValue (Branch lookupSafe) =
    let
        next1ListOntoSoFar : folded
        next1ListOntoSoFar =
            lookupSafe.next1 |> maybeFoldOnto initialFolded reduceInValue
    in
    lookupSafe.next0
        |> maybeFoldOnto
            (case lookupSafe.value of
                Nothing ->
                    next1ListOntoSoFar

                Just middleValue ->
                    next1ListOntoSoFar |> reduceInValue middleValue
            )
            reduceInValue


{-| Combine the values of the 2 [`LookupSafe`](#LookupSafe)s depending on where keys are present in either or both,
see [`AndOr`](https://dark.elm.dmy.fr/packages/lue-bird/elm-and-or/latest/).

roughly `O(max size)`

-}
map2 :
    (AndOr.AndOr aValue bValue -> value)
    ->
        ({ a : LookupSafe aValue, b : LookupSafe bValue }
         -> LookupSafe value
        )
map2 aAndOrBToValue lookupSafes =
    let
        (Branch aLookupSafe) =
            lookupSafes.a

        (Branch bLookupSafe) =
            lookupSafes.b
    in
    Branch
        { next0 = maybeMap2 aAndOrBToValue { a = aLookupSafe.next0, b = bLookupSafe.next0 }
        , value =
            case aLookupSafe.value of
                Nothing ->
                    case bLookupSafe.value of
                        Nothing ->
                            Nothing

                        Just onlyBValue ->
                            AndOr.Only (Or.Second onlyBValue) |> aAndOrBToValue |> Just

                Just onlyAValue ->
                    case bLookupSafe.value of
                        Nothing ->
                            AndOr.Only (Or.First onlyAValue) |> aAndOrBToValue |> Just

                        Just onlyBValue ->
                            AndOr.Both ( onlyAValue, onlyBValue ) |> aAndOrBToValue |> Just
        , next1 = maybeMap2 aAndOrBToValue { a = aLookupSafe.next1, b = bLookupSafe.next1 }
        }


maybeMap2 :
    (AndOr.AndOr aValue bValue -> value)
    ->
        ({ a : Maybe (LookupSafe aValue), b : Maybe (LookupSafe bValue) }
         -> Maybe (LookupSafe value)
        )
maybeMap2 aAndOrBToValue lookupSafes =
    case lookupSafes.a of
        Nothing ->
            case lookupSafes.b of
                Nothing ->
                    Nothing

                Just onlyB ->
                    onlyB |> map (\bValue -> AndOr.Only (Or.Second bValue) |> aAndOrBToValue) |> Just

        Just onlyA ->
            case lookupSafes.b of
                Nothing ->
                    onlyA |> map (\aValue -> AndOr.Only (Or.First aValue) |> aAndOrBToValue) |> Just

                Just onlyB ->
                    map2 aAndOrBToValue { a = onlyA, b = onlyB } |> Just


{-| Fold the 2 [`LookupSafe`](#LookupSafe)s depending on where keys are present in either or both,
see [`AndOr`](https://dark.elm.dmy.fr/packages/lue-bird/elm-and-or/latest/).
The idea is the same as [`Dict.merge`](https://dark.elm.dmy.fr/packages/elm/core/latest/Dict#merge)

roughly `O(max size)`

Implementation detail: Traverses from highest to lowest key

-}
fold2Onto :
    folded
    -> (AndOr.AndOr aValue bValue -> (folded -> folded))
    ->
        ({ a : LookupSafe aValue, b : LookupSafe bValue }
         -> folded
        )
fold2Onto initialFolded reduceInAAndOrB lookupSafes =
    let
        (Branch aLookupSafe) =
            lookupSafes.a

        (Branch bLookupSafe) =
            lookupSafes.b

        reduceInMiddleOnto : folded -> folded
        reduceInMiddleOnto fromFolded =
            case aLookupSafe.value of
                Nothing ->
                    case bLookupSafe.value of
                        Nothing ->
                            fromFolded

                        Just bValue ->
                            fromFolded |> reduceInAAndOrB (AndOr.Only (Or.Second bValue))

                Just aValue ->
                    case bLookupSafe.value of
                        Nothing ->
                            fromFolded |> reduceInAAndOrB (AndOr.Only (Or.First aValue))

                        Just bValue ->
                            fromFolded |> reduceInAAndOrB (AndOr.Both ( aValue, bValue ))

        next1AndInitialFolded : folded
        next1AndInitialFolded =
            { a = aLookupSafe.next1, b = bLookupSafe.next1 }
                |> maybeFold2Onto initialFolded reduceInAAndOrB
    in
    { a = aLookupSafe.next0, b = bLookupSafe.next0 }
        |> maybeFold2Onto
            (reduceInMiddleOnto next1AndInitialFolded)
            reduceInAAndOrB


maybeFold2Onto :
    folded
    -> (AndOr.AndOr aValue bValue -> (folded -> folded))
    ->
        ({ a : Maybe (LookupSafe aValue), b : Maybe (LookupSafe bValue) }
         -> folded
        )
maybeFold2Onto initialFolded reduceInAAndOrB lookupSafes =
    case lookupSafes.a of
        Nothing ->
            lookupSafes.b
                |> maybeFoldOnto initialFolded
                    (\bValue soFar -> soFar |> reduceInAAndOrB (AndOr.Only (Or.Second bValue)))

        Just aLookupSafe ->
            case lookupSafes.b of
                Nothing ->
                    aLookupSafe
                        |> foldOnto initialFolded
                            (\aValue soFar -> soFar |> reduceInAAndOrB (AndOr.Only (Or.First aValue)))

                Just bLookupSafe ->
                    { a = aLookupSafe, b = bLookupSafe } |> fold2Onto initialFolded reduceInAAndOrB
