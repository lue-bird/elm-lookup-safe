module DictSafe exposing
    ( DictSafe(..)
    , empty, one, fromListMap
    , valueAtKey
    , set
    , toList, foldOnto, fold2Onto
    )

{-| key-value lookup

@docs DictSafe


## create

@docs empty, one, fromListMap


## observe

@docs valueAtKey

You can check for emptiness with for example `== DictSafe.empty`


## alter

@docs set


## transform

@docs toList, foldOnto, fold2Onto

-}

import AndOr
import Bit exposing (Bit)
import Or
import StructuredId exposing (StructuredId)


{-| A lookup that maps unique keys to values.

Note: calling `==` on 2 of these will work correctly (and be roughly `O(size)`).

Internally, it's a binary trie.

-}
type DictSafe value
    = Branch
        { value : Maybe value
        , next0 : Maybe (DictSafe value)
        , next1 : Maybe (DictSafe value)
        }


{-| Everything starts somewhere. Start filling it up using [`set`](#set)
-}
empty : DictSafe value_
empty =
    Branch { value = Nothing, next0 = Nothing, next1 = Nothing }


{-| Convert from a `List` by converting each element to key-value entries.

`O(length * key info density)` (roughly `O(length * log(length))`)

-}
fromListMap :
    (element -> { key : StructuredId, value : value })
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
    { key : StructuredId, value : value }
    -> (DictSafe value -> DictSafe value)
set entry dictSafe =
    dictSafe
        |> alterAtKey entry.key
            (\_ -> entry.value |> Just)


{-| Change the contained value at a given key.
`Nothing` if the key wasn't associated with a value previously, `Just` if it was.

The given change function should return `Just` to set a value at the key and `Nothing` to remove it.

`O(key info density)` (roughly `O(log(size))`)

-}
alterAtKey :
    StructuredId
    -> (Maybe value -> Maybe value)
    -> (DictSafe value -> DictSafe value)
alterAtKey entryKey entryValue dictSafe =
    dictSafe
        |> alterAtKeyBits
            (entryKey |> StructuredId.toBits)
            entryValue


alterAtKeyBits :
    List Bit
    -> (Maybe value -> Maybe value)
    -> (DictSafe value -> DictSafe value)
alterAtKeyBits key maybeValueChange (Branch dictSafe) =
    case key of
        [] ->
            Branch
                { value = maybeValueChange Nothing
                , next0 = dictSafe.next0
                , next1 = dictSafe.next1
                }

        firstBit :: secondBitUp ->
            case firstBit of
                Bit.O ->
                    Branch
                        { value = dictSafe.value
                        , next0 = dictSafe.next0 |> maybeAlterBits secondBitUp maybeValueChange
                        , next1 = dictSafe.next1
                        }

                Bit.I ->
                    Branch
                        { value = dictSafe.value
                        , next1 = dictSafe.next1 |> maybeAlterBits secondBitUp maybeValueChange
                        , next0 = dictSafe.next0
                        }


oneBits : List Bit -> value -> DictSafe value
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
one : { key : StructuredId, value : value } -> DictSafe value
one entry =
    oneBits
        (entry.key |> StructuredId.toBits)
        entry.value


maybeAlterBits :
    List Bit
    -> (Maybe value -> Maybe value)
    -> (Maybe (DictSafe value) -> Maybe (DictSafe value))
maybeAlterBits key maybeValueChange maybeDictSafe =
    case maybeDictSafe of
        Nothing ->
            case maybeValueChange Nothing of
                Nothing ->
                    Nothing

                Just value ->
                    Just (oneBits key value)

        Just dictSafe ->
            Just (dictSafe |> alterAtKeyBits key maybeValueChange)


valueAtKeyBits : List Bit -> DictSafe value -> Maybe value
valueAtKeyBits key (Branch dictSafe) =
    case key of
        [] ->
            dictSafe.value

        firstBit :: secondBitUp ->
            case firstBit of
                Bit.O ->
                    maybeValueAtKeyBits secondBitUp dictSafe.next0

                Bit.I ->
                    maybeValueAtKeyBits secondBitUp dictSafe.next1


{-| `Just` the value associated with a given key, or `Nothing` if it's not a member.
-}
valueAtKey : StructuredId -> DictSafe value -> Maybe value
valueAtKey key dictSafe =
    dictSafe |> valueAtKeyBits (key |> StructuredId.toBits)


maybeValueAtKeyBits : List Bit -> Maybe (DictSafe value) -> Maybe value
maybeValueAtKeyBits key maybeDictSafe =
    case maybeDictSafe of
        Nothing ->
            Nothing

        Just dictSafe ->
            dictSafe |> valueAtKeyBits key


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
foldOnto initialFolded reduceInValue (Branch dictSafe) =
    let
        next1ListOntoSoFar : folded
        next1ListOntoSoFar =
            dictSafe.next1 |> maybeFoldOnto initialFolded reduceInValue
    in
    dictSafe.next0
        |> maybeFoldOnto
            (case dictSafe.value of
                Nothing ->
                    next1ListOntoSoFar

                Just middleValue ->
                    next1ListOntoSoFar |> reduceInValue middleValue
            )
            reduceInValue


{-| Fold the 2 [`DictSafe`](#DictSafe)s depending on where keys are present in either or both,
see [`AndOr`](https://dark.elm.dmy.fr/packages/lue-bird/elm-and-or/latest/).
The idea is the same as [`Dict.merge`](https://dark.elm.dmy.fr/packages/elm/core/latest/Dict#merge)

`O(max size)`

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
        (Branch aDictSafe) =
            dictSafes.a

        (Branch bDictSafe) =
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
