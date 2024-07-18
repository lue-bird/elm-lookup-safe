module StructuredId exposing
    ( StructuredId, toString, toBits
    , ofUnit, ofInt, ofString
    , ofParts, ofVariant, ofMaybe, ofList
    )

{-| Assigning each unique value one "structured id"

@docs StructuredId, toString, toBits

@docs ofUnit, ofInt, ofString
@docs ofParts, ofVariant, ofMaybe, ofList

-}

import Bit exposing (Bit)
import Bytes
import Bytes.Decode
import Bytes.Encode
import Json.Encode


{-| A tree with just comparable values at the leaves.
This "structured id" can be compared or converted to a `String` or bits
which makes it possible to use as a key.
-}
type alias StructuredId =
    -- type StructuredId
    --     = String String
    --     | List (List StructuredId)
    --
    -- directly using the most efficient toString-able representation
    -- saves a toJson step but more importantly saves a List.map step in ofList
    Json.Encode.Value


ofUnit : StructuredId
ofUnit =
    Json.Encode.null


ofString : String -> StructuredId
ofString =
    \string ->
        string |> Json.Encode.string


ofInt : Int -> StructuredId
ofInt =
    \int -> int |> Json.Encode.int


ofParts : List StructuredId -> StructuredId
ofParts =
    \fieldValueStructureIds ->
        fieldValueStructureIds |> Json.Encode.list identity


{-|

  - If a variant has no value, use [`StructuredId.ofUnit`](#ofUnit)
  - If a variant has more than one value, use [`StructuredId.ofParts`](#ofParts)

-}
ofVariant : { tag : String, value : StructuredId } -> StructuredId
ofVariant =
    \variant ->
        [ variant.tag |> ofString, variant.value ] |> ofParts


ofMaybe : (value -> StructuredId) -> (Maybe value -> StructuredId)
ofMaybe valueToStructuredId =
    \maybe ->
        ofVariant
            (case maybe of
                Nothing ->
                    { tag = "Nothing", value = ofUnit }

                Just value ->
                    { tag = "Just", value = value |> valueToStructuredId }
            )


ofList : (element -> StructuredId) -> (List element -> StructuredId)
ofList elementToStructuredId =
    \structuredIds ->
        structuredIds |> Json.Encode.list elementToStructuredId


toString : StructuredId -> String
toString =
    \structuredId ->
        structuredId |> toJson |> Json.Encode.encode 0


toJson : StructuredId -> Json.Encode.Value
toJson =
    \structuredId ->
        structuredId


toBits : StructuredId -> List Bit
toBits =
    \structuredId ->
        structuredId
            |> toString
            |> Bytes.Encode.string
            |> Bytes.Encode.encode
            |> bytesToUnsignedInt8List
            |> List.concatMap int8ToBitList


bytesToUnsignedInt8List : Bytes.Bytes -> List Int
bytesToUnsignedInt8List =
    \bytes ->
        bytes
            |> Bytes.Decode.decode
                (unsignedInt8ListBytesDecoder (bytes |> Bytes.width))
            |> -- above decoder should never fail
               Maybe.withDefault []


unsignedInt8ListBytesDecoder : Int -> Bytes.Decode.Decoder (List Int)
unsignedInt8ListBytesDecoder length =
    Bytes.Decode.loop { remainingLength = length, elements = [] }
        (\soFar ->
            if soFar.remainingLength <= 0 then
                Bytes.Decode.succeed (Bytes.Decode.Done (soFar.elements |> List.reverse))

            else
                Bytes.Decode.map
                    (\byte ->
                        Bytes.Decode.Loop
                            { remainingLength = soFar.remainingLength - 1
                            , elements = byte :: soFar.elements
                            }
                    )
                    Bytes.Decode.unsignedInt8
        )


int8ToBitList : Int -> List Bit
int8ToBitList =
    \int8 ->
        case int8 of
            0 ->
                [ Bit.O ]

            1 ->
                [ Bit.I ]

            2 ->
                [ Bit.I, Bit.O ]

            3 ->
                [ Bit.I, Bit.I ]

            4 ->
                [ Bit.I, Bit.O, Bit.O ]

            5 ->
                [ Bit.I, Bit.O, Bit.I ]

            6 ->
                [ Bit.I, Bit.I, Bit.O ]

            7 ->
                [ Bit.I, Bit.I, Bit.I ]

            8 ->
                [ Bit.I, Bit.O, Bit.O, Bit.O ]

            9 ->
                [ Bit.I, Bit.O, Bit.O, Bit.I ]

            10 ->
                [ Bit.I, Bit.O, Bit.I, Bit.O ]

            11 ->
                [ Bit.I, Bit.O, Bit.I, Bit.I ]

            12 ->
                [ Bit.I, Bit.I, Bit.O, Bit.O ]

            13 ->
                [ Bit.I, Bit.I, Bit.O, Bit.I ]

            14 ->
                [ Bit.I, Bit.I, Bit.I, Bit.O ]

            15 ->
                [ Bit.I, Bit.I, Bit.I, Bit.I ]

            16 ->
                [ Bit.I, Bit.O, Bit.O, Bit.O, Bit.O ]

            17 ->
                [ Bit.I, Bit.O, Bit.O, Bit.O, Bit.I ]

            18 ->
                [ Bit.I, Bit.O, Bit.O, Bit.I, Bit.O ]

            19 ->
                [ Bit.I, Bit.O, Bit.O, Bit.I, Bit.I ]

            20 ->
                [ Bit.I, Bit.O, Bit.I, Bit.O, Bit.O ]

            21 ->
                [ Bit.I, Bit.O, Bit.I, Bit.O, Bit.I ]

            22 ->
                [ Bit.I, Bit.O, Bit.I, Bit.I, Bit.O ]

            23 ->
                [ Bit.I, Bit.O, Bit.I, Bit.I, Bit.I ]

            24 ->
                [ Bit.I, Bit.I, Bit.O, Bit.O, Bit.O ]

            25 ->
                [ Bit.I, Bit.I, Bit.O, Bit.O, Bit.I ]

            26 ->
                [ Bit.I, Bit.I, Bit.O, Bit.I, Bit.O ]

            27 ->
                [ Bit.I, Bit.I, Bit.O, Bit.I, Bit.I ]

            28 ->
                [ Bit.I, Bit.I, Bit.I, Bit.O, Bit.O ]

            29 ->
                [ Bit.I, Bit.I, Bit.I, Bit.O, Bit.I ]

            30 ->
                [ Bit.I, Bit.I, Bit.I, Bit.I, Bit.O ]

            31 ->
                [ Bit.I, Bit.I, Bit.I, Bit.I, Bit.I ]

            32 ->
                [ Bit.I, Bit.O, Bit.O, Bit.O, Bit.O, Bit.O ]

            33 ->
                [ Bit.I, Bit.O, Bit.O, Bit.O, Bit.O, Bit.I ]

            34 ->
                [ Bit.I, Bit.O, Bit.O, Bit.O, Bit.I, Bit.O ]

            35 ->
                [ Bit.I, Bit.O, Bit.O, Bit.O, Bit.I, Bit.I ]

            36 ->
                [ Bit.I, Bit.O, Bit.O, Bit.I, Bit.O, Bit.O ]

            37 ->
                [ Bit.I, Bit.O, Bit.O, Bit.I, Bit.O, Bit.I ]

            38 ->
                [ Bit.I, Bit.O, Bit.O, Bit.I, Bit.I, Bit.O ]

            39 ->
                [ Bit.I, Bit.O, Bit.O, Bit.I, Bit.I, Bit.I ]

            40 ->
                [ Bit.I, Bit.O, Bit.I, Bit.O, Bit.O, Bit.O ]

            41 ->
                [ Bit.I, Bit.O, Bit.I, Bit.O, Bit.O, Bit.I ]

            42 ->
                [ Bit.I, Bit.O, Bit.I, Bit.O, Bit.I, Bit.O ]

            43 ->
                [ Bit.I, Bit.O, Bit.I, Bit.O, Bit.I, Bit.I ]

            44 ->
                [ Bit.I, Bit.O, Bit.I, Bit.I, Bit.O, Bit.O ]

            45 ->
                [ Bit.I, Bit.O, Bit.I, Bit.I, Bit.O, Bit.I ]

            46 ->
                [ Bit.I, Bit.O, Bit.I, Bit.I, Bit.I, Bit.O ]

            47 ->
                [ Bit.I, Bit.O, Bit.I, Bit.I, Bit.I, Bit.I ]

            48 ->
                [ Bit.I, Bit.I, Bit.O, Bit.O, Bit.O, Bit.O ]

            49 ->
                [ Bit.I, Bit.I, Bit.O, Bit.O, Bit.O, Bit.I ]

            50 ->
                [ Bit.I, Bit.I, Bit.O, Bit.O, Bit.I, Bit.O ]

            51 ->
                [ Bit.I, Bit.I, Bit.O, Bit.O, Bit.I, Bit.I ]

            52 ->
                [ Bit.I, Bit.I, Bit.O, Bit.I, Bit.O, Bit.O ]

            53 ->
                [ Bit.I, Bit.I, Bit.O, Bit.I, Bit.O, Bit.I ]

            54 ->
                [ Bit.I, Bit.I, Bit.O, Bit.I, Bit.I, Bit.O ]

            55 ->
                [ Bit.I, Bit.I, Bit.O, Bit.I, Bit.I, Bit.I ]

            56 ->
                [ Bit.I, Bit.I, Bit.I, Bit.O, Bit.O, Bit.O ]

            57 ->
                [ Bit.I, Bit.I, Bit.I, Bit.O, Bit.O, Bit.I ]

            58 ->
                [ Bit.I, Bit.I, Bit.I, Bit.O, Bit.I, Bit.O ]

            59 ->
                [ Bit.I, Bit.I, Bit.I, Bit.O, Bit.I, Bit.I ]

            60 ->
                [ Bit.I, Bit.I, Bit.I, Bit.I, Bit.O, Bit.O ]

            61 ->
                [ Bit.I, Bit.I, Bit.I, Bit.I, Bit.O, Bit.I ]

            62 ->
                [ Bit.I, Bit.I, Bit.I, Bit.I, Bit.I, Bit.O ]

            63 ->
                [ Bit.I, Bit.I, Bit.I, Bit.I, Bit.I, Bit.I ]

            64 ->
                [ Bit.I, Bit.O, Bit.O, Bit.O, Bit.O, Bit.O, Bit.O ]

            65 ->
                [ Bit.I, Bit.O, Bit.O, Bit.O, Bit.O, Bit.O, Bit.I ]

            66 ->
                [ Bit.I, Bit.O, Bit.O, Bit.O, Bit.O, Bit.I, Bit.O ]

            67 ->
                [ Bit.I, Bit.O, Bit.O, Bit.O, Bit.O, Bit.I, Bit.I ]

            68 ->
                [ Bit.I, Bit.O, Bit.O, Bit.O, Bit.I, Bit.O, Bit.O ]

            69 ->
                [ Bit.I, Bit.O, Bit.O, Bit.O, Bit.I, Bit.O, Bit.I ]

            70 ->
                [ Bit.I, Bit.O, Bit.O, Bit.O, Bit.I, Bit.I, Bit.O ]

            71 ->
                [ Bit.I, Bit.O, Bit.O, Bit.O, Bit.I, Bit.I, Bit.I ]

            72 ->
                [ Bit.I, Bit.O, Bit.O, Bit.I, Bit.O, Bit.O, Bit.O ]

            73 ->
                [ Bit.I, Bit.O, Bit.O, Bit.I, Bit.O, Bit.O, Bit.I ]

            74 ->
                [ Bit.I, Bit.O, Bit.O, Bit.I, Bit.O, Bit.I, Bit.O ]

            75 ->
                [ Bit.I, Bit.O, Bit.O, Bit.I, Bit.O, Bit.I, Bit.I ]

            76 ->
                [ Bit.I, Bit.O, Bit.O, Bit.I, Bit.I, Bit.O, Bit.O ]

            77 ->
                [ Bit.I, Bit.O, Bit.O, Bit.I, Bit.I, Bit.O, Bit.I ]

            78 ->
                [ Bit.I, Bit.O, Bit.O, Bit.I, Bit.I, Bit.I, Bit.O ]

            79 ->
                [ Bit.I, Bit.O, Bit.O, Bit.I, Bit.I, Bit.I, Bit.I ]

            80 ->
                [ Bit.I, Bit.O, Bit.I, Bit.O, Bit.O, Bit.O, Bit.O ]

            81 ->
                [ Bit.I, Bit.O, Bit.I, Bit.O, Bit.O, Bit.O, Bit.I ]

            82 ->
                [ Bit.I, Bit.O, Bit.I, Bit.O, Bit.O, Bit.I, Bit.O ]

            83 ->
                [ Bit.I, Bit.O, Bit.I, Bit.O, Bit.O, Bit.I, Bit.I ]

            84 ->
                [ Bit.I, Bit.O, Bit.I, Bit.O, Bit.I, Bit.O, Bit.O ]

            85 ->
                [ Bit.I, Bit.O, Bit.I, Bit.O, Bit.I, Bit.O, Bit.I ]

            86 ->
                [ Bit.I, Bit.O, Bit.I, Bit.O, Bit.I, Bit.I, Bit.O ]

            87 ->
                [ Bit.I, Bit.O, Bit.I, Bit.O, Bit.I, Bit.I, Bit.I ]

            88 ->
                [ Bit.I, Bit.O, Bit.I, Bit.I, Bit.O, Bit.O, Bit.O ]

            89 ->
                [ Bit.I, Bit.O, Bit.I, Bit.I, Bit.O, Bit.O, Bit.I ]

            90 ->
                [ Bit.I, Bit.O, Bit.I, Bit.I, Bit.O, Bit.I, Bit.O ]

            91 ->
                [ Bit.I, Bit.O, Bit.I, Bit.I, Bit.O, Bit.I, Bit.I ]

            92 ->
                [ Bit.I, Bit.O, Bit.I, Bit.I, Bit.I, Bit.O, Bit.O ]

            93 ->
                [ Bit.I, Bit.O, Bit.I, Bit.I, Bit.I, Bit.O, Bit.I ]

            94 ->
                [ Bit.I, Bit.O, Bit.I, Bit.I, Bit.I, Bit.I, Bit.O ]

            95 ->
                [ Bit.I, Bit.O, Bit.I, Bit.I, Bit.I, Bit.I, Bit.I ]

            96 ->
                [ Bit.I, Bit.I, Bit.O, Bit.O, Bit.O, Bit.O, Bit.O ]

            97 ->
                [ Bit.I, Bit.I, Bit.O, Bit.O, Bit.O, Bit.O, Bit.I ]

            98 ->
                [ Bit.I, Bit.I, Bit.O, Bit.O, Bit.O, Bit.I, Bit.O ]

            99 ->
                [ Bit.I, Bit.I, Bit.O, Bit.O, Bit.O, Bit.I, Bit.I ]

            100 ->
                [ Bit.I, Bit.I, Bit.O, Bit.O, Bit.I, Bit.O, Bit.O ]

            101 ->
                [ Bit.I, Bit.I, Bit.O, Bit.O, Bit.I, Bit.O, Bit.I ]

            102 ->
                [ Bit.I, Bit.I, Bit.O, Bit.O, Bit.I, Bit.I, Bit.O ]

            103 ->
                [ Bit.I, Bit.I, Bit.O, Bit.O, Bit.I, Bit.I, Bit.I ]

            104 ->
                [ Bit.I, Bit.I, Bit.O, Bit.I, Bit.O, Bit.O, Bit.O ]

            105 ->
                [ Bit.I, Bit.I, Bit.O, Bit.I, Bit.O, Bit.O, Bit.I ]

            106 ->
                [ Bit.I, Bit.I, Bit.O, Bit.I, Bit.O, Bit.I, Bit.O ]

            107 ->
                [ Bit.I, Bit.I, Bit.O, Bit.I, Bit.O, Bit.I, Bit.I ]

            108 ->
                [ Bit.I, Bit.I, Bit.O, Bit.I, Bit.I, Bit.O, Bit.O ]

            109 ->
                [ Bit.I, Bit.I, Bit.O, Bit.I, Bit.I, Bit.O, Bit.I ]

            110 ->
                [ Bit.I, Bit.I, Bit.O, Bit.I, Bit.I, Bit.I, Bit.O ]

            111 ->
                [ Bit.I, Bit.I, Bit.O, Bit.I, Bit.I, Bit.I, Bit.I ]

            112 ->
                [ Bit.I, Bit.I, Bit.I, Bit.O, Bit.O, Bit.O, Bit.O ]

            113 ->
                [ Bit.I, Bit.I, Bit.I, Bit.O, Bit.O, Bit.O, Bit.I ]

            114 ->
                [ Bit.I, Bit.I, Bit.I, Bit.O, Bit.O, Bit.I, Bit.O ]

            115 ->
                [ Bit.I, Bit.I, Bit.I, Bit.O, Bit.O, Bit.I, Bit.I ]

            116 ->
                [ Bit.I, Bit.I, Bit.I, Bit.O, Bit.I, Bit.O, Bit.O ]

            117 ->
                [ Bit.I, Bit.I, Bit.I, Bit.O, Bit.I, Bit.O, Bit.I ]

            118 ->
                [ Bit.I, Bit.I, Bit.I, Bit.O, Bit.I, Bit.I, Bit.O ]

            119 ->
                [ Bit.I, Bit.I, Bit.I, Bit.O, Bit.I, Bit.I, Bit.I ]

            120 ->
                [ Bit.I, Bit.I, Bit.I, Bit.I, Bit.O, Bit.O, Bit.O ]

            121 ->
                [ Bit.I, Bit.I, Bit.I, Bit.I, Bit.O, Bit.O, Bit.I ]

            122 ->
                [ Bit.I, Bit.I, Bit.I, Bit.I, Bit.O, Bit.I, Bit.O ]

            123 ->
                [ Bit.I, Bit.I, Bit.I, Bit.I, Bit.O, Bit.I, Bit.I ]

            124 ->
                [ Bit.I, Bit.I, Bit.I, Bit.I, Bit.I, Bit.O, Bit.O ]

            125 ->
                [ Bit.I, Bit.I, Bit.I, Bit.I, Bit.I, Bit.O, Bit.I ]

            126 ->
                [ Bit.I, Bit.I, Bit.I, Bit.I, Bit.I, Bit.I, Bit.O ]

            127 ->
                [ Bit.I, Bit.I, Bit.I, Bit.I, Bit.I, Bit.I, Bit.I ]

            128 ->
                [ Bit.I, Bit.O, Bit.O, Bit.O, Bit.O, Bit.O, Bit.O, Bit.O ]

            129 ->
                [ Bit.I, Bit.O, Bit.O, Bit.O, Bit.O, Bit.O, Bit.O, Bit.I ]

            130 ->
                [ Bit.I, Bit.O, Bit.O, Bit.O, Bit.O, Bit.O, Bit.I, Bit.O ]

            131 ->
                [ Bit.I, Bit.O, Bit.O, Bit.O, Bit.O, Bit.O, Bit.I, Bit.I ]

            132 ->
                [ Bit.I, Bit.O, Bit.O, Bit.O, Bit.O, Bit.I, Bit.O, Bit.O ]

            133 ->
                [ Bit.I, Bit.O, Bit.O, Bit.O, Bit.O, Bit.I, Bit.O, Bit.I ]

            134 ->
                [ Bit.I, Bit.O, Bit.O, Bit.O, Bit.O, Bit.I, Bit.I, Bit.O ]

            135 ->
                [ Bit.I, Bit.O, Bit.O, Bit.O, Bit.O, Bit.I, Bit.I, Bit.I ]

            136 ->
                [ Bit.I, Bit.O, Bit.O, Bit.O, Bit.I, Bit.O, Bit.O, Bit.O ]

            137 ->
                [ Bit.I, Bit.O, Bit.O, Bit.O, Bit.I, Bit.O, Bit.O, Bit.I ]

            138 ->
                [ Bit.I, Bit.O, Bit.O, Bit.O, Bit.I, Bit.O, Bit.I, Bit.O ]

            139 ->
                [ Bit.I, Bit.O, Bit.O, Bit.O, Bit.I, Bit.O, Bit.I, Bit.I ]

            140 ->
                [ Bit.I, Bit.O, Bit.O, Bit.O, Bit.I, Bit.I, Bit.O, Bit.O ]

            141 ->
                [ Bit.I, Bit.O, Bit.O, Bit.O, Bit.I, Bit.I, Bit.O, Bit.I ]

            142 ->
                [ Bit.I, Bit.O, Bit.O, Bit.O, Bit.I, Bit.I, Bit.I, Bit.O ]

            143 ->
                [ Bit.I, Bit.O, Bit.O, Bit.O, Bit.I, Bit.I, Bit.I, Bit.I ]

            144 ->
                [ Bit.I, Bit.O, Bit.O, Bit.I, Bit.O, Bit.O, Bit.O, Bit.O ]

            145 ->
                [ Bit.I, Bit.O, Bit.O, Bit.I, Bit.O, Bit.O, Bit.O, Bit.I ]

            146 ->
                [ Bit.I, Bit.O, Bit.O, Bit.I, Bit.O, Bit.O, Bit.I, Bit.O ]

            147 ->
                [ Bit.I, Bit.O, Bit.O, Bit.I, Bit.O, Bit.O, Bit.I, Bit.I ]

            148 ->
                [ Bit.I, Bit.O, Bit.O, Bit.I, Bit.O, Bit.I, Bit.O, Bit.O ]

            149 ->
                [ Bit.I, Bit.O, Bit.O, Bit.I, Bit.O, Bit.I, Bit.O, Bit.I ]

            150 ->
                [ Bit.I, Bit.O, Bit.O, Bit.I, Bit.O, Bit.I, Bit.I, Bit.O ]

            151 ->
                [ Bit.I, Bit.O, Bit.O, Bit.I, Bit.O, Bit.I, Bit.I, Bit.I ]

            152 ->
                [ Bit.I, Bit.O, Bit.O, Bit.I, Bit.I, Bit.O, Bit.O, Bit.O ]

            153 ->
                [ Bit.I, Bit.O, Bit.O, Bit.I, Bit.I, Bit.O, Bit.O, Bit.I ]

            154 ->
                [ Bit.I, Bit.O, Bit.O, Bit.I, Bit.I, Bit.O, Bit.I, Bit.O ]

            155 ->
                [ Bit.I, Bit.O, Bit.O, Bit.I, Bit.I, Bit.O, Bit.I, Bit.I ]

            156 ->
                [ Bit.I, Bit.O, Bit.O, Bit.I, Bit.I, Bit.I, Bit.O, Bit.O ]

            157 ->
                [ Bit.I, Bit.O, Bit.O, Bit.I, Bit.I, Bit.I, Bit.O, Bit.I ]

            158 ->
                [ Bit.I, Bit.O, Bit.O, Bit.I, Bit.I, Bit.I, Bit.I, Bit.O ]

            159 ->
                [ Bit.I, Bit.O, Bit.O, Bit.I, Bit.I, Bit.I, Bit.I, Bit.I ]

            160 ->
                [ Bit.I, Bit.O, Bit.I, Bit.O, Bit.O, Bit.O, Bit.O, Bit.O ]

            161 ->
                [ Bit.I, Bit.O, Bit.I, Bit.O, Bit.O, Bit.O, Bit.O, Bit.I ]

            162 ->
                [ Bit.I, Bit.O, Bit.I, Bit.O, Bit.O, Bit.O, Bit.I, Bit.O ]

            163 ->
                [ Bit.I, Bit.O, Bit.I, Bit.O, Bit.O, Bit.O, Bit.I, Bit.I ]

            164 ->
                [ Bit.I, Bit.O, Bit.I, Bit.O, Bit.O, Bit.I, Bit.O, Bit.O ]

            165 ->
                [ Bit.I, Bit.O, Bit.I, Bit.O, Bit.O, Bit.I, Bit.O, Bit.I ]

            166 ->
                [ Bit.I, Bit.O, Bit.I, Bit.O, Bit.O, Bit.I, Bit.I, Bit.O ]

            167 ->
                [ Bit.I, Bit.O, Bit.I, Bit.O, Bit.O, Bit.I, Bit.I, Bit.I ]

            168 ->
                [ Bit.I, Bit.O, Bit.I, Bit.O, Bit.I, Bit.O, Bit.O, Bit.O ]

            169 ->
                [ Bit.I, Bit.O, Bit.I, Bit.O, Bit.I, Bit.O, Bit.O, Bit.I ]

            170 ->
                [ Bit.I, Bit.O, Bit.I, Bit.O, Bit.I, Bit.O, Bit.I, Bit.O ]

            171 ->
                [ Bit.I, Bit.O, Bit.I, Bit.O, Bit.I, Bit.O, Bit.I, Bit.I ]

            172 ->
                [ Bit.I, Bit.O, Bit.I, Bit.O, Bit.I, Bit.I, Bit.O, Bit.O ]

            173 ->
                [ Bit.I, Bit.O, Bit.I, Bit.O, Bit.I, Bit.I, Bit.O, Bit.I ]

            174 ->
                [ Bit.I, Bit.O, Bit.I, Bit.O, Bit.I, Bit.I, Bit.I, Bit.O ]

            175 ->
                [ Bit.I, Bit.O, Bit.I, Bit.O, Bit.I, Bit.I, Bit.I, Bit.I ]

            176 ->
                [ Bit.I, Bit.O, Bit.I, Bit.I, Bit.O, Bit.O, Bit.O, Bit.O ]

            177 ->
                [ Bit.I, Bit.O, Bit.I, Bit.I, Bit.O, Bit.O, Bit.O, Bit.I ]

            178 ->
                [ Bit.I, Bit.O, Bit.I, Bit.I, Bit.O, Bit.O, Bit.I, Bit.O ]

            179 ->
                [ Bit.I, Bit.O, Bit.I, Bit.I, Bit.O, Bit.O, Bit.I, Bit.I ]

            180 ->
                [ Bit.I, Bit.O, Bit.I, Bit.I, Bit.O, Bit.I, Bit.O, Bit.O ]

            181 ->
                [ Bit.I, Bit.O, Bit.I, Bit.I, Bit.O, Bit.I, Bit.O, Bit.I ]

            182 ->
                [ Bit.I, Bit.O, Bit.I, Bit.I, Bit.O, Bit.I, Bit.I, Bit.O ]

            183 ->
                [ Bit.I, Bit.O, Bit.I, Bit.I, Bit.O, Bit.I, Bit.I, Bit.I ]

            184 ->
                [ Bit.I, Bit.O, Bit.I, Bit.I, Bit.I, Bit.O, Bit.O, Bit.O ]

            185 ->
                [ Bit.I, Bit.O, Bit.I, Bit.I, Bit.I, Bit.O, Bit.O, Bit.I ]

            186 ->
                [ Bit.I, Bit.O, Bit.I, Bit.I, Bit.I, Bit.O, Bit.I, Bit.O ]

            187 ->
                [ Bit.I, Bit.O, Bit.I, Bit.I, Bit.I, Bit.O, Bit.I, Bit.I ]

            188 ->
                [ Bit.I, Bit.O, Bit.I, Bit.I, Bit.I, Bit.I, Bit.O, Bit.O ]

            189 ->
                [ Bit.I, Bit.O, Bit.I, Bit.I, Bit.I, Bit.I, Bit.O, Bit.I ]

            190 ->
                [ Bit.I, Bit.O, Bit.I, Bit.I, Bit.I, Bit.I, Bit.I, Bit.O ]

            191 ->
                [ Bit.I, Bit.O, Bit.I, Bit.I, Bit.I, Bit.I, Bit.I, Bit.I ]

            192 ->
                [ Bit.I, Bit.I, Bit.O, Bit.O, Bit.O, Bit.O, Bit.O, Bit.O ]

            193 ->
                [ Bit.I, Bit.I, Bit.O, Bit.O, Bit.O, Bit.O, Bit.O, Bit.I ]

            194 ->
                [ Bit.I, Bit.I, Bit.O, Bit.O, Bit.O, Bit.O, Bit.I, Bit.O ]

            195 ->
                [ Bit.I, Bit.I, Bit.O, Bit.O, Bit.O, Bit.O, Bit.I, Bit.I ]

            196 ->
                [ Bit.I, Bit.I, Bit.O, Bit.O, Bit.O, Bit.I, Bit.O, Bit.O ]

            197 ->
                [ Bit.I, Bit.I, Bit.O, Bit.O, Bit.O, Bit.I, Bit.O, Bit.I ]

            198 ->
                [ Bit.I, Bit.I, Bit.O, Bit.O, Bit.O, Bit.I, Bit.I, Bit.O ]

            199 ->
                [ Bit.I, Bit.I, Bit.O, Bit.O, Bit.O, Bit.I, Bit.I, Bit.I ]

            200 ->
                [ Bit.I, Bit.I, Bit.O, Bit.O, Bit.I, Bit.O, Bit.O, Bit.O ]

            201 ->
                [ Bit.I, Bit.I, Bit.O, Bit.O, Bit.I, Bit.O, Bit.O, Bit.I ]

            202 ->
                [ Bit.I, Bit.I, Bit.O, Bit.O, Bit.I, Bit.O, Bit.I, Bit.O ]

            203 ->
                [ Bit.I, Bit.I, Bit.O, Bit.O, Bit.I, Bit.O, Bit.I, Bit.I ]

            204 ->
                [ Bit.I, Bit.I, Bit.O, Bit.O, Bit.I, Bit.I, Bit.O, Bit.O ]

            205 ->
                [ Bit.I, Bit.I, Bit.O, Bit.O, Bit.I, Bit.I, Bit.O, Bit.I ]

            206 ->
                [ Bit.I, Bit.I, Bit.O, Bit.O, Bit.I, Bit.I, Bit.I, Bit.O ]

            207 ->
                [ Bit.I, Bit.I, Bit.O, Bit.O, Bit.I, Bit.I, Bit.I, Bit.I ]

            208 ->
                [ Bit.I, Bit.I, Bit.O, Bit.I, Bit.O, Bit.O, Bit.O, Bit.O ]

            209 ->
                [ Bit.I, Bit.I, Bit.O, Bit.I, Bit.O, Bit.O, Bit.O, Bit.I ]

            210 ->
                [ Bit.I, Bit.I, Bit.O, Bit.I, Bit.O, Bit.O, Bit.I, Bit.O ]

            211 ->
                [ Bit.I, Bit.I, Bit.O, Bit.I, Bit.O, Bit.O, Bit.I, Bit.I ]

            212 ->
                [ Bit.I, Bit.I, Bit.O, Bit.I, Bit.O, Bit.I, Bit.O, Bit.O ]

            213 ->
                [ Bit.I, Bit.I, Bit.O, Bit.I, Bit.O, Bit.I, Bit.O, Bit.I ]

            214 ->
                [ Bit.I, Bit.I, Bit.O, Bit.I, Bit.O, Bit.I, Bit.I, Bit.O ]

            215 ->
                [ Bit.I, Bit.I, Bit.O, Bit.I, Bit.O, Bit.I, Bit.I, Bit.I ]

            216 ->
                [ Bit.I, Bit.I, Bit.O, Bit.I, Bit.I, Bit.O, Bit.O, Bit.O ]

            217 ->
                [ Bit.I, Bit.I, Bit.O, Bit.I, Bit.I, Bit.O, Bit.O, Bit.I ]

            218 ->
                [ Bit.I, Bit.I, Bit.O, Bit.I, Bit.I, Bit.O, Bit.I, Bit.O ]

            219 ->
                [ Bit.I, Bit.I, Bit.O, Bit.I, Bit.I, Bit.O, Bit.I, Bit.I ]

            220 ->
                [ Bit.I, Bit.I, Bit.O, Bit.I, Bit.I, Bit.I, Bit.O, Bit.O ]

            221 ->
                [ Bit.I, Bit.I, Bit.O, Bit.I, Bit.I, Bit.I, Bit.O, Bit.I ]

            222 ->
                [ Bit.I, Bit.I, Bit.O, Bit.I, Bit.I, Bit.I, Bit.I, Bit.O ]

            223 ->
                [ Bit.I, Bit.I, Bit.O, Bit.I, Bit.I, Bit.I, Bit.I, Bit.I ]

            224 ->
                [ Bit.I, Bit.I, Bit.I, Bit.O, Bit.O, Bit.O, Bit.O, Bit.O ]

            225 ->
                [ Bit.I, Bit.I, Bit.I, Bit.O, Bit.O, Bit.O, Bit.O, Bit.I ]

            226 ->
                [ Bit.I, Bit.I, Bit.I, Bit.O, Bit.O, Bit.O, Bit.I, Bit.O ]

            227 ->
                [ Bit.I, Bit.I, Bit.I, Bit.O, Bit.O, Bit.O, Bit.I, Bit.I ]

            228 ->
                [ Bit.I, Bit.I, Bit.I, Bit.O, Bit.O, Bit.I, Bit.O, Bit.O ]

            229 ->
                [ Bit.I, Bit.I, Bit.I, Bit.O, Bit.O, Bit.I, Bit.O, Bit.I ]

            230 ->
                [ Bit.I, Bit.I, Bit.I, Bit.O, Bit.O, Bit.I, Bit.I, Bit.O ]

            231 ->
                [ Bit.I, Bit.I, Bit.I, Bit.O, Bit.O, Bit.I, Bit.I, Bit.I ]

            232 ->
                [ Bit.I, Bit.I, Bit.I, Bit.O, Bit.I, Bit.O, Bit.O, Bit.O ]

            233 ->
                [ Bit.I, Bit.I, Bit.I, Bit.O, Bit.I, Bit.O, Bit.O, Bit.I ]

            234 ->
                [ Bit.I, Bit.I, Bit.I, Bit.O, Bit.I, Bit.O, Bit.I, Bit.O ]

            235 ->
                [ Bit.I, Bit.I, Bit.I, Bit.O, Bit.I, Bit.O, Bit.I, Bit.I ]

            236 ->
                [ Bit.I, Bit.I, Bit.I, Bit.O, Bit.I, Bit.I, Bit.O, Bit.O ]

            237 ->
                [ Bit.I, Bit.I, Bit.I, Bit.O, Bit.I, Bit.I, Bit.O, Bit.I ]

            238 ->
                [ Bit.I, Bit.I, Bit.I, Bit.O, Bit.I, Bit.I, Bit.I, Bit.O ]

            239 ->
                [ Bit.I, Bit.I, Bit.I, Bit.O, Bit.I, Bit.I, Bit.I, Bit.I ]

            240 ->
                [ Bit.I, Bit.I, Bit.I, Bit.I, Bit.O, Bit.O, Bit.O, Bit.O ]

            241 ->
                [ Bit.I, Bit.I, Bit.I, Bit.I, Bit.O, Bit.O, Bit.O, Bit.I ]

            242 ->
                [ Bit.I, Bit.I, Bit.I, Bit.I, Bit.O, Bit.O, Bit.I, Bit.O ]

            243 ->
                [ Bit.I, Bit.I, Bit.I, Bit.I, Bit.O, Bit.O, Bit.I, Bit.I ]

            244 ->
                [ Bit.I, Bit.I, Bit.I, Bit.I, Bit.O, Bit.I, Bit.O, Bit.O ]

            245 ->
                [ Bit.I, Bit.I, Bit.I, Bit.I, Bit.O, Bit.I, Bit.O, Bit.I ]

            246 ->
                [ Bit.I, Bit.I, Bit.I, Bit.I, Bit.O, Bit.I, Bit.I, Bit.O ]

            247 ->
                [ Bit.I, Bit.I, Bit.I, Bit.I, Bit.O, Bit.I, Bit.I, Bit.I ]

            248 ->
                [ Bit.I, Bit.I, Bit.I, Bit.I, Bit.I, Bit.O, Bit.O, Bit.O ]

            249 ->
                [ Bit.I, Bit.I, Bit.I, Bit.I, Bit.I, Bit.O, Bit.O, Bit.I ]

            250 ->
                [ Bit.I, Bit.I, Bit.I, Bit.I, Bit.I, Bit.O, Bit.I, Bit.O ]

            251 ->
                [ Bit.I, Bit.I, Bit.I, Bit.I, Bit.I, Bit.O, Bit.I, Bit.I ]

            252 ->
                [ Bit.I, Bit.I, Bit.I, Bit.I, Bit.I, Bit.I, Bit.O, Bit.O ]

            253 ->
                [ Bit.I, Bit.I, Bit.I, Bit.I, Bit.I, Bit.I, Bit.O, Bit.I ]

            254 ->
                [ Bit.I, Bit.I, Bit.I, Bit.I, Bit.I, Bit.I, Bit.I, Bit.O ]

            _ ->
                [ Bit.I, Bit.I, Bit.I, Bit.I, Bit.I, Bit.I, Bit.I, Bit.I ]
