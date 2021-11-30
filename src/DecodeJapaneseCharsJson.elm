module DecodeJapaneseCharsJson exposing (AllCharTablesJson, Category(..), CharDescription, SubCategory, allCharTablesDecoder, flatCharsList)

import Json.Decode as Decode exposing (Decoder, at, list, string, succeed)
import Json.Decode.Pipeline exposing (required)


type alias JapaneseCharJson =
    { character : String
    , pronunciation : String
    }


type Rows
    = Rows (List ( String, List JapaneseCharJson ))


type alias CharTableJson =
    { japaneseName : String
    , columnOrder : List String
    , rowOrder : List String
    , rows : Rows
    }


type alias AllCharTablesJson =
    { monographs : CharTableJson
    , digraphs : CharTableJson
    , monographsWithDiacritics : CharTableJson
    , digraphsWithDiacritics : CharTableJson
    }


japaneseCharDecoder : Decoder JapaneseCharJson
japaneseCharDecoder =
    succeed JapaneseCharJson
        |> required "Character" string
        |> required "Pronunciation" string


rowsDecoder : Decoder Rows
rowsDecoder =
    Decode.keyValuePairs (Decode.list japaneseCharDecoder)
        |> Decode.map Rows


charTableDecoder : Decoder CharTableJson
charTableDecoder =
    succeed CharTableJson
        |> required "Japanese name" string
        |> required "Column order" (Decode.list string)
        |> required "Row order" (Decode.list string)
        |> required "Rows" rowsDecoder


allCharTablesDecoder : Decoder AllCharTablesJson
allCharTablesDecoder =
    succeed AllCharTablesJson
        |> required "Monographs" charTableDecoder
        |> required "Digraphs" charTableDecoder
        |> required "Monographs with diacritics" charTableDecoder
        |> required "Digraphs with diacritics" charTableDecoder


type Category
    = Katakana
    | Hiragana


type SubCategory
    = Monographs
    | Digraphs
    | MonographsWithDiacritics
    | DigraphsWithDiacritics


type alias CharDescription =
    { category : Category
    , subCategory : SubCategory
    , character : String
    , pronunciation : String
    , row : String
    , column : String
    }


flattenRows : Category -> SubCategory -> Rows -> List CharDescription
flattenRows category subCategory (Rows rows) =
    rows
        |> List.concatMap
            (\( row, chars ) ->
                chars
                    |> List.map
                        (\{ character, pronunciation } ->
                            { category = category
                            , subCategory = subCategory
                            , character = character
                            , pronunciation = pronunciation
                            , row = row
                            , column = pronunciation |> String.reverse |> String.left 1
                            }
                        )
            )


flatCharsList : AllCharTablesJson -> List CharDescription
flatCharsList { monographs, digraphs, monographsWithDiacritics, digraphsWithDiacritics } =
    List.concat
        [ flattenRows Katakana Monographs monographs.rows
        , flattenRows Katakana Digraphs digraphs.rows
        , flattenRows Katakana MonographsWithDiacritics monographsWithDiacritics.rows
        , flattenRows Katakana DigraphsWithDiacritics digraphsWithDiacritics.rows
        ]
