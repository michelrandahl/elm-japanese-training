module QuizGeneration exposing (AlphabetType, CharacterPools, LazyGenerator(..), QuizQuestion, Translators, generateQuizList)

import Dict exposing (Dict)
import Random exposing (Generator)
import Random.List exposing (shuffle)
import RandomSample exposing (randomSample)


type LazyGenerator a
    = Nil
    | LazyNext ( a, () -> Generator (LazyGenerator a) )


type alias Translators =
    { romajiToKatakana : Dict String String
    , katakanaToRomaji : Dict String String
    }


type alias CharacterPools =
    { japaneseCharacterPool : List String
    , romajiCharacterPool : List String
    }


type AlphabetType
    = Japanese
    | Romaji


type alias QuizQuestion =
    { alphabetType : AlphabetType
    , options : List String
    , question : String
    , answer : String
    }


chooseOptionsPool : Translators -> AlphabetType -> List String
chooseOptionsPool { katakanaToRomaji, romajiToKatakana } alphabetType =
    case alphabetType of
        Japanese ->
            Dict.keys romajiToKatakana

        Romaji ->
            Dict.keys katakanaToRomaji


generateOptions : Int -> Translators -> AlphabetType -> String -> Generator (List String)
generateOptions numberOfQuizOptions translators alphabetType answer =
    chooseOptionsPool translators alphabetType
        |> List.filter ((/=) answer)
        |> randomSample numberOfQuizOptions
        |> Random.andThen (shuffle << (::) answer)


lookupAnswer : Translators -> AlphabetType -> String -> Maybe String
lookupAnswer translators alphabetType question =
    case alphabetType of
        Japanese ->
            Dict.get question translators.katakanaToRomaji

        Romaji ->
            Dict.get question translators.romajiToKatakana


generateQuestion : Int -> Translators -> AlphabetType -> Generator String -> Generator (Maybe QuizQuestion)
generateQuestion numberOfQuizOptions translators alphabetType genQuestion =
    genQuestion
        |> Random.andThen
            (\question ->
                question
                    |> lookupAnswer translators alphabetType
                    |> Maybe.map
                        (\answer ->
                            generateOptions numberOfQuizOptions translators alphabetType answer
                                |> Random.map
                                    (\options ->
                                        Just
                                            { alphabetType = alphabetType
                                            , options = options
                                            , question = question
                                            , answer = answer
                                            }
                                    )
                        )
                    |> Maybe.withDefault (Random.constant Nothing)
            )


updateCharacterPools : CharacterPools -> AlphabetType -> String -> CharacterPools
updateCharacterPools ({ japaneseCharacterPool, romajiCharacterPool } as charPools) alphabetType question =
    case alphabetType of
        Japanese ->
            { charPools | japaneseCharacterPool = List.filter ((/=) question) japaneseCharacterPool }

        Romaji ->
            { charPools | romajiCharacterPool = List.filter ((/=) question) romajiCharacterPool }


generateQuizList : Translators -> Int -> Int -> CharacterPools -> Generator (LazyGenerator QuizQuestion)
generateQuizList translators numberOfQuizOptions numberOfQuestions characterPools =
    if numberOfQuestions <= 0 then
        Random.constant Nil

    else
        let
            genQuizQuestion : AlphabetType -> Generator String -> Generator (Maybe QuizQuestion)
            genQuizQuestion =
                generateQuestion numberOfQuizOptions translators

            lazyGenerator : Maybe QuizQuestion -> LazyGenerator QuizQuestion
            lazyGenerator mQuizQuestion =
                mQuizQuestion
                    |> Maybe.map
                        (\({ alphabetType, question } as quizQuestion) ->
                            LazyNext
                                ( quizQuestion
                                , \_ ->
                                    updateCharacterPools characterPools alphabetType question
                                        |> generateQuizList translators numberOfQuizOptions (numberOfQuestions - 1)
                                )
                        )
                    |> Maybe.withDefault Nil
        in
        Random.uniform Japanese [ Romaji ]
            |> Random.andThen
                (\alphabetType ->
                    case ( alphabetType, characterPools.japaneseCharacterPool, characterPools.romajiCharacterPool ) of
                        ( Japanese, jaHead :: jaTail, _ ) ->
                            Random.uniform jaHead jaTail
                                |> genQuizQuestion alphabetType
                                |> Random.map lazyGenerator

                        ( Romaji, _, roHead :: roTail ) ->
                            Random.uniform roHead roTail
                                |> genQuizQuestion alphabetType
                                |> Random.map lazyGenerator

                        ( Romaji, jaHead :: jaTail, [] ) ->
                            Random.uniform jaHead jaTail
                                |> genQuizQuestion Japanese
                                |> Random.map lazyGenerator

                        ( Japanese, [], roHead :: roTail ) ->
                            Random.uniform roHead roTail
                                |> genQuizQuestion Romaji
                                |> Random.map lazyGenerator

                        ( _, _, _ ) ->
                            Random.constant Nil
                )
