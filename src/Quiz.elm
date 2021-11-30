port module Quiz exposing (main)

import Browser
import DecodeJapaneseCharsJson exposing (AllCharTablesJson, Category(..), CharDescription, allCharTablesDecoder, flatCharsList)
import Dict exposing (Dict)
import Html exposing (Html, button, div, h1, h5, i, span, text)
import Html.Attributes exposing (class, classList, disabled, id, property, style, type_)
import Html.Events exposing (onClick)
import Http
import Json.Encode as Encode
import QuizGeneration exposing (CharacterPools, LazyGenerator(..), QuizQuestion, Translators, generateQuizList)
import Random exposing (Generator)


port enableModal : String -> Cmd msg


type Msg
    = GeneratedQuiz (LazyGenerator QuizQuestion)
    | InitializeQuiz
    | GenNextQuestion
    | NextQuestion (LazyGenerator QuizQuestion)
    | GotKatakanaJson (Result Http.Error AllCharTablesJson)
    | SelectedQuizOption String
    | OpenModal


type alias CoreData =
    { katakanaToRomaji : Dict String String
    , romajiToKatakana : Dict String String
    , katakanaJson : AllCharTablesJson
    , flatKatakana : List CharDescription
    }


type alias QuizFeedback =
    { success : Bool
    , correct : String
    , wrong : Maybe String
    }


type alias Model =
    { quiz : Maybe QuizQuestion
    , quizGenerator : Generator (LazyGenerator QuizQuestion)
    , quizFeedback : Maybe QuizFeedback
    , score : Int
    , totalNumberOfQuestions : Int
    , questionsLeft : Int
    , errorMessage : Maybe String
    , coreData : Maybe CoreData
    }


initialCmd : Cmd Msg
initialCmd =
    Http.get
        { url = "./data/katakana.json"
        , expect = Http.expectJson GotKatakanaJson allCharTablesDecoder
        }


initialModel : Model
initialModel =
    { errorMessage = Nothing
    , quizFeedback = Nothing
    , quizGenerator = Random.constant Nil
    , score = 0
    , totalNumberOfQuestions = 0
    , questionsLeft = 0
    , coreData = Nothing
    , quiz = Nothing
    }


quizOption : Maybe QuizFeedback -> String -> Html Msg
quizOption quizFeedback character =
    let
        hasFeedback : Bool
        hasFeedback =
            quizFeedback
                |> Maybe.map (always True)
                |> Maybe.withDefault False

        greenButton : Bool
        greenButton =
            quizFeedback
                |> Maybe.map ((==) character << .correct)
                |> Maybe.withDefault False

        redButton : Bool
        redButton =
            quizFeedback
                |> Maybe.andThen .wrong
                |> Maybe.map ((==) character)
                |> Maybe.withDefault False
    in
    div []
        [ button
            [ disabled hasFeedback
            , classList
                [ ( "btn", True )
                , ( "btn-success", not hasFeedback )
                , ( "btn-secondary", hasFeedback && not greenButton && not redButton )
                , ( "btn-success", greenButton )
                , ( "btn-danger", redButton )
                ]
            , onClick (SelectedQuizOption character)
            ]
            [ text character ]
        ]


strProperty : String -> String -> Html.Attribute msg
strProperty name value =
    property name (Encode.string value)


viewProgressBar : Int -> Int -> Html Msg
viewProgressBar n maxN =
    let
        progress : Int
        progress =
            round (100.0 - (toFloat n / toFloat maxN) * 100.0)
    in
    div [ class "progress" ]
        [ div
            [ class "progress-bar"
            , strProperty "role" "progressbar"
            , style "width" (String.fromInt progress ++ "%")
            , strProperty "aria-valuenow" (String.fromInt progress)
            , strProperty "aria-valuemin" "0"
            , strProperty "aria-valuemax" "100"
            ]
            [ text (String.fromInt progress ++ "%") ]
        ]


viewQuiz : Maybe QuizFeedback -> Int -> Int -> QuizQuestion -> Html Msg
viewQuiz quizFeedback totalNumberOfQuestions questionsLeft { question, options } =
    div []
        [ div [ id "quiz-question" ] [ i [] [ text "Question: " ], text question ]
        , div [ id "quiz-options", class "btn-group-vertical" ]
            (List.map (quizOption quizFeedback) options)
        , quizFeedback
            |> Maybe.map
                (\{ success } ->
                    div []
                        [ div []
                            [ text
                                (if success then
                                    "correct!"

                                 else
                                    "wrong!"
                                )
                            ]
                        , button
                            [ class "btn btn-primary"
                            , onClick GenNextQuestion
                            ]
                            [ text "next question" ]
                        ]
                )
            |> Maybe.withDefault (div [] [])
        , viewProgressBar questionsLeft totalNumberOfQuestions
        ]


viewCharOverviewModal : Model -> Html Msg
viewCharOverviewModal _ =
    div
        [ class "modal fade"
        , id "exampleModal"
        , strProperty "tabindex" "-1"
        , strProperty "aria-labelledby" "exampleModalLabel"
        , strProperty "aria-hidden" "true"
        ]
        [ div [ class "modal-dialog" ]
            [ div [ class "modal-content" ]
                [ div [ class "modal-header" ]
                    [ h5 [ class "modal-title", id "exampleModalLabel" ]
                        [ text "Modal title" ]
                    , button
                        [ type_ "button"
                        , class "close"
                        , strProperty "data-dismiss" "modal"
                        , strProperty "aria-label" "Close"
                        ]
                        [ span [ strProperty "aria-hidden" "true" ]
                            [ text "&times;" ]
                        ]
                    ]
                ]
            , div [ class "modal-body" ] [ text "..." ]
            , div [ class "modal-footer" ]
                [ button
                    [ type_ "button"
                    , class "btn btn-secondary"
                    , strProperty "data-dismiss" "modal"
                    ]
                    [ text "Close" ]
                ]
            ]
        ]


view : Model -> Html Msg
view { coreData, quiz, quizFeedback, score, questionsLeft, totalNumberOfQuestions } =
    div []
        (case coreData of
            Just _ ->
                [ h1 [] [ text "Japanese Character Training" ]
                , div [] [ i [] [ text ("score: " ++ String.fromInt score) ] ]
                , quiz
                    |> Maybe.map (always (div [] []))
                    |> Maybe.withDefault
                        (div []
                            [ button
                                [ class "btn btn-primary"
                                , onClick InitializeQuiz
                                ]
                                [ text "Start Quiz" ]
                            ]
                        )
                , quiz
                    |> Maybe.map (always (div [] []))
                    |> Maybe.withDefault
                        (button
                            [ class "btn btn-primary"
                            , onClick OpenModal
                            ]
                            [ text "open demo modal" ]
                        )
                , quiz
                    |> Maybe.map
                        (viewQuiz quizFeedback
                            totalNumberOfQuestions
                            questionsLeft
                        )
                    |> Maybe.withDefault (div [] [])
                ]

            Nothing ->
                [ text "core data not loaded!" ]
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OpenModal ->
            ( model, enableModal "hello foobar from elm" )

        GeneratedQuiz (LazyNext ( quiz, makeGenerator )) ->
            ( { model
                | quiz = Just quiz
                , quizGenerator = makeGenerator ()
                , score = 0
              }
            , Cmd.none
            )

        GeneratedQuiz Nil ->
            ( { model
                | errorMessage = Just "failed to generate quiz"
                , questionsLeft = 0
                , totalNumberOfQuestions = 0
              }
            , Cmd.none
            )

        NextQuestion (LazyNext ( quiz, makeGenerator )) ->
            ( { model | quiz = Just quiz, quizGenerator = makeGenerator () }
            , Cmd.none
            )

        NextQuestion Nil ->
            ( model, Cmd.none )

        GenNextQuestion ->
            ( { model
                | quizFeedback = Nothing
                , quiz = Nothing
              }
            , model.quizGenerator |> Random.generate NextQuestion
            )

        SelectedQuizOption selectedQuizOption ->
            model.quiz
                |> Maybe.map
                    (\quiz ->
                        let
                            success : Bool
                            success =
                                quiz.answer == selectedQuizOption

                            wrong : Maybe String
                            wrong =
                                if success then
                                    Nothing

                                else
                                    Just selectedQuizOption

                            score =
                                if success then
                                    model.score + 1

                                else
                                    model.score - 1
                        in
                        ( { model
                            | quizFeedback =
                                Just
                                    { success = success
                                    , correct = quiz.answer
                                    , wrong = wrong
                                    }
                            , score = score
                            , questionsLeft = model.questionsLeft - 1
                          }
                        , Cmd.none
                        )
                    )
                |> Maybe.withDefault ( model, Cmd.none )

        InitializeQuiz ->
            model.coreData
                |> Maybe.map
                    (\{ katakanaToRomaji, romajiToKatakana } ->
                        let
                            numberOfQuestions : Int
                            numberOfQuestions =
                                5

                            translators : Translators
                            translators =
                                { katakanaToRomaji = katakanaToRomaji
                                , romajiToKatakana = romajiToKatakana
                                }

                            characterPools : CharacterPools
                            characterPools =
                                { japaneseCharacterPool = Dict.keys katakanaToRomaji
                                , romajiCharacterPool = Dict.keys romajiToKatakana
                                }

                            gen : Generator (LazyGenerator QuizQuestion)
                            gen =
                                generateQuizList translators 4 5 characterPools
                        in
                        ( { model
                            | questionsLeft = numberOfQuestions
                            , totalNumberOfQuestions = numberOfQuestions
                          }
                        , Random.generate GeneratedQuiz gen
                        )
                    )
                |> Maybe.withDefault ( model, Cmd.none )

        GotKatakanaJson (Ok json) ->
            ( let
                flatKatakana =
                    flatCharsList json
              in
              { model
                | coreData =
                    Just
                        { katakanaJson = json
                        , flatKatakana = flatKatakana
                        , katakanaToRomaji =
                            flatKatakana
                                |> List.map (\{ character, pronunciation } -> ( character, pronunciation ))
                                |> Dict.fromList
                        , romajiToKatakana =
                            flatKatakana
                                |> List.map (\{ character, pronunciation } -> ( pronunciation, character ))
                                |> Dict.fromList
                        }
              }
            , Cmd.none
            )

        GotKatakanaJson (Err _) ->
            ( { model | errorMessage = Just "Failed to fetch katakana json" }, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, initialCmd )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
