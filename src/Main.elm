module Main exposing (..)

import Browser
import Browser.Dom exposing (Element)
import Element exposing (Element, alignLeft, alignRight, centerX, centerY, column, el, fill, link, padding, paddingXY, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as ElmEvent
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import List
import String exposing (fromInt)



---- MODEL ----


type alias Chapter =
    { title : Int
    , maxScore : Int
    , score : Int
    }


type alias Model =
    { page : Course
    , chapters : List Chapter
    , score : Int
    , psp : Int
    }


math101Chapters : List Chapter
math101Chapters =
    [ Chapter 2 100 0
    , Chapter 3 78 0
    , Chapter 4 75 0
    ]


math102Chapters : List Chapter
math102Chapters =
    [ Chapter 5 100 0
    , Chapter 6 100 0
    , Chapter 7 83 0
    , Chapter 8 100 0
    , Chapter 11 90 0
    ]


math201Chapters : List Chapter
math201Chapters =
    [ Chapter 10 100 0
    , Chapter 12 100 0
    , Chapter 14 100 0
    , Chapter 15 67 0
    ]


init : ( Model, Cmd Msg )
init =
    ( { page = MATH101, chapters = math101Chapters, score = 0, psp = 20 }, Cmd.none )



---- UPDATE ----


type Course
    = MATH101
    | MATH102
    | MATH201


type Msg
    = NoOp
    | GoToCourse Course
    | Calculate
    | ScoreChange Chapter String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GoToCourse page ->
            case page of
                MATH101 ->
                    ( Model page math101Chapters 0 20, Cmd.none )

                MATH102 ->
                    ( Model page math102Chapters 0 20, Cmd.none )

                MATH201 ->
                    ( Model page math201Chapters 0 15, Cmd.none )

        ScoreChange ch strScore ->
            let
                otherChapters =
                    List.filter (\c -> c.title /= ch.title) model.chapters

                score =
                    (Maybe.withDefault 0 <| String.toInt strScore) |> min 100

                updatedCh =
                    Chapter ch.title ch.maxScore score

                chapters =
                    updatedCh :: otherChapters
            in
            ( { model | chapters = List.sortBy .title chapters }, Cmd.none )

        Calculate ->
            let
                scoreSum =
                    List.sum (List.map calculateChapterScore model.chapters)

                scorePercentage =
                    toFloat scoreSum / (toFloat <| List.length model.chapters)

                score =
                    toFloat model.psp * (scorePercentage / 100.0) |> ceiling
            in
            ( { model | score = score }, Cmd.none )

        _ ->
            ( model, Cmd.none )


calculateChapterScore : Chapter -> Int
calculateChapterScore ch =
    round ((toFloat ch.score / toFloat ch.maxScore) * 100) |> min 100



---- VIEW ----


calculatorView : Model -> Html Msg
calculatorView model =
    Element.layout [] <|
        column
            [ width fill, centerY, spacing 30 ]
            [ column
                [ centerX
                , Background.color purple
                , Font.color (rgb255 255 255 255)
                , padding 30
                , Border.rounded 10
                , spacing 25
                ]
                [ el
                    [ Font.size 40
                    , centerX
                    ]
                    (text "WEBASSIGN PSP")
                , el
                    [ Font.size 20
                    , centerX
                    ]
                    (text "Score Calculator")
                ]
            , row [ centerX, spacing 25, Font.size 26 ]
                [ el []
                    (link [ Font.color blue, ElmEvent.onClick (GoToCourse MATH101) ]
                        { url = "#"
                        , label = text "MATH101"
                        }
                    )
                , el []
                    (link [ Font.color blue, ElmEvent.onClick (GoToCourse MATH102) ]
                        { url = "#"
                        , label = text "MATH102"
                        }
                    )
                , el []
                    (link [ Font.color blue, ElmEvent.onClick (GoToCourse MATH201) ]
                        { url = "#"
                        , label = text "MATH201"
                        }
                    )
                ]
            , row [ centerX, spacing 25, Font.size 26 ] [ viewPage model ]
            ]


viewMessage : Html Msg
viewMessage =
    column [ centerX, centerY, spacing 25 ]
        [ el [ Font.size 25, Font.bold ] (text "The calculator is not available anymore.")
        , el [ Font.size 25, Font.bold ] (text " Please check your scores on")
        , el [ alignRight ] <| Element.newTabLink [ Font.color blue ] { label = text "Darajati System through portal", url = "http://portal.kfupm.edu.sa" }
        ]
        |> Element.layout []


view : Model -> Html Msg
view model =
    viewMessage


white : Element.Color
white =
    rgb255 255 255 255


black : Element.Color
black =
    rgb255 0 0 0


purple : Element.Color
purple =
    rgb255 240 120 245


blue : Element.Color
blue =
    rgb255 0 125 244


viewPage : Model -> Element Msg
viewPage { page, chapters, score, psp } =
    let
        title =
            case page of
                MATH101 ->
                    "MATH101"

                MATH102 ->
                    "MATH102"

                MATH201 ->
                    "MATH201"
    in
    column [ spacing 25 ]
        [ el [ centerX, paddingXY 0 20, Border.widthEach { bottom = 10, left = 0, right = 0, top = 0 }, width fill ] (text title)
        , showTable chapters
        , el [ centerX, paddingXY 0 20, Border.widthEach { bottom = 10, left = 0, right = 0, top = 0 }, width fill ] myButton
        , el [ centerX, paddingXY 0 20, Border.widthEach { bottom = 10, left = 0, right = 0, top = 0 }, width fill ]
            (text <| "Your score: " ++ String.padLeft 10 ' ' (fromInt score))
        , el [ centerX, paddingXY 0 1, width fill, Font.color blue ]
            (text <| "Out of " ++ String.padLeft 3 ' ' (fromInt psp))
        ]


myButton : Element Msg
myButton =
    Input.button
        [ Background.color blue
        , centerX
        , Font.color white
        , Border.rounded 20
        , width fill
        , padding 20
        , Element.focused
            [ Background.color purple, Font.color black ]
        ]
        { onPress = Just Calculate
        , label = text "Calculate"
        }


showTable : List Chapter -> Element Msg
showTable chapters =
    Element.table [ width (Element.px 600), alignLeft, spacing 25, Border.width 1, padding 10 ]
        { data = chapters
        , columns =
            [ { header = el [ alignRight ] (Element.text "Chapter")
              , width = Element.fillPortion 3
              , view =
                    \ch ->
                        el [] (text <| fromInt ch.title)
              }
            , { header = el [ alignRight ] (Element.text "Maximum score")
              , width = fill
              , view =
                    \ch ->
                        Element.text <| fromInt ch.maxScore
              }
            , { header = el [ alignRight ] (Element.text "Your score")
              , width = fill
              , view =
                    \ch ->
                        el [] (Input.text [] { onChange = ScoreChange ch, text = fromInt ch.score, placeholder = Nothing, label = Input.labelLeft [] (text "") })
              }
            ]
        }



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
