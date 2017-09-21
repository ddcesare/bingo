module Bingo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http


-- MODEL


type alias Model =
    { name : String
    , gameNumber : Int
    , entries : List Entry
    }


type alias Entry =
    { id : Int
    , phrase : String
    , points : Int
    , marked : Bool
    }


initialModel : Model
initialModel =
    { name = "dd"
    , gameNumber = 1
    , entries = []
    }



-- UPDATE


type Msg
    = NewGame
    | Mark Int
    | SortEntry
    | NewEntries (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ->
            ( { model
                | gameNumber = model.gameNumber + 1
              }
            , getEntries
            )

        NewEntries result ->
            case result of
                Ok jsonString ->
                    let
                        _ =
                            Debug.log "It worked!" jsonString
                    in
                    ( model, Cmd.none )

                Err error ->
                    let
                        _ =
                            Debug.log "Oops!" error
                    in
                    ( model, Cmd.none )

        Mark id ->
            let
                markEntry e =
                    if e.id == id then
                        { e | marked = not e.marked }
                    else
                        e
            in
            ( { model | entries = List.map markEntry model.entries }, Cmd.none )

        SortEntry ->
            ( { model | entries = List.sortBy .id model.entries }, Cmd.none )



-- COMMANDS


entriesUrl : String
entriesUrl =
    "http://localhost:3000/random-entries"


getEntries : Cmd Msg
getEntries =
    entriesUrl
        |> Http.getString
        |> Http.send NewEntries



-- VIEW


playerInfo : String -> Int -> String
playerInfo name gameNumber =
    name ++ " - Game #" ++ toString gameNumber


viewPlayer : String -> Int -> Html Msg
viewPlayer name gameNumber =
    let
        playerInfoText =
            playerInfo name gameNumber
                |> String.toUpper
                |> text
    in
    h2 [ id "info", class "classy" ] [ playerInfoText ]


viewHeader : String -> Html Msg
viewHeader title =
    header []
        [ h1 [] [ text title ] ]


viewFooter : Html Msg
viewFooter =
    footer []
        [ a [ href "http://elm-lang.org" ]
            [ text "Powered By Elm" ]
        ]


viewEntryItem : Entry -> Html Msg
viewEntryItem entry =
    li [ classList [ ( "marked", entry.marked ) ], onClick (Mark entry.id) ]
        [ span [ class "phrase" ] [ text entry.phrase ]
        , span [ class "point" ] [ text (toString entry.points) ]
        ]


viewEntryList : List Entry -> Html Msg
viewEntryList entries =
    entries
        |> List.map viewEntryItem
        |> ul []


sumMarkedPoints : List Entry -> Int
sumMarkedPoints entries =
    entries
        |> List.filter .marked
        |> List.foldl (\e sum -> sum + e.points) 0


viewScore : Int -> Html Msg
viewScore sum =
    div
        [ class "score" ]
        [ span [ class "label" ] [ text "Score" ]
        , span [ class "value" ] [ text (toString sum) ]
        ]


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ viewHeader "BUZZWORD BINGO"
        , viewPlayer model.name model.gameNumber
        , viewEntryList model.entries
        , viewScore (sumMarkedPoints model.entries)
        , div [ class "button-group" ]
            [ button [ onClick NewGame ] [ text "New Game" ] ]
        , div [ class "button-group" ]
            [ button [ onClick SortEntry ] [ text "Sort Entries" ] ]
        , div [ class "debug" ] [ text (toString model) ]
        , viewFooter
        ]


init : ( Model, Cmd Msg )
init =
    ( initialModel, getEntries )


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
