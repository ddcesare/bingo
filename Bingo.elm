module Bingo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, field, succeed)
import Json.Encode as Encode


-- MODEL


type alias Model =
    { name : String
    , gameNumber : Int
    , entries : List Entry
    , alertMessage : Maybe String
    }


type alias Entry =
    { id : Int
    , phrase : String
    , points : Int
    , marked : Bool
    }


type alias Score =
    { id : Int
    , name : String
    , score : Int
    }


initialModel : Model
initialModel =
    { name = "dd"
    , gameNumber = 1
    , entries = []
    , alertMessage = Nothing
    }



-- UPDATE


type Msg
    = NewGame
    | Mark Int
    | NewEntries (Result Http.Error (List Entry))
    | CloseAlert
    | ShareScore
    | NewScore (Result Http.Error Score)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ->
            ( { model
                | gameNumber = model.gameNumber + 1
              }
            , getEntries
            )

        ShareScore ->
            ( model, postScore model )

        NewScore result ->
            case result of
                Ok score ->
                    let
                        message =
                            "Your score of " ++ toString score.score ++ " was successfully shared!"
                    in
                    ( { model | alertMessage = Just message }, Cmd.none )

                Err error ->
                    let
                        message =
                            "Error posting your score: " ++ toString error
                    in
                    ( { model | alertMessage = Just message }, Cmd.none )

        NewEntries result ->
            case result of
                Ok randomEntries ->
                    let
                        _ =
                            Debug.log "It worked!" randomEntries
                    in
                    ( { model | entries = sortEntries randomEntries }, Cmd.none )

                Err error ->
                    let
                        errorMessage =
                            case error of
                                Http.NetworkError ->
                                    "Is the server running?"

                                Http.Timeout ->
                                    "Request timed out!"

                                Http.BadUrl url ->
                                    "Invalid URL: " ++ url

                                Http.BadStatus response ->
                                    case response.status.code of
                                        401 ->
                                            "Unauthorized"

                                        404 ->
                                            "Not Found"

                                        code ->
                                            toString code

                                Http.BadPayload reason response ->
                                    reason
                    in
                    ( { model | alertMessage = Just (toString errorMessage) }, Cmd.none )

        CloseAlert ->
            ( { model | alertMessage = Nothing }, Cmd.none )

        Mark id ->
            let
                markEntry e =
                    if e.id == id then
                        { e | marked = not e.marked }
                    else
                        e
            in
            ( { model | entries = List.map markEntry model.entries }, Cmd.none )


sortEntries : List Entry -> List Entry
sortEntries entries =
    List.sortBy .points entries



-- COMMANDS


entriesUrl : String
entriesUrl =
    "http://localhost:3000/random-entries"


entryListDecoder : Decoder (List Entry)
entryListDecoder =
    Decode.list entryDecoder


getEntries : Cmd Msg
getEntries =
    entryListDecoder
        |> Http.get entriesUrl
        |> Http.send NewEntries


postScore : Model -> Cmd Msg
postScore model =
    let
        url =
            "http://localhost:3000/scores"

        body =
            encodeScore model
                |> Http.jsonBody

        request =
            Http.post url body scoreDecoder
    in
    Http.send NewScore request



-- DECODERS/ENCODERS


entryDecoder : Decoder Entry
entryDecoder =
    Decode.map4 Entry
        (field "id" Decode.int)
        (field "phrase" Decode.string)
        (field "points" Decode.int)
        (succeed False)


encodeScore : Model -> Encode.Value
encodeScore model =
    Encode.object
        [ ( "name", Encode.string model.name )
        , ( "score", Encode.int (sumMarkedPoints model.entries) )
        ]


scoreDecoder : Decoder Score
scoreDecoder =
    Decode.map3 Score
        (field "id" Decode.int)
        (field "name" Decode.string)
        (field "score" Decode.int)



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


viewAlertMessage : Maybe String -> Html Msg
viewAlertMessage alertMessage =
    case alertMessage of
        Just message ->
            div [ class "alert" ]
                [ span [ class "close", onClick CloseAlert ] [ text "X" ]
                , text message
                ]

        Nothing ->
            text ""


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ viewHeader "BUZZWORD BINGO"
        , viewPlayer model.name model.gameNumber
        , viewAlertMessage model.alertMessage
        , viewEntryList model.entries
        , viewScore (sumMarkedPoints model.entries)
        , div [ class "button-group" ]
            [ button [ onClick NewGame ] [ text "New Game" ]
            , button [ onClick ShareScore ] [ text "Share Score" ]
            ]
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
