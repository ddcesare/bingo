module Bingo exposing (..)

import Entry
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Encode as Encode
import Score
import Settings
import ViewHelpers exposing (..)


-- MODEL


type GameState
    = EnteringName
    | Playing


type alias Model =
    { name : String
    , gameNumber : Int
    , entries : List Entry.Entry
    , alertMessage : Maybe String
    , nameInput : String
    , gameState : GameState
    }


initialModel : Model
initialModel =
    { name = "Anonymous"
    , gameNumber = 1
    , entries = []
    , alertMessage = Nothing
    , nameInput = ""
    , gameState = EnteringName
    }



-- UPDATE


type Msg
    = NewGame
    | Mark Int
    | NewEntries (Result Http.Error (List Entry.Entry))
    | CloseAlert
    | ShareScore
    | NewScore (Result Http.Error Score.Score)
    | SetNameInput String
    | SaveName
    | CancelName
    | ChangeGameState GameState


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeGameState state ->
            ( { model | gameState = state }, Cmd.none )

        SetNameInput value ->
            ( { model | nameInput = value }, Cmd.none )

        SaveName ->
            ( { model
                | name = model.nameInput
                , nameInput = ""
                , gameState = Playing
              }
            , Cmd.none
            )

        CancelName ->
            ( { model | nameInput = "", gameState = Playing }, Cmd.none )

        NewGame ->
            ( { model
                | gameNumber = model.gameNumber + 1
              }
            , getEntries
            )

        ShareScore ->
            ( model, Score.postScore (encodeScore model) NewScore )

        NewScore result ->
            case result of
                Ok score ->
                    let
                        message =
                            "Your score of " ++ toString score.score ++ " was successfully shared!"
                    in
                    ( { model | alertMessage = Just message }, Cmd.none )

                Err error ->
                    ( { model | alertMessage = Just (httpErrorToMessage error) }, Cmd.none )

        NewEntries result ->
            case result of
                Ok randomEntries ->
                    let
                        _ =
                            Debug.log "It worked!" randomEntries
                    in
                    ( { model | entries = sortEntries randomEntries }, Cmd.none )

                Err error ->
                    ( { model | alertMessage = Just (httpErrorToMessage error) }, Cmd.none )

        CloseAlert ->
            ( { model | alertMessage = Nothing }, Cmd.none )

        Mark id ->
            ( { model | entries = Entry.markEntryWithId model.entries id }, Cmd.none )


sortEntries : List Entry.Entry -> List Entry.Entry
sortEntries entries =
    List.sortBy .points entries


httpErrorToMessage : Http.Error -> String
httpErrorToMessage error =
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



-- COMMANDS


getEntries : Cmd Msg
getEntries =
    Entry.getEntries NewEntries Settings.apiUrlPrefix



-- DECODERS/ENCODERS


encodeScore : Model -> Encode.Value
encodeScore model =
    Encode.object
        [ ( "name", Encode.string model.name )
        , ( "score", Encode.int (Entry.sumMarkedPoints model.entries) )
        ]



-- VIEW


hasZeroScore : Model -> Bool
hasZeroScore model =
    Entry.sumMarkedPoints model.entries == 0


viewPlayer : String -> Int -> Html Msg
viewPlayer name gameNumber =
    h2 [ id "info", class "classy" ]
        [ a [ href "#", onClick (ChangeGameState EnteringName) ] [ text name ]
        , text (" - Game #" ++ toString gameNumber)
        ]


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


validateNameInput : Model -> Bool
validateNameInput model =
    String.isEmpty model.nameInput


viewNameInput : Model -> Html Msg
viewNameInput model =
    case model.gameState of
        EnteringName ->
            div [ class "name-input" ]
                [ input
                    [ type_ "text"
                    , value model.nameInput
                    , placeholder "Who's playing?"
                    , autofocus True
                    , onInput SetNameInput
                    ]
                    []
                , button [ disabled (validateNameInput model), onClick SaveName ] [ text "Save" ]
                , primaryButton CancelName "Cancel"
                ]

        Playing ->
            text ""


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ viewHeader "BUZZWORD BINGO"
        , viewPlayer model.name model.gameNumber
        , viewNameInput model
        , alert CloseAlert model.alertMessage
        , Entry.viewEntryList Mark model.entries
        , Score.viewScore (Entry.sumMarkedPoints model.entries)
        , div [ class "button-group" ]
            [ primaryButton NewGame "New Game"
            , primaryButton ShareScore "Share Score"
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
