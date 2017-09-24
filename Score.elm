module Score exposing (Score, postScore, viewScore)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, field, succeed)
import Json.Encode as Encode
import Settings


type alias Score =
    { id : Int
    , name : String
    , score : Int
    }



-- COMMANDS
-- postScore : String -> Model -> Cmd msg


postScore : Encode.Value -> (Result Http.Error Score -> msg) -> Cmd msg
postScore encodedModel msg =
    let
        url =
            Settings.apiUrlPrefix ++ "/scores"

        body =
            encodedModel
                |> Http.jsonBody

        request =
            Http.post url body scoreDecoder
    in
    Http.send msg request



-- DECODERS


scoreDecoder : Decoder Score
scoreDecoder =
    Decode.map3 Score
        (field "id" Decode.int)
        (field "name" Decode.string)
        (field "score" Decode.int)



-- VIEW


viewScore : Int -> Html msg
viewScore sum =
    div
        [ class "score" ]
        [ span [ class "label" ] [ text "Score" ]
        , span [ class "value" ] [ text (toString sum) ]
        ]
