module Entry
    exposing
        ( Entry
        , getEntries
        , markEntryWithId
        , sumMarkedPoints
        , viewEntryList
        )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, field, succeed)


type alias Entry =
    { id : Int
    , phrase : String
    , points : Int
    , marked : Bool
    }


markEntryWithId : List Entry -> Int -> List Entry
markEntryWithId entries id =
    let
        markEntry e =
            if e.id == id then
                { e | marked = not e.marked }
            else
                e
    in
    List.map markEntry entries



-- COMMANDS


getEntries : (Result Http.Error (List Entry) -> msg) -> String -> Cmd msg
getEntries msg url =
    Decode.list entryDecoder
        |> Http.get (url ++ "/random-entries")
        |> Http.send msg



-- DECODERS


entryDecoder : Decoder Entry
entryDecoder =
    Decode.map4 Entry
        (field "id" Decode.int)
        (field "phrase" Decode.string)
        (field "points" Decode.int)
        (succeed False)



-- VIEW


viewEntryItem : (Int -> msg) -> Entry -> Html msg
viewEntryItem msg entry =
    li [ classList [ ( "marked", entry.marked ) ], onClick (msg entry.id) ]
        [ span [ class "phrase" ] [ text entry.phrase ]
        , span [ class "point" ] [ text (toString entry.points) ]
        ]


viewEntryList : (Int -> msg) -> List Entry -> Html msg
viewEntryList msg entries =
    entries
        |> List.map (viewEntryItem msg)
        |> ul []


sumMarkedPoints : List Entry -> Int
sumMarkedPoints entries =
    entries
        |> List.filter .marked
        |> List.foldl (\e sum -> sum + e.points) 0
