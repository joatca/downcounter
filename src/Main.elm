
module Main exposing (..)

import Maybe exposing (..)
import Browser
import Html exposing (Html)
--import Html.Attributes
import Dict exposing (Dict)
import Task
import Time exposing (Zone, Posix, toYear, Month(..), posixToMillis, millisToPosix, utc)
import Time.Extra exposing (Parts, partsToPosix)
import Debug
import Element exposing (Element, el, text, column, table,
                             fill, shrink, width, rgb255, spacing, centerX, padding)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font

-- MAIN
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

      
-- MODEL
type alias Event =
    { name : String
    , parts : Parts -- for an event of non-zero duration, this is the *end*
    , duration : Int -- duration in milliseconds
    }
type Relation
    = Before
    | During
type alias NextEvent =
    { name : String
    --, relation : Relation
    , eventTime : Posix
    }
type alias Unit =
    { suffix : String
    , name : String
    , div : Int
    , pad : Int
    }
type alias UnitVals = Dict String Int
type alias Model =
    { zone : Zone
    , time : Posix
    , nextEvents : List NextEvent
    }

-- given a time span in seconds, return a UnitVals dictionary
hms = [ { name = "s", suffix = "", div = 60, pad = 2 }
      , { name = "m", suffix = ":", div = 60, pad = 2 }
      , { name = "h", suffix = ":", div = 24, pad = 2 }
      , { name = "d", suffix = "d ", div = 1000000, pad = 3 }
      ]

-- given an Event and the current time, return a NextEvent with everything computed
eventToNextEvent : Posix -> Zone -> Event -> NextEvent
eventToNextEvent now zone event =
    let
        year = toYear zone now
        parts = event.parts
        posixThisYear = partsToPosix zone { parts | year = year }
        thisYearMillis = posixToMillis posixThisYear
        nowMillis = posixToMillis now
        eventTime = if thisYearMillis >= nowMillis then
                        -- the event this year is in the future
                        posixThisYear
                    else
                        -- the event this year is in the past, find it for next year
                        partsToPosix zone { parts | year = year + 1 }
    in
        { name = event.name
        , eventTime = eventTime
        }

baseParts = { year = 0, month = Jan, day = 1, hour = 0, minute = 0, second = 0, millisecond = 0 }
events = [ { name = "Christmas Day"
          , parts = { baseParts | month = Dec, day = 25 }
          , duration = 24*60*60
          }
        , { name = "New Year's Day"
          , parts = { baseParts | month = Jan, day = 1 }
          , duration = 24*60*60
          }
        , { name = "Remembrance Day"
          , parts = { baseParts | month = Nov, day = 11, hour = 11, minute = 11 }
          , duration = 24*60*60
          }
        , { name = "National Day for Truth and Reconciliation"
          , parts = { baseParts | month = Sep, day = 30 }
          , duration = 24*60*60
          }
        , { name = "Halloween"
          , parts = { baseParts | month = Oct, day = 31 }
          , duration = 24*60*60
          }
        , { name = "Valentine's Day"
          , parts = { baseParts | month = Feb, day = 14 }
          , duration = 24*60*60
          }
        , { name = "Canada Day"
          , parts = { baseParts | month = Jul, day = 1 }
          , duration = 24*60*60
          }
        , { name = "Test Day"
          , parts = { baseParts | month = Feb, day = 24, hour = 22, minute = 28 }
          , duration = 24*60*60
          }
        ]

makeNextEvents : Posix -> Zone -> List NextEvent
makeNextEvents now zone =
    List.map (eventToNextEvent now zone) events

init : () -> (Model, Cmd Msg)
init _ =
    let
        zone =
            utc
        epoch =
            millisToPosix 0
    in
        ( Model zone epoch (makeNextEvents epoch zone |> List.sortWith compareNextEvent)
        , Task.perform AdjustTimeZone Time.here
        )

compareNextEvent : NextEvent -> NextEvent -> Order
compareNextEvent a b =
    compare (posixToMillis a.eventTime) (posixToMillis b.eventTime)

    
-- UPDATE
type Msg
  = Tick Posix
  | AdjustTimeZone Zone

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
      Tick newTime ->
          let
              firstEvent = List.head model.nextEvents
              recompute = case firstEvent of
                              Nothing ->
                                  True
                              Just first ->
                                  (posixToMillis first.eventTime) < (posixToMillis newTime)
          in
              if recompute then
                  ( { model | time = newTime, nextEvents = makeNextEvents newTime model.zone |> List.sortWith compareNextEvent }
                  , Cmd.none
                  )
              else
                  ( { model | time = newTime }
                  , Cmd.none
                  )
              
      AdjustTimeZone newZone ->
          ( { model | zone = newZone }
          , Cmd.none
          )

        
-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 1000 Tick

      
-- VIEW
view : Model -> Html Msg
view model =
    Element.layout []
        (column [ width fill, centerX ]
             (List.map (viewNextEvent model.time) model.nextEvents)
        )

viewNextEvent : Posix -> NextEvent -> Element Msg
viewNextEvent time nextEvent =
    let
        parts = secsToParts hms ((subtractPosix nextEvent.eventTime time) // 1000)
    in
        text (nextEvent.name ++ " " ++ (viewNum 3 "d " (Dict.get "d" parts))
                  ++ (viewNum 2 ":" (Dict.get "h" parts))
                  ++ (viewNum 2 ":" (Dict.get "m" parts))
                  ++ (viewNum 2 "" (Dict.get "s" parts))
             )

subtractPosix : Posix -> Posix -> Int
subtractPosix a b =
    (posixToMillis a) - (posixToMillis b)
                                                       
secsToParts : List Unit -> Int -> UnitVals
secsToParts unitList remaining =
    case unitList of
        [] ->
            Dict.empty
        unit :: units ->
            Dict.insert unit.name
                -- don't take the modulus if this is the last one
                (if List.isEmpty units then remaining else modBy unit.div remaining)
                (secsToParts units (remaining // unit.div))
viewNum : Int -> String -> Maybe Int -> String
viewNum pad suffix val =
    (val |> withDefault 0 |> String.fromInt |> String.pad pad '0') ++ suffix
