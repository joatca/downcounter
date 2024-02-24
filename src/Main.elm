-- Show the current time in your time zone.
--
-- Read how it works:
--   https://guide.elm-lang.org/effects/time.html
--
-- For an analog clock, check out this SVG example:
--   https://elm-lang.org/examples/clock
--

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
    , isoSuffix : String -- for an event of non-zero duration, this is the *end*
    , parts : Parts -- for an event of non-zero duration, this is the *end*
    , duration : Int -- duration in milliseconds
    }
type Relation
    = Before
    | During
type alias Counter =
    { name : String
    --, relation : Relation
    , timeSpan : Int
    , unitVals : UnitVals
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
    , counters : List Counter
    }

-- given a time span in seconds, return a UnitVals dictionary
hms = [ { name = "m", suffix = ":", div = 60, pad = 2 }
      , { name = "h", suffix = ":", div = 24, pad = 2 }
      , { name = "d", suffix = "d ", div = 1000000, pad = 3 }
      ]
secsToUnits : List Unit -> Int -> UnitVals
secsToUnits unitList remaining =
    case unitList of
        [] ->
            Dict.empty
        unit :: units ->
            Dict.insert unit.name
                -- don't take the modulus if this is the last one
                (if List.isEmpty units then remaining else modBy unit.div remaining)
                (secsToUnits units (remaining // unit.div))
-- given an Event and the current time, return a Counter with everything computed
eventToCounter : Posix -> Zone -> Event -> Counter
eventToCounter now zone event =
    let
        year = toYear zone now
        parts = event.parts
        posixThisYear = partsToPosix zone { parts | year = year }
        timeFinal = if (posixToMillis posixThisYear) >= (posixToMillis now) then
                        -- the event this year is in the future
                        posixThisYear
                    else
                        -- the event this year is in the past, find it for next year
                        partsToPosix zone { parts | year = year + 1 }
        timeSpan = (posixToMillis timeFinal) - (posixToMillis now)
        unitVals = secsToUnits hms (timeSpan//60000)
    in
        { name = event.name
        , timeSpan = timeSpan
        , unitVals = unitVals
        }

baseParts = { year = 0, month = Jan, day = 1, hour = 0, minute = 0, second = 0, millisecond = 0 }
events = [ { name = "Christmas Day"
          , isoSuffix = "-12-25"
          , parts = { baseParts | month = Dec, day = 25 }
          , duration = 24*60*60
          }
        , { name = "New Year's Day"
          , isoSuffix = "-01-01"
          , parts = { baseParts | month = Jan, day = 1 }
          , duration = 24*60*60
          }
        , { name = "Remembrance Day"
          , isoSuffix = "-11-11T11:11:00"
          , parts = { baseParts | month = Nov, day = 11, hour = 11, minute = 11 }
          , duration = 24*60*60
          }
        , { name = "National Day for Truth and Reconciliation"
          , isoSuffix = "-09-30"
          , parts = { baseParts | month = Sep, day = 30 }
          , duration = 24*60*60
          }
        , { name = "Halloween"
          , isoSuffix = "-10-31"
          , parts = { baseParts | month = Oct, day = 31 }
          , duration = 24*60*60
          }
        , { name = "Valentine's Day"
          , isoSuffix = "-02-14"
          , parts = { baseParts | month = Feb, day = 14 }
          , duration = 24*60*60
          }
        , { name = "Canada Day"
          , isoSuffix = "-07-01"
          , parts = { baseParts | month = Jul, day = 1 }
          , duration = 24*60*60
          }
        ]
makeCounters : Posix -> Zone -> List Counter
makeCounters now zone =
    List.map (eventToCounter now zone) events

init : () -> (Model, Cmd Msg)
init _ =
    let
        zone =
            utc
        now =
            millisToPosix 0
    in
        ( Model zone now (makeCounters now zone |> List.sortBy .timeSpan)
        , Task.perform AdjustTimeZone Time.here
        )

    
-- UPDATE
type Msg
  = Tick Posix
  | AdjustTimeZone Zone

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ( { model | time = newTime, counters = makeCounters newTime model.zone |> List.sortBy .timeSpan }
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
-- view model =
--       div [] [ div [] (List.map viewCounter model.counters)
--              ]
view model =
    Element.layout []
        (column [ width fill, centerX ]
            [ text "Countdown"
            , table []
                { data = model.counters
                , columns =
                    [ { header = text "Name"
                      , width = fill
                      , view =
                          \ctr -> text ctr.name
                      }
                    , { header = text "D"
                      , width = shrink
                      , view =
                          \ctr -> text (viewNum 2 "D" (Dict.get "d" ctr.unitVals))
                      }
                    , { header = text "H"
                      , width = shrink
                      , view =
                          \ctr -> text (viewNum 2 ":" (Dict.get "h" ctr.unitVals))
                      }
                    , { header = text "M"
                      , width = shrink
                      , view =
                          \ctr -> text (viewNum 2 "" (Dict.get "m" ctr.unitVals))
                      }
                    ]
                }
            ])

viewNum : Int -> String -> Maybe Int -> String
viewNum pad suffix val =
    (val |> withDefault 0 |> String.fromInt |> String.pad pad '0') ++ suffix
