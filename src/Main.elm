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
import Time
import Iso8601 exposing (toTime)
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
    { zone : Time.Zone
    , time : Time.Posix
    , counters : List Counter
    }

-- given a time span in seconds, return a UnitVals dictionary
hms = [ { name = "s", suffix = "", div = 60, pad = 2 }
      , { name = "m", suffix = ":", div = 60, pad = 2 }
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
                (modBy unit.div remaining)
                (secsToUnits units (remaining // unit.div))
-- given an Event and the current time, return a Counter with everything computed
eventToCounter : Time.Posix -> Time.Zone -> Event -> Counter
eventToCounter now zone event =
    let
        year = Time.toYear zone now
        timeFinal = case Iso8601.toTime ((String.fromInt year) ++ event.isoSuffix) of
                        Err _ -> now
                        Ok posix1 ->
                            if ((Time.posixToMillis posix1) >= (Time.posixToMillis now)) then -- in the future using the current year
                                posix1
                            else -- in the past using the current year, use the next year
                                Iso8601.toTime ((String.fromInt (year+1)) ++ event.isoSuffix)
                                    |> Result.withDefault now
        timeSpan = (Time.posixToMillis timeFinal) - (Time.posixToMillis now)
        unitVals = secsToUnits hms (timeSpan//1000)
    in
        { name = event.name
        , timeSpan = timeSpan
        , unitVals = unitVals
        }

makeCounters : Time.Posix -> Time.Zone -> List Counter
makeCounters now zone =
    List.map (eventToCounter now zone) [ { name = "Christmas Day"
                                         , isoSuffix = "-12-25"
                                         , duration = 24*60*60
                                         }
                                       , { name = "New Year's Day"
                                         , isoSuffix = "-01-01"
                                         , duration = 24*60*60
                                         }
                                       , { name = "Remembrance Day"
                                         , isoSuffix = "-11-11T11:11:00"
                                         , duration = 24*60*60
                                         }
                                       , { name = "National Day for Truth and Reconciliation"
                                         , isoSuffix = "-09-30"
                                         , duration = 24*60*60
                                         }
                                       , { name = "Halloween"
                                         , isoSuffix = "-10-31"
                                         , duration = 24*60*60
                                         }
                                       , { name = "Valentine's Day"
                                         , isoSuffix = "-02-14"
                                         , duration = 24*60*60
                                         }
                                       , { name = "Canada Day"
                                         , isoSuffix = "-07-01"
                                         , duration = 24*60*60
                                         }
                                       ]

init : () -> (Model, Cmd Msg)
init _ =
    let
        zone =
            Time.utc
        now =
            Time.millisToPosix 0
    in
        ( Model zone now (makeCounters now zone |> List.sortBy .timeSpan)
        , Task.perform AdjustTimeZone Time.here
        )

    
-- UPDATE
type Msg
  = Tick Time.Posix
  | AdjustTimeZone Time.Zone

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
                          \ctr -> text (viewNum 2 ":" (Dict.get "m" ctr.unitVals))
                      }
                    , { header = text "S"
                      , width = shrink
                      , view =
                          \ctr -> text (viewNum 2 "" (Dict.get "s" ctr.unitVals))
                      }
                    ]
                }
            ])

viewNum : Int -> String -> Maybe Int -> String
viewNum pad suffix val =
    (val |> withDefault 0 |> String.fromInt |> String.pad pad '0') ++ suffix
