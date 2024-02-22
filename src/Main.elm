-- Show the current time in your time zone.
--
-- Read how it works:
--   https://guide.elm-lang.org/effects/time.html
--
-- For an analog clock, check out this SVG example:
--   https://elm-lang.org/examples/clock
--

module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Task
import Time
import Iso8601 exposing (toTime)
import Debug


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
    }
type alias Unit =
    { suffix : String
    , div : Int
    , pad : Int
    }
type alias Model =
    { zone : Time.Zone
    , time : Time.Posix
    , counters : List Counter
    }

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
                                case Iso8601.toTime ((String.fromInt (year+1)) ++ event.isoSuffix) of
                                    Err _ -> now
                                    Ok posix2 -> posix2
    in
        { name = event.name
        , timeSpan = (Time.posixToMillis timeFinal) - (Time.posixToMillis now)
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
        ( Model zone now (List.sortBy .timeSpan (makeCounters now zone))
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
      ( { model | time = newTime, counters = List.sortBy .timeSpan (makeCounters newTime model.zone) }
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
  let
    hour   = String.padLeft 2 '0' (String.fromInt (Time.toHour   model.zone model.time))
    minute = String.padLeft 2 '0' (String.fromInt (Time.toMinute model.zone model.time))
    second = String.padLeft 2 '0' (String.fromInt (Time.toSecond model.zone model.time))
  in
      div [] [ h1 [] [ text (hour ++ ":" ++ minute ++ ":" ++ second) ]
             , div [] (List.map viewCounter model.counters)
             ]

secsToSpan : Int -> List Unit -> List (Html Msg)
secsToSpan remaining unitList =
    case unitList of
        [] ->
            []
        unit :: units ->
            span [ ] [ text ((String.pad unit.pad '0' (String.fromInt (modBy unit.div remaining))) ++ unit.suffix) ] :: secsToSpan (remaining // unit.div) units

hms = [ { suffix = "", div = 60, pad = 2 }
      , { suffix = ":", div = 60, pad = 2 }
      , { suffix = ":", div = 24, pad = 2 }
      , { suffix = "d ", div = 1000000, pad = 3 }
      ]
viewCounter : Counter -> Html Msg
viewCounter counter =
    div [] [ (span [ style "color" "green" ] [ text (counter.name ++ " ") ])
           , (span [ style "color" "blue" ] (List.reverse (secsToSpan (counter.timeSpan//1000) hms)) )
           ]
