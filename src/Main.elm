
module Main exposing (..)

import Maybe exposing (..)
import Browser
import Html exposing (Html)
--import Html.Attributes
import Dict exposing (Dict)
import Task
import Time exposing (Zone, Posix, toYear, toWeekday, toMonth
                     , toDay, toHour, toMinute, Weekday(..), Month(..)
                     , posixToMillis, millisToPosix, utc)
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
-- a date finder function takes a year and a time zone and returns a date in that year
type alias DateFinder = (Int -> Zone -> Posix)
type alias Event =
    { name : String
    , nextFinder : DateFinder
    }
type alias NextEvent =
    { name : String
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

hms = [ { name = "s", suffix = "", div = 60, pad = 2 }
      , { name = "m", suffix = ":", div = 60, pad = 2 }
      , { name = "h", suffix = ":", div = 24, pad = 2 }
      , { name = "d", suffix = "d ", div = 1000000, pad = 3 }
      ]

-- some "constants"
day = 86400 * 1000

-- identity date finder function; replaces only the given year and returns the exact datetime
exactDate : Parts -> Int -> Zone -> Posix
exactDate parts year zone =
    partsToPosix zone { parts | year = year }

-- function that finds the nth weekday of whatever month is in the given Parts
nthWeekdayOffset : Int -> Int -> Weekday -> Parts -> Int -> Zone -> Posix
nthWeekdayOffset offset n weekday parts year zone =
    let
        tryPosix = (partsToPosix zone { parts | year = year, day = 1 + (n-1)*7 } |> posixToMillis) + offset * day
                 |> millisToPosix
    in
        if (toWeekday zone tryPosix) == weekday then
            tryPosix
        else
            nthWeekdayOffset (offset+1) n weekday parts year zone -- inefficient but simple and we'll never do more than 6

nthWeekday = nthWeekdayOffset 0

nthWeekdayPlusDays : Int -> Int -> Weekday -> Parts -> Int -> Zone -> Posix
nthWeekdayPlusDays addDays n weekday parts year zone =
    (nthWeekday n weekday parts year zone |> posixToMillis) + addDays * day |> millisToPosix

-- function that finds the nth day preceding a given date that is a particular weekday, e.g. 1st Monday
-- preceding May 25
nthPreceding : Int -> Weekday -> Parts -> Int -> Zone -> Posix
nthPreceding n weekday parts year zone =
    let
        baseDate = partsToPosix zone { parts | year = year }
        precedingDay = (posixToMillis baseDate) - (day + (n-1)*day*7) |> millisToPosix
    in
        nthPrecedingReal precedingDay weekday parts year zone
nthPrecedingReal : Posix -> Weekday -> Parts -> Int -> Zone -> Posix
nthPrecedingReal posix weekday parts year zone =
    if (toWeekday zone posix) == weekday then
        posix
    else
        nthPrecedingReal ((posixToMillis posix) - day |> millisToPosix) weekday parts year zone

-- given an Event and the current time, return a NextEvent with everything computed
eventToNextEvent : Posix -> Zone -> Event -> NextEvent
eventToNextEvent now zone event =
    let
        year = toYear zone now
        posixThisYear = event.nextFinder year zone
        thisYearMillis = posixToMillis posixThisYear
        nowMillis = posixToMillis now
        eventTime = if thisYearMillis >= nowMillis then
                        -- the event this year is in the future
                        posixThisYear
                    else
                        -- the event this year is in the past, find it for next year
                        event.nextFinder (year+1) zone
    in
        { name = event.name
        , eventTime = eventTime
        }

baseParts = { year = 0, month = Jan, day = 0, hour = 0, minute = 0, second = 0, millisecond = 0 }
events = [ { name = "Christmas Day"
          , nextFinder = exactDate { baseParts | month = Dec, day = 25 }
          }
        , { name = "New Year's Day"
          , nextFinder = exactDate { baseParts | month = Jan, day = 1 }
          }
        , { name = "Remembrance Day 11th hour"
          , nextFinder = exactDate { baseParts | month = Nov, day = 11, hour = 11 }
          }
        , { name = "Orange Shirt Day (T&R)"
          , nextFinder = exactDate { baseParts | month = Sep, day = 30 }
          }
        , { name = "Halloween"
          , nextFinder = exactDate { baseParts | month = Oct, day = 31 }
          }
        , { name = "Valentine's Day"
          , nextFinder = exactDate { baseParts | month = Feb, day = 14 }
          }
        , { name = "Canada Day"
          , nextFinder = exactDate { baseParts | month = Jul, day = 1 }
          }
        , { name = "Labour Day"
          , nextFinder = nthWeekday 1 Mon { baseParts | month = Sep }
          }
        , { name = "Family Day"
          , nextFinder = nthWeekday 3 Mon { baseParts | month = Feb }
          }
        , { name = "Civic Holiday"
          , nextFinder = nthWeekday 1 Mon { baseParts | month = Aug }
          }
        , { name = "Thanksgiving"
          , nextFinder = nthWeekday 2 Mon { baseParts | month = Oct }
          }
        , { name = "Black Friday"
          , nextFinder = nthWeekdayPlusDays 1 4 Thu { baseParts | month = Nov }
          }
        , { name = "Victoria Day"
          , nextFinder = nthPreceding 1 Mon { baseParts | month = May, day = 25 }
          }
        -- , { name = "Test Day"
        --   , nextFinder = nthPreceding 1 Sun { baseParts | month = Feb, day = 27, hour = 19, minute = 1 }
        --   }
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
        ( Model zone epoch []
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
             (List.map (viewNextEvent model.zone model.time) model.nextEvents)
        )

viewNextEvent : Zone -> Posix -> NextEvent -> Element Msg
viewNextEvent zone time nextEvent =
    let
        evTime = nextEvent.eventTime
        parts = secsToParts hms ((subtractPosix evTime time) // 1000)
    in
        column [ width fill, centerX, padding 2 ]
            [ el [ width fill
                 , padding 2
                 , Font.size 28
                 , Font.regular
                 , Font.center
                 ]
                  (text nextEvent.name)
            , el [ width fill
                 , padding 2
                 , Font.size 14
                 , Font.center
                 , Font.italic
                 ]
                  (text ((viewNum 0 "-" (toYear zone evTime)
                              ++ (viewMonth 0 "-" (toMonth zone evTime))
                              ++ (viewNum 2 " " (toDay zone evTime))
                              ++ (viewNum 2 ":" (toHour zone evTime))
                              ++ (viewNum 2 "" (toMinute zone evTime))
                         )
                        )
                  )
            , el [ width fill
                 , Font.size 24
                 , Font.bold
                 , Font.center
                 , Font.variantList [ Font.tabularNumbers ]
                 ]
                  (text ((maybeViewNum 3 "d " (Dict.get "d" parts))
                             ++ (maybeViewNum 2 ":" (Dict.get "h" parts))
                             ++ (maybeViewNum 2 ":" (Dict.get "m" parts))
                             ++ (maybeViewNum 2 "" (Dict.get "s" parts))
                        )
                  )
            ]

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


maybeViewNum : Int -> String -> Maybe Int -> String
maybeViewNum pad suffix val =
    viewNum pad suffix (withDefault 0 val)

viewNum : Int -> String -> Int -> String
viewNum pad suffix val =
    ((String.fromInt val) |> String.pad pad '0') ++ suffix

viewMonth : Int -> String -> Month -> String
viewMonth pad suffix val =
    ((case val of
          Jan -> "January"
          Feb -> "February"
          Mar -> "March"
          Apr -> "April"
          May -> "May"
          Jun -> "June"
          Jul -> "July"
          Aug -> "August"
          Sep -> "September"
          Oct -> "October"
          Nov -> "November"
          Dec -> "December"
     ) |> String.pad pad ' ') ++ suffix
