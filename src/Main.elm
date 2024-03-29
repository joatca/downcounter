-- Copyright Fraser McCrossan 2024

-- This program is free software: you can redistribute it and/or modify it under the terms of the GNU General
-- Public License as published by the Free Software Foundation, either version 3 of the License, or (at your
-- option) any later version.

-- This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
-- implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details.

-- You should have received a copy of the GNU General Public License along with this program. If not, see
-- <https://www.gnu.org/licenses/>.

module Main exposing (..)

import Maybe exposing (..)
import Browser
import Browser.Events as E
import Browser.Dom as Dom
import Html exposing (Html)
import Dict exposing (Dict)
import Task
import Round
import Time exposing (Zone, Posix, toYear, toWeekday, toMonth
                     , toDay, toHour, toMinute, Weekday(..), Month(..)
                     , posixToMillis, millisToPosix, utc)
import Time.Extra exposing (Parts, partsToPosix)
import Process exposing (sleep)
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
type alias Flags =
    { width : Int
    , height : Int
    , ratio : Float
    }
type alias Model =
    { width : Int
    , height : Int
    , pixelRatio : Float
    , zone : Zone
    , time : Posix
    , nextEvents : List NextEvent
    }

dhms = [ { name = "s", suffix = "", div = 60, pad = 2 }
       , { name = "m", suffix = ":", div = 60, pad = 2 }
       , { name = "h", suffix = ":", div = 24, pad = 2 }
       , { name = "d", suffix = "d ", div = 1000000, pad = 3 }
       ]
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


-- the table below contains partial functions based on the routines that follow; each is called for the current
-- year and if it returns a time in the past then it is called for the following year
-- 
-- "exactDate { baseParts | month = Sep, day = 30 }" is the exact date September 30
-- "nthWeekday 3 Mon { baseParts | month = Feb }" is the 3rd Monday in February
-- "nthWeekdayPlusDays 1 4 Thu { baseParts | month = Nov }" is 1 day after the 4th Thursday in November
-- "nthPreceding 1 Mon { baseParts | month = May, day = 25 }" is the first Monday preceding May 25
-- "easterSunday baseParts" computes Gregorian Easter Sunday, parts are passed as a convenience

baseParts = { year = 0, month = Jan, day = 0, hour = 0, minute = 0, second = 0, millisecond = 0 }
events = [ { name = "New Year's Day"
           , nextFinder = exactDate { baseParts | month = Jan, day = 1 }
           }
         , { name = "Valentine's Day"
           , nextFinder = exactDate { baseParts | month = Feb, day = 14 }
           }
         , { name = "Family Day"
           , nextFinder = nthWeekday 3 Mon { baseParts | month = Feb }
           }
         , { name = "International Women's Day"
           , nextFinder = exactDate { baseParts | month = Mar, day = 8 }
           }
         , { name = "St. Patrick's Day"
           , nextFinder = exactDate { baseParts | month = Mar, day = 17 }
           }
         , { name = "Easter Sunday"
           , nextFinder = easterSunday baseParts
           }
         , { name = "Mother's Day"
           , nextFinder = nthWeekday 2 Sun { baseParts | month = May }
           }
         , { name = "Victoria Day"
           , nextFinder = nthPreceding 1 Mon { baseParts | month = May, day = 25 }
           }
         , { name = "Father's Day"
           , nextFinder = nthWeekday 3 Sun { baseParts | month = Jun }
           }
         , { name = "Canada Day"
           , nextFinder = exactDate { baseParts | month = Jul, day = 1 }
           }
         , { name = "Civic Holiday"
           , nextFinder = nthWeekday 1 Mon { baseParts | month = Aug }
           }
         , { name = "Labour Day"
           , nextFinder = nthWeekday 1 Mon { baseParts | month = Sep }
           }
         , { name = "Orange Shirt Day (T&R)"
           , nextFinder = exactDate { baseParts | month = Sep, day = 30 }
           }
         , { name = "Thanksgiving"
           , nextFinder = nthWeekday 2 Mon { baseParts | month = Oct }
           }
         , { name = "Halloween"
           , nextFinder = exactDate { baseParts | month = Oct, day = 31 }
           }
         , { name = "Black Friday"
           , nextFinder = nthWeekdayPlusDays 1 4 Thu { baseParts | month = Nov }
           }
         , { name = "Remembrance Day 11th hour"
           , nextFinder = exactDate { baseParts | month = Nov, day = 11, hour = 11 }
          }
         , { name = "International Men's Day"
           , nextFinder = exactDate { baseParts | month = Nov, day = 19 }
           }
         , { name = "Christmas Day"
           , nextFinder = exactDate { baseParts | month = Dec, day = 25 }
           }
         -- , { name = "Test event"
         --   , nextFinder = exactDate { baseParts | month = Mar, day = 10 }
         --   }
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

-- given a year, find Easter Sunday of that year using the Meeus/Jones/Butcher algorithm as modified by New
-- Scientist https://en.wikipedia.org/wiki/Date_of_Easter#Anonymous_Gregorian_algorithm
easterSunday : Parts -> Int -> Zone -> Posix
easterSunday parts year zone =
    let
        y = year
        a = modBy 19 y
        b = y // 100
        c = modBy 100 y
        d = b // 4
        e = modBy 4 b
        g = (8*b + 13) // 25
        h = modBy 30 (19*a + b - d - g + 15)
        i = c // 4
        k = modBy 4 c
        l = modBy 7 (32 + 2*e + 2*i - h - k)
        m = (a + 11*h +19*l) // 433
        n = (h + l - 7*m + 90) // 25
        p = modBy 32 (h + l - 7*m + 33*n + 19)
    in
        partsToPosix zone { parts | year = y, month = (numToMonth n), day = p }

-- convert an integer month to a Month

numToMonth : Int -> Month
numToMonth monthNum =
    case monthNum of
          1 -> Jan
          2 -> Feb
          3 -> Mar
          4 -> Apr
          5 -> May
          6 -> Jun
          7 -> Jul
          8 -> Aug
          9 -> Sep
          10 -> Oct
          11 -> Nov
          _ -> Dec


makeNextEvents : Posix -> Zone -> List NextEvent
makeNextEvents now zone =
    List.map (eventToNextEvent now zone) events

init : Flags -> (Model, Cmd Msg)
init flags =
    let
        zone = utc
        epoch = millisToPosix 0
        width = flags.width
        height = flags.height
        ratio =  flags.ratio
    in
        ( Model flags.width flags.height flags.ratio zone epoch []
        , Task.perform AdjustTimeZone Time.here
        )

compareNextEvent : NextEvent -> NextEvent -> Order
compareNextEvent a b =
    compare (posixToMillis a.eventTime) (posixToMillis b.eventTime)

    
-- UPDATE
type Msg
  = Tick Posix
  | AdjustTimeZone Zone
  | Resized Int Int
  | WaitTimeOver

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
      Tick tickTime ->
          let
              roundedTime = (posixToMillis tickTime) // 1000 * 1000 |> millisToPosix
              firstEvent = List.head model.nextEvents
              recompute = case firstEvent of
                              Nothing ->
                                  True
                              Just first ->
                                  (posixToMillis first.eventTime) < (posixToMillis roundedTime)
          in
              if recompute then
                  ( { model | time = roundedTime, nextEvents = makeNextEvents roundedTime model.zone |> List.sortWith compareNextEvent }
                  , scheduleNextTick tickTime
                  )
              else
                  ( { model | time = roundedTime }
                  , scheduleNextTick tickTime
                  )

      WaitTimeOver ->
          ( model
          , Task.perform Tick Time.now
          )
              
      AdjustTimeZone newZone ->
          ( { model | zone = newZone }
          , Task.perform Tick Time.now
          )

      Resized w h ->
          ( { model | width = w, height = h }
          , Cmd.none
          )

scheduleNextTick : Posix -> Cmd Msg
scheduleNextTick fromTime =
    let
        remaining = toFloat (1000 - modBy 1000 (posixToMillis fromTime))
    in
        Process.sleep remaining |> Task.perform (\_ -> WaitTimeOver)
        
-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  E.onResize (\w h -> Resized w h)

      
-- VIEW
view : Model -> Html Msg
view model =
    Element.layout []
        -- (text ((String.fromFloat model.pixelRatio)))
        (column [ width fill, centerX ]
             (List.map (viewNextEvent model) model.nextEvents)
        )

viewNextEvent : Model -> NextEvent -> Element Msg
viewNextEvent model nextEvent =
    let
        evTime = nextEvent.eventTime
        remainingTime = (subtractPosix evTime time) // 1000
        parts = secsToParts dhms remainingTime
        zone = model.zone
        time = model.time
    in
        column [ width fill, centerX, padding 2 ]
            [ el [ width fill
                 , padding 4
                 , Font.size (fontSize model.width model.pixelRatio 28)
                 , Font.regular
                 , Font.center
                 ]
                  (text nextEvent.name)
            , el [ width fill
                 , padding 2
                 , Font.size (fontSize model.width model.pixelRatio 14)
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
                 , padding 2
                 , Font.size (fontSize model.width model.pixelRatio 24)
                 , Font.bold
                 , Font.center
                 , Font.variantList [ Font.tabularNumbers ]
                 ]
                  (if remainingTime < 30*24*60*60 then
                      (text ((maybeViewNum 0 "d " (Dict.get "d" parts))
                                 ++ (maybeViewNum 2 ":" (Dict.get "h" parts))
                                 ++ (maybeViewNum 2 ":" (Dict.get "m" parts))
                                 ++ (maybeViewNum 2 "" (Dict.get "s" parts))
                            )
                      )
                  else
                      (text ((Round.round 1 ((toFloat remainingTime) / (24*60*60))) ++ " days"))
                  )
            ]

fontSize : Int -> Float -> Int -> Int
fontSize width pixelRatio baseSize =
    (toFloat baseSize) * pixelRatio |> round

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
