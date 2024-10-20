module Tests exposing (..)

import Date
import Expect
import ListSettings exposing (..)
import LunarTask exposing (LunarTask, genTaskWithOptions, getHistoricalCadence, getLastCompletedAt, markTaskCompleted, pastDue)
import Main exposing (..)
import Test exposing (..)
import Time
import Url


suite : Test
suite =
    let
        dayInMillis =
            86400000.0

        genEntries : Int -> Float -> Int -> List Date.Date
        genEntries start period count =
            let
                periodInMillis : Float
                periodInMillis =
                    period * dayInMillis
            in
            List.range 0 (count - 1)
                |> List.map toFloat
                |> List.map (\n -> toFloat start + (periodInMillis * n))
                |> List.map (\n -> Time.millisToPosix <| round n)
                |> List.map (\n -> Date.fromPosix Time.utc n)

        url =
            { protocol = Url.Http
            , host = "localhost"
            , port_ = Just 8000
            , path = "/"
            , query = Just "q=mow%20lawn&filter=pastdue&sort=lastcompleted&order=asc"
            , fragment = Nothing
            }

        ( testModel, _ ) =
            initWithMaybeNavKey ( 1711211815576, True ) url Nothing
    in
    describe "Lunar Tasks"
        [ describe "query params"
            [ describe "on initialization with loaded query params"
                [ test "'mow lawn' in text search" <|
                    \_ ->
                        Expect.equal (Just "mow lawn") testModel.searchTerm
                , test "filter by past due" <|
                    \_ ->
                        Expect.equal testModel.filter FilterPastDue
                , test "sort by last completed, ascending" <|
                    \_ ->
                        Expect.equal testModel.sort (SortLastCompleted ASC)
                ]
            ]
        , test "CreateTask adds task to Model task list" <|
            \_ ->
                let
                    demoModel =
                        { testModel
                            | demo = Just { demoId = 1 }
                            , newTaskTitle = "test new task"
                            , newTaskPeriod = 15
                        }

                    ( updatedModel, _ ) =
                        update CreateTask demoModel
                in
                Expect.equal 1 (List.length updatedModel.tasks)
        , test "truncates completed entries list to the last 100 entries" <|
            \_ ->
                let
                    taskWithOneMoreCompletionEntry =
                        markTaskCompleted
                            LunarTask.lunarTaskWithOneHundredCompletionEntries
                            (Just <| Date.fromOrdinalDate 2023 21)
                in
                Expect.equal (List.length <| taskWithOneMoreCompletionEntry.completionEntries) 100
        , test "truncation does not remove newest completion entry" <|
            \_ ->
                let
                    taskWithOneMoreCompletionEntry =
                        markTaskCompleted
                            LunarTask.lunarTaskWithOneHundredCompletionEntries
                            (Just <| Date.fromOrdinalDate 2023 21)
                in
                Expect.equal (getLastCompletedAt taskWithOneMoreCompletionEntry) (Date.fromOrdinalDate 2023 21)
        , test "with the start of a new day, a task transitions to a past due state" <|
            \_ ->
                let
                    prevTime =
                        Time.millisToPosix 1711211815576

                    dayLaterTime =
                        Time.millisToPosix (1711211815576 + (10000 * 60 * 60 * 24))

                    taskThatIsPastDueInOneDay =
                        LunarTask.genTaskWithOptions
                            { period = 1
                            , entries = [ Date.fromPosix Time.utc prevTime ]
                            }

                    ( dayLaterModel, _ ) =
                        update (ReceivedCurrentTime dayLaterTime) testModel
                in
                Expect.all
                    [ \task -> Expect.notEqual (pastDue testModel.currentDate task) True
                    , \task -> Expect.equal (pastDue dayLaterModel.currentDate task) True
                    ]
                    taskThatIsPastDueInOneDay
        , describe "historical cadence of completed entries"
            [ test "no reporting under five entries" <|
                \_ ->
                    let
                        task =
                            genTaskWithOptions { period = 10, entries = [] }

                        fourEntries =
                            genEntries 1711211815576 12.2 4

                        taskUnderFiveCompletionEntries =
                            List.foldl (\entry t -> markTaskCompleted t (Just entry)) task fourEntries
                    in
                    Expect.equal (getHistoricalCadence taskUnderFiveCompletionEntries) Nothing
            , test "reporting over five entries" <|
                \_ ->
                    let
                        task =
                            genTaskWithOptions { period = 10, entries = [] }

                        overFiveEntries =
                            genEntries 1711211815576 15 12

                        taskOverFiveCompletionEntries =
                            List.foldl (\entry t -> markTaskCompleted t (Just entry)) task overFiveEntries
                    in
                    Expect.equal (getHistoricalCadence taskOverFiveCompletionEntries) (Just 15)
            ]
        ]
