module Tests exposing (..)

import Date
import Expect
import ListSettings exposing (..)
import LunarTask exposing (SeasonalData(..), genTaskWithOptions, getHistoricalCadence, getLastCompletedAt, getSeasonalData, markTaskCompleted, pastDue)
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

        fallOrdinal =
            280

        springOrdinal =
            100

        summerMonthsOrdinalStart =
            springOrdinal

        summerMonthsOrdinalEnd =
            fallOrdinal
    in
    describe "Lunar Tasks"
        [ describe "when the manual past due date is set"
            [ test "the past due state of task can be pushed back to a later date" <|
                \_ ->
                    let
                        taskPeriod =
                            5

                        currentDate =
                            Date.fromOrdinalDate 2024 fallOrdinal

                        lastCompletedAt =
                            Date.add Date.Days (taskPeriod * -3) currentDate

                        manualPastDueDate =
                            Date.add Date.Days (taskPeriod * 3) currentDate

                        notRecentlyCompletedButWithManualPastDueDateInFuture =
                            genTaskWithOptions
                                { entries = [ lastCompletedAt ]
                                , period = taskPeriod
                                , manualPastDueDate = Just manualPastDueDate
                                }
                    in
                    Expect.equal
                        (LunarTask.pastDue
                            currentDate
                            notRecentlyCompletedButWithManualPastDueDateInFuture
                        )
                        False
            , test "but only if the date is more recent than the last completion entry" <|
                \_ ->
                    let
                        currentDate =
                            Date.fromOrdinalDate 2024 fallOrdinal

                        taskPeriod =
                            5

                        lastCompletedAt =
                            Date.add Date.Days (floor (taskPeriod * -0.5)) currentDate

                        manualPastDueDate =
                            Date.add Date.Days (taskPeriod * -3) currentDate

                        stillNotPastDueTaskWithOutdatedManualPastDueDate =
                            genTaskWithOptions
                                { entries = [ lastCompletedAt ]
                                , period = taskPeriod
                                , manualPastDueDate = Just manualPastDueDate
                                }
                    in
                    Expect.equal
                        (LunarTask.pastDue
                            currentDate
                            stillNotPastDueTaskWithOutdatedManualPastDueDate
                        )
                        False
            , test "the past due state of task can also be brought forward to an earlier date" <|
                \_ ->
                    let
                        taskPeriod =
                            300

                        currentDate =
                            Date.fromOrdinalDate 2024 fallOrdinal

                        lastCompletedAt =
                            Date.add Date.Days (floor (taskPeriod * -0.3)) currentDate

                        manualPastDueDate =
                            Date.add Date.Days (floor (taskPeriod * -0.2)) currentDate

                        recentlyCompletedButPastDueAgainTask =
                            genTaskWithOptions
                                { entries = [ lastCompletedAt ]
                                , period = taskPeriod
                                , manualPastDueDate = Just manualPastDueDate
                                }
                    in
                    Expect.equal
                        (LunarTask.pastDue
                            currentDate
                            recentlyCompletedButPastDueAgainTask
                        )
                        True
            ]
        , describe "getSeasonalData"
            [ describe "currently summer time of year"
                [ test "summer task shows as in season" <|
                    \_ ->
                        let
                            currentDate =
                                Date.fromOrdinalDate 2024 (fallOrdinal - 20)

                            currentSeasonStartDate =
                                Date.fromOrdinalDate 2024 summerMonthsOrdinalStart

                            currentSeasonEndDate =
                                Date.fromOrdinalDate 2024 summerMonthsOrdinalEnd
                        in
                        Expect.equal (CurrentSeason currentSeasonStartDate currentSeasonEndDate)
                            (getSeasonalData
                                currentDate
                                summerMonthsOrdinalStart
                                summerMonthsOrdinalEnd
                            )
                ]
            , describe "currently winter time of year"
                [ test "summer task shows as not in season" <|
                    \_ ->
                        let
                            currentDate =
                                Date.fromOrdinalDate 2024 (fallOrdinal + 20)

                            nextSeasonStartDate =
                                Date.fromOrdinalDate 2025 summerMonthsOrdinalStart

                            nextSeasonEndDate =
                                Date.fromOrdinalDate 2025 summerMonthsOrdinalEnd
                        in
                        Expect.equal (NextSeason nextSeasonStartDate nextSeasonEndDate)
                            (getSeasonalData
                                currentDate
                                summerMonthsOrdinalStart
                                summerMonthsOrdinalEnd
                            )
                ]
            ]
        , describe "query params"
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
                        update (NewTaskEffect NewTaskSubmit) demoModel
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
                            , manualPastDueDate = Nothing
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
                            genTaskWithOptions { period = 10, entries = [], manualPastDueDate = Nothing }

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
                            genTaskWithOptions { period = 10, entries = [], manualPastDueDate = Nothing }

                        overFiveEntries =
                            genEntries 1711211815576 15 12

                        taskOverFiveCompletionEntries =
                            List.foldl (\entry t -> markTaskCompleted t (Just entry)) task overFiveEntries
                    in
                    Expect.equal (getHistoricalCadence taskOverFiveCompletionEntries) (Just 15)
            ]
        ]
