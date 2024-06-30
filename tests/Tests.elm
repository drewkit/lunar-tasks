module Tests exposing (..)

import Date
import Expect
import ListSettings exposing (..)
import LunarTask exposing (getLastCompletedAt, markTaskCompleted, pastDue)
import Main exposing (..)
import Test exposing (..)
import Time
import Url


suite : Test
suite =
    let
        url =
            { protocol = Url.Http
            , host = "localhost"
            , port_ = Just 8000
            , path = "/"
            , query = Just "q=mow%20lawn&filter=pastdue&sort=lastcompleted&sortorder=asc"
            , fragment = Nothing
            }

        ( testModel, _ ) =
            initWithoutNavKey ( 1711211815576, True ) url

        ( modelWithEmptyQueryParams, _ ) =
            update (UrlChanged { url | query = Nothing }) testModel
    in
    describe "Demo Mode"
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
            , skip <|
                describe "with query params removed"
                    [ test "nothing in text search" <|
                        \_ ->
                            Expect.equal Nothing modelWithEmptyQueryParams.searchTerm
                    , test "filter by All" <|
                        \_ ->
                            Expect.equal modelWithEmptyQueryParams.filter FilterAll
                    , test "sort by NoSort DESC" <|
                        \_ ->
                            Expect.equal (NoSort DESC) modelWithEmptyQueryParams.sort
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
                            , lastCompletion = Date.fromPosix Time.utc prevTime
                            }

                    ( dayLaterModel, _ ) =
                        update (ReceivedCurrentTime dayLaterTime) testModel
                in
                Expect.all
                    [ \task -> Expect.notEqual (pastDue testModel.currentDate task) True
                    , \task -> Expect.equal (pastDue dayLaterModel.currentDate task) True
                    ]
                    taskThatIsPastDueInOneDay
        ]
