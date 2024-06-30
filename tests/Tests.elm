module Tests exposing (..)

import Date
import Expect
import LunarTask exposing (getLastCompletedAt, markTaskCompleted, pastDue)
import Main exposing (..)
import Test exposing (..)
import Time
import Url


suite : Test
suite =
    let
        ( testModel, _ ) =
            initWithoutNavKey ( 1711211815576, True )
                { protocol = Url.Http
                , host = "localhost"
                , port_ = Just 8000
                , path = "/"
                , query = Nothing
                , fragment = Nothing
                }
    in
    describe "Demo Mode"
        [ test "CreateTask adds task to Model task list" <|
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
