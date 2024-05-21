module Tests exposing (..)

import Date
import Expect
import LunarTask exposing (markTaskCompleted)
import Main exposing (..)
import Test exposing (..)


suite : Test
suite =
    let
        ( testModel, _ ) =
            init 1711211815576 True
    in
    describe "Demo Mode"
        [ test "CreateTask adds task to Model task list" <|
            \_ ->
                let
                    demoModel =
                        { testModel
                            | demo = True
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
        ]
