module Tests exposing (..)

import Expect
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
        ]
