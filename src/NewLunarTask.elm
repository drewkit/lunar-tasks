module NewLunarTask exposing
    ( NewLunarTask
    , newLunarTaskEncoder
    , resetNewTask
    )

import Date exposing (Unit(..))
import Json.Encode as Encode


type alias NewLunarTask r =
    { r
        | taskOwner : String
        , newTaskTitle : String
        , newTaskPeriod : Int
        , newTaskCompletedAt : Date.Date
        , demo : Bool
        , demoId : Int
        , currentDate : Date.Date
        , newTaskNotes : String
    }



-- DEFAULT SETTINGS


resetNewTask : NewLunarTask r -> NewLunarTask r
resetNewTask task =
    { task
        | newTaskTitle = ""
        , newTaskCompletedAt = task.currentDate
        , newTaskPeriod = 20
        , newTaskNotes = ""
    }



-- TASK SETTERS/GETTERS
-- ENCODER / DECODER


newLunarTaskEncoder : NewLunarTask r -> Encode.Value
newLunarTaskEncoder task =
    let
        date =
            task.newTaskCompletedAt

        completionYear : Int
        completionYear =
            Date.year date

        completionDay : Int
        completionDay =
            Date.ordinalDay date

        completionEntries : Encode.Value
        completionEntries =
            Encode.list Encode.object
                [ [ ( "year", Encode.int completionYear )
                  , ( "day", Encode.int completionDay )
                  ]
                ]
    in
    if task.demo then
        Encode.object
            [ ( "taskOwner", Encode.string task.taskOwner )
            , ( "title", Encode.string task.newTaskTitle )
            , ( "period", Encode.int task.newTaskPeriod )
            , ( "notes", Encode.string task.newTaskNotes )
            , ( "completionEntries", completionEntries )
            , ( "id", Encode.string (String.fromInt task.demoId) )
            ]

    else
        Encode.object
            [ ( "taskOwner", Encode.string task.taskOwner )
            , ( "title", Encode.string task.newTaskTitle )
            , ( "period", Encode.int task.newTaskPeriod )
            , ( "notes", Encode.string task.newTaskNotes )
            , ( "completionEntries", completionEntries )
            ]
