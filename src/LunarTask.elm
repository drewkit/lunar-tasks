module LunarTask exposing
    ( LunarTask
    , deleteTaskFromList
    , findTaskById
    , genTaskWithOptions
    , getDaysPastDue
    , getLastCompletedAt
    , insertOrUpdateTask
    , lunarTaskDecoder
    , lunarTaskEncoder
    , lunarTaskWithOneHundredCompletionEntries
    , markTaskCompleted
    , notPastDue
    , pastDue
    , pastDueByDays
    , pastDueByPeriods
    , periodsPastDue
    , removeCompletionEntry
    )

import Date exposing (Unit(..))
import Json.Decode exposing (Decoder, field, int, map2, map7, string)
import Json.Encode as Encode


type alias LunarTask =
    { taskOwner : String
    , title : String
    , notes : String
    , id : String
    , period : Int
    , bitTags : Int
    , completionEntries : List Date.Date
    }



-- TASK SETTERS/GETTERS


markTaskCompleted : LunarTask -> Maybe Date.Date -> LunarTask
markTaskCompleted task maybeEntryDate =
    case maybeEntryDate of
        Just entryDate ->
            if List.member entryDate task.completionEntries then
                task

            else
                let
                    completionEntries =
                        (entryDate :: task.completionEntries)
                            |> List.sortWith Date.compare
                            |> List.reverse
                            |> List.take 100
                in
                { task | completionEntries = completionEntries }

        Nothing ->
            task


getLastCompletedAt : LunarTask -> Date.Date
getLastCompletedAt task =
    Maybe.withDefault
        (Date.fromOrdinalDate 1990 1)
        (List.head task.completionEntries)


removeCompletionEntry : LunarTask -> Date.Date -> LunarTask
removeCompletionEntry task dateEntry =
    let
        completionEntries =
            List.filter
                (\n -> n /= dateEntry)
                task.completionEntries
    in
    { task | completionEntries = completionEntries }


getDaysPastDue : Date.Date -> LunarTask -> Int
getDaysPastDue currentDate task =
    let
        dateDiff =
            Date.diff Days (getLastCompletedAt task) currentDate
    in
    [ dateDiff - task.period ]
        |> (::) 0
        |> List.maximum
        |> Maybe.withDefault 0


pastDue : Date.Date -> LunarTask -> Bool
pastDue currentDate task =
    getDaysPastDue currentDate task > 0


notPastDue : Date.Date -> LunarTask -> Bool
notPastDue currentDate task =
    not (pastDue currentDate task)


pastDueByDays : Date.Date -> Int -> LunarTask -> Bool
pastDueByDays currentDate days task =
    getDaysPastDue currentDate task >= days


periodsPastDue : Date.Date -> LunarTask -> Float
periodsPastDue currentDate task =
    toFloat (getDaysPastDue currentDate task) / toFloat task.period


pastDueByPeriods : Date.Date -> Int -> LunarTask -> Bool
pastDueByPeriods currentDate periods task =
    periodsPastDue currentDate task >= toFloat periods



-- TASKLIST SETTERS/GETTERS


findTaskById : String -> List LunarTask -> Maybe LunarTask
findTaskById id tasks =
    case tasks of
        [] ->
            Nothing

        first :: rest ->
            if first.id == id then
                Just first

            else
                findTaskById id rest


insertOrUpdateTask : LunarTask -> List LunarTask -> List LunarTask
insertOrUpdateTask task taskList =
    case taskList of
        [] ->
            [ task ]

        first :: rest ->
            if first.id == task.id then
                task :: rest

            else
                first :: insertOrUpdateTask task rest


deleteTaskFromList : String -> List LunarTask -> List LunarTask
deleteTaskFromList taskId taskList =
    case taskList of
        [] ->
            []

        first :: rest ->
            if first.id == taskId then
                rest

            else
                first :: deleteTaskFromList taskId rest



-- ENCODER / DECODER


lunarTaskEncoder : LunarTask -> Encode.Value
lunarTaskEncoder task =
    let
        completionEntryToObject date =
            [ ( "year", Encode.int (Date.year date) )
            , ( "day", Encode.int (Date.ordinalDay date) )
            ]
    in
    Encode.object
        [ ( "taskOwner", Encode.string task.taskOwner )
        , ( "id", Encode.string task.id )
        , ( "title", Encode.string task.title )
        , ( "notes", Encode.string task.notes )
        , ( "period", Encode.int task.period )
        , ( "bitTags", Encode.int task.bitTags )
        , ( "completionEntries", Encode.list Encode.object (List.map completionEntryToObject task.completionEntries) )
        ]


lunarTaskDecoder : Decoder LunarTask
lunarTaskDecoder =
    map7 LunarTask
        (field "taskOwner" string)
        (field "title" string)
        (field "notes" string)
        (field "id" string)
        (field "period" int)
        (field "bitTags" int)
        (field "completionEntries" (Json.Decode.list entryDecoder))


entryDecoder : Decoder Date.Date
entryDecoder =
    entryInfoDecoder
        |> Json.Decode.andThen
            (\n -> Json.Decode.succeed (entryInfoToDate n))


entryInfoDecoder : Decoder EntryInfo
entryInfoDecoder =
    map2 EntryInfo
        (field "year" int)
        (field "day" int)


type alias EntryInfo =
    { year : Int
    , day : Int
    }


entryInfoToDate : EntryInfo -> Date.Date
entryInfoToDate info =
    Date.fromOrdinalDate info.year info.day



-- Mocked Data


genTaskWithOptions : { lastCompletion : Date.Date, period : Int } -> LunarTask
genTaskWithOptions opts =
    { taskOwner = "1234567"
    , title = "task"
    , bitTags = 0
    , notes = ""
    , id = "1234567788"
    , period = opts.period
    , completionEntries = [ opts.lastCompletion ]
    }


lunarTaskWithOneHundredCompletionEntries : LunarTask
lunarTaskWithOneHundredCompletionEntries =
    let
        completionEntries : List Date.Date
        completionEntries =
            List.map
                (\day -> Date.fromOrdinalDate 2021 day)
                (List.range
                    1
                    100
                )
    in
    { taskOwner = "1234567"
    , title = "taskWithOneHundredCompletionEntries"
    , bitTags = 0
    , notes = ""
    , id = "1234567788"
    , period = 20
    , completionEntries = completionEntries
    }
