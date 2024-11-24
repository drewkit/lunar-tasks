module LunarTask exposing
    ( AllYearOrSeasonal(..)
    , LunarTask
    , deleteTaskFromList
    , findTaskById
    , genTaskWithOptions
    , getDaysPastDue
    , getHistoricalCadence
    , getLastCompletedAt
    , getNextPastDueDate
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

import Date exposing (Date, Interval(..), Unit(..))
import Html exposing (th)
import Http exposing (task)
import Json.Decode as Decode exposing (Decoder, field, int, map2, map7, map8, string)
import Json.Encode as Encode
import List exposing (length)


type alias SeasonStart =
    Int


type alias SeasonDuration =
    Int


type AllYearOrSeasonal
    = AllYear
    | Seasonal SeasonStart SeasonDuration


type alias LunarTask =
    { taskOwner : String
    , title : String
    , notes : String
    , id : String
    , period : Int
    , bitTags : Int
    , completionEntries : List Date.Date
    , allYearOrSeasonal : AllYearOrSeasonal
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


getHistoricalCadence : LunarTask -> Maybe Int
getHistoricalCadence task =
    let
        completionEntryCount =
            List.length task.completionEntries
    in
    if completionEntryCount < 5 then
        Nothing

    else
        let
            completionEntries =
                List.reverse task.completionEntries

            previousDates =
                List.take (completionEntryCount - 1) completionEntries

            nextDates =
                List.drop 1 completionEntries

            recordedCadences =
                List.map2 (\p n -> Date.diff Date.Days p n) previousDates nextDates

            recordedCadenceCount =
                List.length recordedCadences
        in
        Just <| List.sum recordedCadences // recordedCadenceCount


getNextPastDueDate : LunarTask -> Date.Date
getNextPastDueDate task =
    let
        lastCompletionDate =
            getLastCompletedAt task
    in
    Date.add Date.Days task.period lastCompletionDate


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
        lastCompletedAt =
            case task.allYearOrSeasonal of
                Seasonal seasonStart seasonDuration ->
                    case getSeasonalData currentDate seasonStart seasonDuration of
                        NextSeason seasonStartDate _ ->
                            seasonStartDate

                        CurrentSeason seasonStartDate seasonEndDate ->
                            let
                                taskLastCompleted =
                                    getLastCompletedAt task
                            in
                            if Date.isBetween seasonStartDate seasonEndDate taskLastCompleted then
                                taskLastCompleted

                            else
                                seasonStartDate

                AllYear ->
                    getLastCompletedAt task

        dateDiff =
            Date.diff Days lastCompletedAt currentDate
    in
    [ dateDiff - task.period ]
        |> (::) 0
        |> List.maximum
        |> Maybe.withDefault 0


type SeasonalData
    = CurrentSeason Date.Date Date.Date
    | NextSeason Date.Date Date.Date


getSeasonalData : Date.Date -> SeasonStart -> SeasonDuration -> SeasonalData
getSeasonalData currentDate seasonStart seasonDuration =
    let
        currentYear =
            Date.year currentDate

        aSeasonStartDate =
            Date.fromOrdinalDate (currentYear - 1) seasonStart

        aSeasonEndDate =
            Date.add Date.Days seasonDuration aSeasonStartDate

        bSeasonStartDate =
            Date.fromOrdinalDate currentYear seasonStart

        bSeasonEndDate =
            Date.add Date.Days seasonDuration bSeasonStartDate

        cSeasonStartDate =
            Date.fromOrdinalDate (currentYear + 1) seasonStart

        cSeasonEndDate =
            Date.add Date.Days seasonDuration cSeasonStartDate
    in
    if Date.isBetween aSeasonStartDate aSeasonEndDate currentDate then
        CurrentSeason aSeasonStartDate aSeasonEndDate

    else if Date.isBetween bSeasonStartDate bSeasonEndDate currentDate then
        CurrentSeason bSeasonStartDate bSeasonEndDate

    else if Date.isBetween aSeasonEndDate bSeasonStartDate currentDate then
        NextSeason bSeasonStartDate bSeasonEndDate

    else
        NextSeason cSeasonStartDate cSeasonEndDate


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
    map8 LunarTask
        (field "taskOwner" string)
        (field "title" string)
        (field "notes" string)
        (field "id" string)
        (field "period" int)
        (field "bitTags" int)
        (field "completionEntries" (Decode.list entryDecoder))
        (Decode.succeed AllYear)


entryDecoder : Decoder Date.Date
entryDecoder =
    entryInfoDecoder
        |> Decode.andThen
            (\n -> Decode.succeed (entryInfoToDate n))


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


genTaskWithOptions : { entries : List Date.Date, period : Int } -> LunarTask
genTaskWithOptions opts =
    { taskOwner = "1234567"
    , title = "task"
    , bitTags = 0
    , notes = ""
    , id = "1234567788"
    , period = opts.period
    , completionEntries = opts.entries
    , allYearOrSeasonal = AllYear
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
    , allYearOrSeasonal = AllYear
    }
