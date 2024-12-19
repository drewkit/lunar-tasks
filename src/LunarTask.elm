module LunarTask exposing
    ( AllYearOrSeasonal(..)
    , AllYearOrSeasonalOption(..)
    , LunarTask
    , SeasonalData(..)
    , deleteTaskFromList
    , findTaskById
    , genTaskWithOptions
    , getDaysPastDue
    , getHistoricalCadence
    , getLastCompletedAt
    , getNextPastDueDate
    , getSeasonalData
    , getTaskTypeOption
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
import Json.Decode as Decode exposing (Decoder, field, int, map2, oneOf, string)
import Json.Decode.Extra exposing (andMap)
import Json.Encode as Encode
import List exposing (length)


type alias SeasonStart =
    Int


type alias SeasonEnd =
    Int


type AllYearOrSeasonal
    = AllYear
    | Seasonal SeasonStart SeasonEnd


type AllYearOrSeasonalOption
    = AllYearOption
    | SeasonalOption


getTaskTypeOption : LunarTask -> AllYearOrSeasonalOption
getTaskTypeOption task =
    case task.taskType of
        AllYear ->
            AllYearOption

        Seasonal _ _ ->
            SeasonalOption


type alias LunarTask =
    { taskOwner : String
    , title : String
    , notes : String
    , id : String
    , period : Int
    , bitTags : Int
    , completionEntries : List Date.Date
    , taskType : AllYearOrSeasonal
    , manualPastDueDate : Maybe Date.Date
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
            case task.taskType of
                Seasonal seasonStart seasonEnd ->
                    case getSeasonalData currentDate seasonStart seasonEnd of
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

        manualPastDueDate =
            Maybe.withDefault (Date.fromOrdinalDate 1990 1)
                task.manualPastDueDate

        manualPastDueDateDiff =
            Date.diff Days manualPastDueDate currentDate
    in
    case Date.compare manualPastDueDate lastCompletedAt of
        GT ->
            [ manualPastDueDateDiff ]
                |> (::) 0
                |> List.maximum
                |> Maybe.withDefault 0

        _ ->
            [ dateDiff - task.period ]
                |> (::) 0
                |> List.maximum
                |> Maybe.withDefault 0


type SeasonalData
    = CurrentSeason Date.Date Date.Date
    | NextSeason Date.Date Date.Date


getSeasonalData : Date.Date -> SeasonStart -> SeasonEnd -> SeasonalData
getSeasonalData currentDate seasonStart seasonEnd =
    let
        currentYear =
            Date.year currentDate

        startDateInThisYear =
            Date.fromOrdinalDate currentYear seasonStart

        startDateInNextYear =
            Date.fromOrdinalDate (currentYear + 1) seasonStart

        startDateInLastYear =
            Date.fromOrdinalDate (currentYear - 1) seasonStart

        endDateInThisYear =
            Date.fromOrdinalDate currentYear seasonEnd

        endDateInNextYear =
            Date.fromOrdinalDate (currentYear + 1) seasonEnd
    in
    case Date.compare startDateInThisYear endDateInThisYear of
        GT ->
            if
                Date.isBetween
                    startDateInLastYear
                    endDateInThisYear
                    currentDate
            then
                CurrentSeason startDateInLastYear endDateInThisYear

            else if
                Date.isBetween
                    startDateInThisYear
                    endDateInNextYear
                    currentDate
            then
                CurrentSeason startDateInThisYear endDateInNextYear

            else
                NextSeason startDateInThisYear endDateInNextYear

        _ ->
            if
                Date.isBetween
                    startDateInThisYear
                    endDateInThisYear
                    currentDate
            then
                CurrentSeason startDateInThisYear endDateInThisYear

            else if
                Date.isBetween
                    (Date.fromOrdinalDate currentYear 0)
                    startDateInThisYear
                    currentDate
            then
                NextSeason startDateInThisYear endDateInThisYear

            else
                NextSeason startDateInNextYear
                    endDateInNextYear


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

        allYearOrSeasonalToObject =
            case task.taskType of
                AllYear ->
                    Encode.null

                Seasonal seasonStart seasonEnd ->
                    Encode.object
                        [ ( "seasonStart", Encode.int seasonStart )
                        , ( "seasonEnd", Encode.int seasonEnd )
                        ]

        manualPastDueDateToObject =
            case task.manualPastDueDate of
                Nothing ->
                    Encode.null

                Just date ->
                    Encode.object <| completionEntryToObject date
    in
    Encode.object
        [ ( "taskOwner", Encode.string task.taskOwner )
        , ( "id", Encode.string task.id )
        , ( "title", Encode.string task.title )
        , ( "notes", Encode.string task.notes )
        , ( "period", Encode.int task.period )
        , ( "bitTags", Encode.int task.bitTags )
        , ( "completionEntries", Encode.list Encode.object (List.map completionEntryToObject task.completionEntries) )
        , ( "type", allYearOrSeasonalToObject )
        , ( "manualPastDueDate", manualPastDueDateToObject )
        ]


lunarTaskDecoder : Decoder LunarTask
lunarTaskDecoder =
    Decode.succeed LunarTask
        |> andMap (field "taskOwner" string)
        |> andMap (field "title" string)
        |> andMap (field "notes" string)
        |> andMap (field "id" string)
        |> andMap (field "period" int)
        |> andMap (field "bitTags" int)
        |> andMap (field "completionEntries" (Decode.list entryDecoder))
        |> andMap (field "type" allYearOrSeasonalDecoder)
        |> andMap (field "manualPastDueDate" (Decode.maybe entryDecoder))


allYearOrSeasonalDecoder : Decoder AllYearOrSeasonal
allYearOrSeasonalDecoder =
    oneOf
        [ seasonalDecoder
        , Decode.succeed AllYear
        ]


seasonalDecoder : Decoder AllYearOrSeasonal
seasonalDecoder =
    oneOf
        [ map2 Seasonal
            (field "seasonStart" int)
            (field "seasonEnd" int)
        , seasonalDecoderWithDurationField
        ]


seasonalDecoderWithDurationField : Decoder AllYearOrSeasonal
seasonalDecoderWithDurationField =
    map2
        (\start duration ->
            let
                end =
                    modBy 365 (start + duration)
            in
            Seasonal start end
        )
        (field "seasonStart" int)
        (field "seasonDuration" int)


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


genTaskWithOptions :
    { entries : List Date.Date
    , period : Int
    , manualPastDueDate : Maybe Date.Date
    }
    -> LunarTask
genTaskWithOptions opts =
    { taskOwner = "1234567"
    , title = "task"
    , bitTags = 0
    , notes = ""
    , id = "1234567788"
    , period = opts.period
    , completionEntries = opts.entries
    , taskType = AllYear
    , manualPastDueDate = opts.manualPastDueDate
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
    , taskType = AllYear
    , manualPastDueDate = Nothing
    }
