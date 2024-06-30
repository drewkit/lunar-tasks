module ListSettings exposing (..)

import BitFlags exposing (BitFlagSettings)
import Date
import LunarTask exposing (..)
import Set exposing (Set)


type alias ListSettings r =
    { r
        | filter : ListFilter
        , sort : ListSort
        , tagsSelected : ( Set String, Set String )
        , searchTerm : Maybe String
        , tagSettings : BitFlagSettings
    }


type ListFilter
    = FilterPastDue
    | FilterPastDueByDays Int
    | FilterPastDueByPeriods Int
    | FilterNonPastDue
    | FilterAll


type SortOrder
    = ASC
    | DESC


type ListSort
    = SortPastDueDays SortOrder
    | SortPastDuePeriods SortOrder
    | SortLastCompleted SortOrder
    | NoSort SortOrder


updateListFilter : ListSettings r -> ListFilter -> ListSettings r
updateListFilter listSettings filterSetting =
    { listSettings | filter = filterSetting }


resetFilter : ListSettings r -> ListSettings r
resetFilter listSettings =
    { listSettings
        | filter = FilterAll
        , sort = NoSort DESC
        , tagsSelected = ( Set.fromList [], Set.fromList [] )
        , searchTerm = Nothing
    }


updateSort : ListSort -> ListSettings r -> ListSettings r
updateSort listSort listSettings =
    { listSettings | sort = listSort }


toggleTag : String -> ListSettings r -> ListSettings r
toggleTag tag listSettings =
    let
        ( whitelistTags, blacklistTags ) =
            listSettings.tagsSelected

        whitelistMember =
            Set.member tag whitelistTags

        blacklistMember =
            Set.member tag blacklistTags
    in
    case ( whitelistMember, blacklistMember ) of
        ( False, False ) ->
            { listSettings | tagsSelected = ( Set.insert tag whitelistTags, blacklistTags ) }

        ( True, False ) ->
            { listSettings | tagsSelected = ( Set.remove tag whitelistTags, Set.insert tag blacklistTags ) }

        ( False, True ) ->
            { listSettings | tagsSelected = ( whitelistTags, Set.remove tag blacklistTags ) }

        -- revisit this to make impossible states impossible
        ( True, True ) ->
            { listSettings | tagsSelected = ( Set.remove tag whitelistTags, Set.remove tag blacklistTags ) }


toggleSortOrder : ListSettings r -> ListSettings r
toggleSortOrder listSettings =
    let
        listSort =
            listSettings.sort
    in
    { listSettings | sort = flipOrder listSort }


selectFilter : ListFilter -> ListSettings r -> ListSettings r
selectFilter filter listSettings =
    { listSettings | filter = filter }


updateSearchTerm : String -> ListSettings r -> ListSettings r
updateSearchTerm termString listSettings =
    let
        term =
            if String.trim termString == "" then
                Nothing

            else
                Just termString
    in
    { listSettings | searchTerm = term }



-- FILTER


filterTaskList : ListFilter -> Date.Date -> List LunarTask -> List LunarTask
filterTaskList listFilter currentDate tasks =
    case listFilter of
        FilterAll ->
            tasks

        FilterPastDue ->
            List.filter (pastDue currentDate) tasks

        FilterNonPastDue ->
            List.filter (notPastDue currentDate) tasks

        FilterPastDueByDays days ->
            List.filter (pastDueByDays currentDate days) tasks

        FilterPastDueByPeriods periods ->
            List.filter (pastDueByPeriods currentDate periods) tasks


filterByTags : (Int -> Bool) -> List LunarTask -> List LunarTask
filterByTags match tasks =
    List.filter (\t -> match t.bitTags) tasks


hasTerm : String -> LunarTask -> Bool
hasTerm rawTerm task =
    let
        term =
            rawTerm |> String.trim

        taskTitle =
            task.title |> String.trim
    in
    String.contains term taskTitle


filterByTerm : Maybe String -> List LunarTask -> List LunarTask
filterByTerm maybeTerm tasks =
    case maybeTerm of
        Nothing ->
            tasks

        Just term ->
            List.filter (hasTerm term) tasks



-- SORT


comparePastDueDays : Date.Date -> LunarTask -> LunarTask -> Order
comparePastDueDays currentDate a b =
    let
        aDays =
            getDaysPastDue currentDate a

        bDays =
            getDaysPastDue currentDate b
    in
    if aDays > bDays then
        GT

    else if aDays < bDays then
        LT

    else
        EQ


comparePastDuePeriods : Date.Date -> LunarTask -> LunarTask -> Order
comparePastDuePeriods currentDate a b =
    let
        aPeriods =
            periodsPastDue currentDate a

        bPeriods =
            periodsPastDue currentDate b
    in
    if aPeriods > bPeriods then
        GT

    else if aPeriods < bPeriods then
        LT

    else
        EQ


compareLastCompleted : LunarTask -> LunarTask -> Order
compareLastCompleted a b =
    Date.compare (getLastCompletedAt a)
        (getLastCompletedAt b)


sortTaskList : ListSort -> Date.Date -> List LunarTask -> List LunarTask
sortTaskList sortType currentDate tasks =
    case sortType of
        SortPastDueDays sortOrder ->
            case sortOrder of
                ASC ->
                    List.sortWith (comparePastDueDays currentDate) tasks

                DESC ->
                    List.sortWith (comparePastDueDays currentDate) tasks
                        |> List.reverse

        SortPastDuePeriods sortOrder ->
            case sortOrder of
                ASC ->
                    List.sortWith (comparePastDuePeriods currentDate) tasks

                DESC ->
                    List.sortWith (comparePastDuePeriods currentDate) tasks
                        |> List.reverse

        SortLastCompleted sortOrder ->
            case sortOrder of
                ASC ->
                    List.sortWith compareLastCompleted tasks

                DESC ->
                    List.sortWith compareLastCompleted tasks
                        |> List.reverse

        NoSort sortOrder ->
            case sortOrder of
                ASC ->
                    tasks

                DESC ->
                    tasks |> List.reverse



-- HELPERS


sortOrderFromListSort : ListSort -> SortOrder
sortOrderFromListSort listSort =
    case listSort of
        SortPastDueDays sortOrder ->
            sortOrder

        SortPastDuePeriods sortOrder ->
            sortOrder

        SortLastCompleted sortOrder ->
            sortOrder

        NoSort sortOrder ->
            sortOrder


isComplementarySortOrder : SortOrder -> ListSort -> Bool
isComplementarySortOrder sortOrder listSort =
    sortOrder /= sortOrderFromListSort listSort


isMatchingSortOrder : SortOrder -> ListSort -> Bool
isMatchingSortOrder sortOrder listSort =
    sortOrder == sortOrderFromListSort listSort


listSortToText : ListSort -> String
listSortToText listSort =
    case listSort of
        SortPastDueDays _ ->
            "Days Past Due"

        SortPastDuePeriods _ ->
            "Periods Lapsed"

        SortLastCompleted _ ->
            "Last Completed"

        NoSort _ ->
            "Created At"


flipOrder : ListSort -> ListSort
flipOrder listSort =
    let
        flip a =
            if a == ASC then
                DESC

            else
                ASC
    in
    case listSort of
        SortPastDueDays order ->
            SortPastDueDays (flip order)

        SortPastDuePeriods order ->
            SortPastDuePeriods (flip order)

        SortLastCompleted order ->
            SortLastCompleted (flip order)

        NoSort order ->
            NoSort (flip order)
