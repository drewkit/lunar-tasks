module ListSettings exposing (..)

import Date
import LunarTask exposing (..)


type alias ListSettings r =
    { r
        | filter : ListFilter
        , sort : ListSort
        , tagSelected : Maybe String
        , searchTerm : Maybe String
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
        , tagSelected = Nothing
        , searchTerm = Nothing
    }


updateSort : ListSort -> ListSettings r -> ListSettings r
updateSort listSort listSettings =
    { listSettings | sort = listSort }


selectTag : Maybe String -> ListSettings r -> ListSettings r
selectTag maybeTagName listSettings =
    case maybeTagName of
        Nothing ->
            listSettings

        Just tagName ->
            let
                tagSelected =
                    if listSettings.tagSelected == Just tagName then
                        Nothing

                    else
                        Just tagName
            in
            { listSettings | tagSelected = tagSelected }


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


filterByTag : Maybe String -> List LunarTask -> List LunarTask
filterByTag maybeTagSelected tasks =
    let
        hasTag : String -> LunarTask -> Bool
        hasTag tag task =
            case .tag task of
                Just val ->
                    val == tag

                Nothing ->
                    False
    in
    case maybeTagSelected of
        Just tagName ->
            List.filter (hasTag tagName) tasks

        Nothing ->
            tasks


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
