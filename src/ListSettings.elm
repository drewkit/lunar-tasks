module ListSettings exposing (..)

import BitFlags
import Date
import Dict
import Json.Decode as Decode exposing (Decoder, errorToString)
import Json.Encode as Encode
import LunarTask exposing (..)
import Set exposing (Set)
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser exposing ((<?>), Parser)
import Url.Parser.Query as QueryParser


type alias SavedView =
    { filter : ListFilter
    , sort : ListSort
    , tagsSelected : ( Int, Int )
    , searchTerm : Maybe String
    , title : Maybe String
    }


savedViewEncoder : SavedView -> Encode.Value
savedViewEncoder savedView =
    let
        ( listSortType, listSortOrder ) =
            sortToStr savedView.sort
    in
    Encode.object
        [ ( "filter", Encode.string (filterToStr savedView.filter) )
        , ( "sortType", Encode.string listSortType )
        , ( "sortOrder", Encode.string listSortOrder )
        , ( "tagsWhitelisted", Encode.int (Tuple.first savedView.tagsSelected) )
        , ( "tagsBlacklisted", Encode.int (Tuple.second savedView.tagsSelected) )
        , ( "searchTerm"
          , case savedView.searchTerm of
                Just searchTerm ->
                    Encode.string searchTerm

                Nothing ->
                    Encode.null
          )
        , ( "title"
          , case savedView.title of
                Just title ->
                    Encode.string title

                Nothing ->
                    Encode.null
          )
        ]


savedViewSearch : Maybe String -> List SavedView -> Maybe SavedView
savedViewSearch savedViewTitle savedViews =
    case savedViews of
        savedView :: rest ->
            if savedView.title == savedViewTitle then
                Just savedView

            else
                savedViewSearch savedViewTitle rest

        [] ->
            Nothing


filterToStr : ListFilter -> String
filterToStr filter =
    filterToQueryParam filter


sortToStr : ListSort -> ( String, String )
sortToStr listSort =
    listSortToQueryParam listSort


savedViewDecoder : Decoder SavedView
savedViewDecoder =
    Decode.map7
        (\listFilter whitelistTags blacklistTags title searchTerm sortType sortOrder ->
            SavedView
                listFilter
                (strToListSort sortOrder sortType)
                ( whitelistTags, blacklistTags )
                searchTerm
                title
        )
        (Decode.field "filter" listFilterDecoder)
        (Decode.field "tagsWhitelisted" Decode.int)
        (Decode.field "tagsBlacklisted" Decode.int)
        (Decode.field "title" (Decode.maybe Decode.string))
        (Decode.field "searchTerm" (Decode.maybe Decode.string))
        (Decode.field "sortType" Decode.string)
        (Decode.field "sortOrder" sortOrderDecoder)


listFilterDecoder : Decoder ListFilter
listFilterDecoder =
    Decode.map strToListFilter Decode.string


strToListFilter : String -> ListFilter
strToListFilter str =
    case str of
        "pastdue" ->
            FilterPastDue

        "nonpastdue" ->
            FilterNonPastDue

        "all" ->
            FilterAll

        -- not in use
        "filterpastduebydays" ->
            FilterPastDueByDays 1

        -- not in use
        "filterpastduebyperiods" ->
            FilterPastDueByPeriods 1

        _ ->
            FilterAll


sortOrderDecoder : Decoder SortOrder
sortOrderDecoder =
    Decode.string
        |> Decode.map
            (\str ->
                case str of
                    "asc" ->
                        ASC

                    "desc" ->
                        DESC

                    _ ->
                        ASC
            )


strToSortOrder : String -> SortOrder
strToSortOrder str =
    case str of
        "asc" ->
            ASC

        "desc" ->
            DESC

        _ ->
            ASC


strToListSort : SortOrder -> String -> ListSort
strToListSort sortOrder str =
    case str of
        "days" ->
            SortPastDueDays sortOrder

        "period" ->
            SortPastDuePeriods sortOrder

        "lastcompleted" ->
            SortLastCompleted sortOrder

        "createdat" ->
            NoSort sortOrder

        _ ->
            NoSort sortOrder


defaultSavedView : SavedView
defaultSavedView =
    { filter = FilterAll
    , sort = NoSort DESC
    , tagsSelected = ( 0, 0 )
    , searchTerm = Nothing
    , title = Nothing
    }


setSavedView : SavedView -> ListSettings r -> ListSettings r
setSavedView savedView listSettings =
    { listSettings
        | filter = savedView.filter
        , sort = savedView.sort
        , tagsSelected = savedView.tagsSelected
        , searchTerm = savedView.searchTerm
    }


savedViewMatch : SavedView -> SavedView -> Bool
savedViewMatch a b =
    a.filter
        == b.filter
        && a.sort
        == b.sort
        && a.tagsSelected
        == b.tagsSelected
        && a.searchTerm
        == b.searchTerm


findMatchingSavedView : SavedView -> List SavedView -> Maybe SavedView
findMatchingSavedView currentSavedView savedViews =
    case savedViews of
        savedView :: remainingSavedViews ->
            if savedViewMatch savedView currentSavedView then
                Just savedView

            else
                findMatchingSavedView currentSavedView remainingSavedViews

        [] ->
            Nothing


currentView : ListSettings r -> SavedView
currentView ls =
    let
        savedViewPlaceholder =
            SavedView ls.filter ls.sort ls.tagsSelected ls.searchTerm Nothing
    in
    case findMatchingSavedView savedViewPlaceholder ls.savedViews of
        Just savedView ->
            savedView

        Nothing ->
            let
                placeholderName =
                    genUniqueSavedViewName ls (getSavedViewName savedViewPlaceholder)

                savedViewPlaceholderWithUniqueName =
                    { savedViewPlaceholder | title = Just placeholderName }
            in
            savedViewPlaceholderWithUniqueName


currentViewIsSavedView : ListSettings r -> Bool
currentViewIsSavedView ls =
    List.any
        (savedViewMatch <| currentView ls)
        ls.savedViews


getSavedViewName : SavedView -> String
getSavedViewName savedView =
    Maybe.withDefault
        (Maybe.withDefault "SAVED VIEW" savedView.searchTerm)
        savedView.title


genUniqueSavedViewName : ListSettings r -> String -> String
genUniqueSavedViewName ls savedViewName =
    let
        savedViewNames =
            List.map (\x -> getSavedViewName x) ls.savedViews
    in
    if List.member savedViewName savedViewNames then
        recurGenUniqueSavedViewName 1 savedViewNames savedViewName

    else
        savedViewName


recurGenUniqueSavedViewName : Int -> List String -> String -> String
recurGenUniqueSavedViewName count savedViewNames savedViewName =
    let
        iteratedSavedViewName =
            savedViewName ++ " (" ++ String.fromInt count ++ ")"
    in
    if List.member iteratedSavedViewName savedViewNames then
        recurGenUniqueSavedViewName (count + 1) savedViewNames savedViewName

    else
        iteratedSavedViewName


type alias ListSettings r =
    { r
        | filter : ListFilter
        , sort : ListSort
        , tagsSelected : ( Int, Int )
        , searchTerm : Maybe String
        , savedViews : List SavedView
        , tagSettings : BitFlags.BitFlagSettings
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


updateSort : ListSort -> ListSettings r -> ListSettings r
updateSort listSort listSettings =
    { listSettings | sort = listSort }


toggleTag : String -> ListSettings r -> ListSettings r
toggleTag tag listSettings =
    let
        ( whitelistRegister, blacklistRegister ) =
            listSettings.tagsSelected

        getEnabledFlags =
            listSettings.tagSettings
                |> BitFlags.enabledFlags

        ( whitelistTags, blacklistTags ) =
            ( getEnabledFlags whitelistRegister
            , getEnabledFlags blacklistRegister
            )

        whitelistMember =
            List.member tag whitelistTags

        blacklistMember =
            List.member tag blacklistTags

        flipTag =
            listSettings.tagSettings
                |> BitFlags.flipFlag
    in
    case ( whitelistMember, blacklistMember ) of
        ( False, False ) ->
            { listSettings | tagsSelected = ( flipTag tag whitelistRegister, blacklistRegister ) }

        ( True, False ) ->
            { listSettings | tagsSelected = ( flipTag tag whitelistRegister, flipTag tag blacklistRegister ) }

        ( False, True ) ->
            { listSettings | tagsSelected = ( whitelistRegister, flipTag tag blacklistRegister ) }

        -- revisit this to make impossible states impossible
        ( True, True ) ->
            { listSettings | tagsSelected = ( flipTag tag blacklistRegister, flipTag tag blacklistRegister ) }


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
            rawTerm |> String.trim |> String.toLower

        taskTitle =
            task.title |> String.trim |> String.toLower
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



-- SETTING QUERY PARAMS


sortOrderToQueryParam : SortOrder -> String
sortOrderToQueryParam sortOrder =
    case sortOrder of
        ASC ->
            "asc"

        DESC ->
            "desc"


listSortToQueryParam : ListSort -> ( String, String )
listSortToQueryParam listSort =
    case listSort of
        SortPastDueDays order ->
            ( "days", sortOrderToQueryParam order )

        SortPastDuePeriods order ->
            ( "period", sortOrderToQueryParam order )

        SortLastCompleted order ->
            ( "lastcompleted", sortOrderToQueryParam order )

        NoSort order ->
            ( "createdat", sortOrderToQueryParam order )


filterToQueryParam : ListFilter -> String
filterToQueryParam listFilter =
    case listFilter of
        FilterPastDue ->
            "pastdue"

        FilterNonPastDue ->
            "nonpastdue"

        FilterAll ->
            "all"

        FilterPastDueByDays _ ->
            "filterpastduebydays"

        FilterPastDueByPeriods _ ->
            "filterpastduebyperiods"


generateQueryParams : ListSettings r -> String
generateQueryParams listSettings =
    let
        ( whitelistRegister, blacklistRegister ) =
            listSettings.tagsSelected

        showEnabledTags =
            listSettings.tagSettings
                |> BitFlags.enabledFlags

        ( whitelistStr, blacklistStr ) =
            ( String.join "," (showEnabledTags whitelistRegister)
            , String.join "," (showEnabledTags blacklistRegister)
            )

        buildIfNotDefault defaultVal val builder acc =
            if defaultVal == val then
                acc

            else
                builder :: acc
    in
    case listSortToQueryParam listSettings.sort of
        ( listSort, sortOrder ) ->
            []
                |> buildIfNotDefault (NoSort DESC) listSettings.sort (Builder.string "sort" listSort)
                |> buildIfNotDefault (NoSort DESC) listSettings.sort (Builder.string "order" sortOrder)
                |> buildIfNotDefault FilterAll listSettings.filter (Builder.string "filter" (filterToQueryParam listSettings.filter))
                |> buildIfNotDefault Nothing listSettings.searchTerm (Builder.string "q" (Maybe.withDefault "" listSettings.searchTerm))
                |> buildIfNotDefault 0 whitelistRegister (Builder.string "whitelist" whitelistStr)
                |> buildIfNotDefault 0 blacklistRegister (Builder.string "blacklist" blacklistStr)
                |> Builder.toQuery



-- PARSING QUERY PARAMS


initListSettingsFromQueryParams : Url -> ListSettings r -> ListSettings r
initListSettingsFromQueryParams url listSettings =
    let
        sortParser =
            QueryParser.map2 (\a b -> Maybe.withDefault NoSort a (Maybe.withDefault DESC b))
                (QueryParser.enum
                    "sort"
                    (Dict.fromList
                        [ ( "days", SortPastDueDays )
                        , ( "period", SortPastDuePeriods )
                        , ( "lastcompleted", SortLastCompleted )
                        , ( "createdat", NoSort )
                        ]
                    )
                )
                (QueryParser.enum
                    "order"
                    (Dict.fromList
                        [ ( "asc", ASC )
                        , ( "desc", DESC )
                        ]
                    )
                )

        filterParser =
            Url.Parser.map (Maybe.withDefault FilterAll) <|
                Url.Parser.top
                    <?> QueryParser.enum
                            "filter"
                            (Dict.fromList
                                [ ( "pastdue", FilterPastDue )
                                , ( "notpastdue", FilterNonPastDue )
                                , ( "all", FilterAll )
                                ]
                            )

        searchParser : Parser (Maybe String -> b) b
        searchParser =
            Url.Parser.top
                <?> QueryParser.string "q"

        selectedTagsParser : Parser (( Set String, Set String ) -> b) b
        selectedTagsParser =
            Url.Parser.map constructSelectedTagsFromQueryParams <|
                Url.Parser.top
                    <?> QueryParser.string "whitelist"
                    <?> QueryParser.string "blacklist"

        constructSelectedTagsFromQueryParams : Maybe String -> Maybe String -> ( Set String, Set String )
        constructSelectedTagsFromQueryParams maybeWhite maybeBlack =
            let
                processPresentParam param =
                    String.split "," (Maybe.withDefault "" param)
                        |> Set.fromList
            in
            case ( isPresent maybeWhite, isPresent maybeBlack ) of
                ( True, True ) ->
                    ( processPresentParam maybeWhite, processPresentParam maybeBlack )

                ( True, False ) ->
                    ( processPresentParam maybeWhite, Set.empty )

                ( False, True ) ->
                    ( Set.empty, processPresentParam maybeBlack )

                ( False, False ) ->
                    ( Set.empty, Set.empty )

        sort =
            Url.Parser.parse (Url.Parser.query sortParser) url

        filter : Maybe ListFilter
        filter =
            Url.Parser.parse filterParser url

        search =
            Url.Parser.parse searchParser url

        selectedTags =
            Url.Parser.parse selectedTagsParser url
                |> Maybe.withDefault ( Set.empty, Set.empty )
                |> (\( a, b ) -> ( BitFlags.genRegister listSettings.tagSettings a, BitFlags.genRegister listSettings.tagSettings b ))

        savedViewParser : Parser (Maybe String -> b) b
        savedViewParser =
            Url.Parser.top
                <?> QueryParser.string "savedview"

        maybeSavedView : Maybe SavedView
        maybeSavedView =
            Url.Parser.parse savedViewParser url
                |> Maybe.andThen (\title -> savedViewSearch title listSettings.savedViews)

        settingsWithoutSavedViewAsserted =
            { listSettings
                | sort = Maybe.withDefault (NoSort DESC) sort
                , filter = Maybe.withDefault FilterAll filter
                , searchTerm = Maybe.withDefault Nothing search
                , tagsSelected = selectedTags
            }
    in
    case maybeSavedView of
        Just savedView ->
            setSavedView savedView listSettings

        Nothing ->
            settingsWithoutSavedViewAsserted



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


isPresent : Maybe a -> Bool
isPresent item =
    case item of
        Nothing ->
            False

        Just _ ->
            True
