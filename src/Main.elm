port module Main exposing (..)

import BitFlags exposing (BitFlagSettings)
import Browser exposing (UrlRequest(..))
import Browser.Dom as Dom
import Browser.Events exposing (Visibility(..), onVisibilityChange)
import Browser.Navigation as Nav
import Date exposing (Date, Unit(..))
import DatePicker exposing (defaultSettings)
import Dropdown exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input exposing (OptionState(..), button)
import FeatherIcons as Icon exposing (Icon, key)
import Html exposing (Html, td, th, tr)
import Html.Attributes exposing (style, type_, value)
import Html.Events
import Http
import Json.Decode as Decode exposing (Decoder, errorToString)
import Json.Encode as Encode
import Keyboard exposing (Key(..))
import List
import ListSettings exposing (..)
import LunarTask exposing (..)
import Markdown.Renderer.ElmUi as Markdown
import NewLunarTask exposing (..)
import Process
import SHA1
import SearchBox
import Set
import Task
import Time exposing (utc)
import Url exposing (Url)
import Url.Parser exposing ((<?>))



-- MAIN


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequest
        , onUrlChange = UrlChanged
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ messageReceiver Recv
        , Keyboard.downs ProcessDownKeys
        , Time.every 60000 ReceivedCurrentTime
        , onVisibilityChange VisibilityChanged
        ]



-- PORTS


port userActions : ( String, String, Encode.Value ) -> Cmd msg


port taskAction : ( String, Encode.Value ) -> Cmd msg


port messageReceiver : ({ tag : String, payload : Decode.Value } -> msg) -> Sub msg


port userLoginAction : String -> Cmd msg


port localStoreAction : ( String, Encode.Value ) -> Cmd msg



-- MODEL


type alias Model =
    { url : Url
    , maybeKey : Maybe Nav.Key
    , tasks : List LunarTask
    , taskOwner : String
    , currentDate : Date.Date
    , currentZone : Time.Zone
    , filter : ListFilter
    , sort : ListSort
    , tagsSelected : ( Int, Int )
    , searchTerm : Maybe String
    , savedViews : List SavedView
    , savedViewDropdownState : Dropdown.State SavedView
    , savedViewTitleInput : String
    , view : ViewState
    , newTaskTitle : String
    , newTaskNotes : String
    , newTaskPeriod : Int
    , newTaskCompletedAt : Date.Date
    , newTaskType : AllYearOrSeasonal
    , taskTypeOption : AllYearOrSeasonalOption
    , banner : String
    , editedTask : Maybe LunarTask
    , tagSettings : BitFlagSettings
    , demo : Maybe { demoId : Int }
    , datePicker : DatePicker.DatePicker
    , datePickerForManualPastDue : DatePicker.DatePicker
    , tagNameInput : String
    , tagSearchBox : SearchBox.State
    , tagSearchBoxText : String
    , tagSearchBoxSelected : Maybe String
    , tagResourcesLoaded : Bool
    , lastCacheCheckAt : Time.Posix
    , receivedCurrentTimeAt : Time.Posix
    }


type alias EditingNotes =
    Bool


type MainTaskViewModes
    = Normal
    | EditSavedViewTitle


type LoadedTasksViewState
    = JsonExportView
    | MainTasksView MainTaskViewModes
    | EditTaskView EditingNotes
    | TagSettingsView (Maybe String)


type ViewState
    = LoginPromptView
    | LoadingTasksView
    | LoadingTasksFailureView
    | LoadedTasksView LoadedTasksViewState



-- LOCAL STORE


type alias LocalStore =
    { tasks : List LunarTask
    , taskOwner : String
    , bitFlags : BitFlagSettings
    }


localStoreDecoder : Decoder LocalStore
localStoreDecoder =
    Decode.map3 LocalStore
        (Decode.field "tasks" (Decode.list lunarTaskDecoder))
        (Decode.field "taskOwner" Decode.string)
        (Decode.field "bitFlags" (Decode.list Decode.string)
            |> Decode.map (\l -> Result.withDefault (BitFlags.defaultSettings 25) (BitFlags.initSettings { bitLimit = 25, flags = l }))
        )


localStoreEncoder : Model -> Encode.Value
localStoreEncoder model =
    Encode.object
        [ ( "tasks", Encode.list lunarTaskEncoder model.tasks )
        , ( "taskOwner", Encode.string model.taskOwner )
        , ( "bitFlags", Encode.list Encode.string (BitFlags.serialize model.tagSettings) )
        ]



-- CACHE DIGEST


generateCacheDigest : List LunarTask -> Model -> String
generateCacheDigest tasks model =
    let
        tags =
            model.tagSettings
                |> BitFlags.serialize
                |> String.join ","
    in
    tasks
        |> Encode.list lunarTaskEncoder
        |> Encode.encode 0
        |> (++) tags
        |> SHA1.fromString
        |> SHA1.toHex



-- MISC DECODING / ENCODING


taskTagsDecoder : Decoder (List String)
taskTagsDecoder =
    Decode.at [ "record", "tags" ] (Decode.list Decode.string)


taskOwnerDecoder : Decoder String
taskOwnerDecoder =
    Decode.oneOf [ googleAuthTaskOwnerDecoder, authStoreTaskOwnerDecoder ]


googleAuthTaskOwnerDecoder : Decoder String
googleAuthTaskOwnerDecoder =
    Decode.at [ "record", "id" ] Decode.string


authStoreTaskOwnerDecoder : Decoder String
authStoreTaskOwnerDecoder =
    Decode.at [ "id" ] Decode.string


authStoreTaskOwnerEncoder : String -> Encode.Value
authStoreTaskOwnerEncoder id =
    Encode.object
        [ ( "id", Encode.string id ) ]



-- MISC DEFAULT SETTINGS


type alias LoginAttributes r =
    { r
        | tasks : List LunarTask
        , taskOwner : String
        , view : ViewState
        , demo : Maybe { demoId : Int }
        , tagSettings : BitFlagSettings
        , banner : String
        , tagResourcesLoaded : Bool
    }


resetLogin : LoginAttributes r -> LoginAttributes r
resetLogin attrs =
    { attrs
        | tasks = []
        , demo = Nothing
        , taskOwner = ""
        , view = LoginPromptView
        , tagSettings = BitFlags.defaultSettings 25
        , banner = ""
        , tagResourcesLoaded = False
    }


datePickerSettings : DatePicker.Settings
datePickerSettings =
    { defaultSettings
        | placeholder = "Add an entry"
        , inputAttributes =
            [ style "border" "none"
            , style "font-size" "22px"
            , style "text-align" "center"
            ]
    }


savedViewDropdownConfig : Dropdown.Config SavedView Msg Model
savedViewDropdownConfig =
    let
        itemToTitle item =
            getSavedViewName item
                |> String.pad 20 ' '
    in
    Dropdown.basic
        { itemsFromModel = \m -> m.savedViews
        , selectionFromModel = \m -> findMatchingSavedView (currentView m) m.savedViews
        , dropdownMsg = SavedViewDropdown
        , onSelectMsg = SavedViewSelection
        , itemToPrompt = \savedView -> el [] (text <| itemToTitle savedView)
        , itemToElement = \_ _ savedView -> el [] (text <| itemToTitle savedView)
        }
        |> Dropdown.withListAttributes
            [ Border.solid
            , Border.color color.grey
            , Border.width 1
            , Background.color color.white
            ]
        |> Dropdown.withPromptElement (el [] <| text "-- Saved Views --")



-- INIT


currentTags : BitFlagSettings -> List String
currentTags settings =
    BitFlags.allFlags settings


type alias Flags =
    ( Int, Bool )


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    initWithMaybeNavKey flags url (Just key)


initWithMaybeNavKey : Flags -> Url -> Maybe Nav.Key -> ( Model, Cmd Msg )
initWithMaybeNavKey ( currentTimeinMillis, validAuth ) url maybeKey =
    let
        loginCmd =
            if validAuth then
                userLoginAction "login"

            else
                Cmd.none

        loadingOrLoginView =
            if validAuth then
                LoadingTasksView

            else
                LoginPromptView

        currentZone =
            utc

        currentDate =
            Date.fromPosix currentZone (Time.millisToPosix currentTimeinMillis)

        currentTime =
            Time.millisToPosix currentTimeinMillis

        ( newDatePicker, datePickerCmd ) =
            DatePicker.init

        ( newDatePickerForManualPastDue, datePickerCmdForManualPastDue ) =
            DatePicker.init

        model : Model
        model =
            { maybeKey = maybeKey
            , url = url
            , tasks = []
            , taskOwner = ""
            , currentDate = currentDate
            , currentZone = currentZone

            -- filter will be overridden on init with default saved view
            , filter = FilterAll

            -- sort will be overridden on init with default saved view
            , sort = NoSort DESC

            -- tags selected will be overridden on init with default saved view
            , tagsSelected = ( 0, 0 )
            , tagSettings = BitFlags.defaultSettings 25

            -- search term will be overridden on init with default saved view
            , searchTerm = Nothing
            , savedViews = []
            , savedViewDropdownState = Dropdown.init "saved-view-dropdown"
            , savedViewTitleInput = ""
            , datePicker = newDatePicker
            , datePickerForManualPastDue = newDatePickerForManualPastDue
            , view = loadingOrLoginView
            , newTaskTitle = ""
            , newTaskNotes = ""
            , newTaskPeriod = 15
            , newTaskCompletedAt = currentDate
            , newTaskType = AllYear
            , taskTypeOption = AllYearOption
            , banner = ""
            , editedTask = Nothing
            , demo = Nothing
            , tagNameInput = ""
            , tagSearchBox = SearchBox.init
            , tagSearchBoxText = ""
            , tagSearchBoxSelected = Nothing
            , tagResourcesLoaded = False
            , lastCacheCheckAt = currentTime
            , receivedCurrentTimeAt = currentTime
            }
    in
    ( model
        |> initListSettingsFromQueryParams model.url
    , Cmd.batch
        [ loginCmd
        , Time.now |> Task.perform ReceivedCurrentTime
        , Time.here |> Task.perform AdjustTimeZone
        , Cmd.map (\x -> NewTaskEffect (SetNewTaskDatePicker x)) datePickerCmd
        , Cmd.map (\x -> EditTaskEffect (SetManualPastDueDate x)) datePickerCmdForManualPastDue
        ]
    )



-- UPDATE


type Msg
    = Recv { tag : String, payload : Decode.Value }
    | ToggleTag String
    | EditTags
    | SelectTagToEdit (Maybe String)
    | DeleteTag String
    | UpdatedTagNameInput String
    | CompareCacheDigest Decode.Value
    | CreateTag
    | UpdateTag String
    | Search String
    | ClearSearch
    | ClearBanner
    | SavedViewRemove
    | SavedViewRemoved Decode.Value
    | SavedViewAdd
    | SavedViewAdded Decode.Value
    | SavedViewDropdown (Dropdown.Msg SavedView)
    | SavedViewSelection (Maybe SavedView)
    | SavedViewEditTitle String
    | SavedViewUpdateTitleInput String
    | SavedViewUpdateTitle
    | FilterReset
    | LocalStoreFetched Decode.Value
    | ToggleSortOrder
    | ToggleNoteEdit
    | SelectFilter ListFilter
    | SelectSort ListSort
    | ViewChange ViewState
    | MarkTaskCompleted LunarTask Date.Date
    | NewTaskEffect NewTaskMsg
    | EditTaskEffect EditTaskMsg
    | DeleteTask LunarTask
    | ProcessDownKeys Keyboard.RawKey
    | AdjustTimeZone Time.Zone
    | ReceivedCurrentTime Time.Posix
    | TaskUpdated Decode.Value
    | TaskDeleted Decode.Value
    | VisibilityChanged Visibility
    | LogOutUser Int
    | LoadTasks Decode.Value
    | LoadTags Decode.Value
    | LoginUser Int
    | DemoLoginUser Int
    | LoadDemo (Result Http.Error String)
    | UserLoggedIn Decode.Value
    | DemoIdTick Time.Posix
    | ReturnToMain
    | UrlRequest Browser.UrlRequest
    | UrlChanged Url
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        -- PORT HELPERS
        deleteTask : Encode.Value -> Cmd msg
        deleteTask encodedTask =
            taskAction ( "delete", encodedTask )

        fetchTasks : Cmd msg
        fetchTasks =
            taskAction ( "fetch", Encode.null )

        fetchTags : Cmd msg
        fetchTags =
            userActions ( "fetchTags", model.taskOwner, Encode.null )

        updateTags : Encode.Value -> Cmd msg
        updateTags encodedTags =
            userActions ( "updateTags", model.taskOwner, encodedTags )

        fetchCacheDigest : Cmd msg
        fetchCacheDigest =
            userActions ( "fetchCacheDigest", model.taskOwner, Encode.null )

        updateCacheDigest : String -> Cmd msg
        updateCacheDigest newDigest =
            userActions ( "updateCacheDigest", model.taskOwner, Encode.string newDigest )

        -- UPDATE HELPERS WITH RAILWAY PATTERN
        adjustDate : ( Model, List (Cmd Msg) ) -> ( Model, List (Cmd Msg) )
        adjustDate ( m, lc ) =
            let
                newDate =
                    Date.fromPosix m.currentZone m.receivedCurrentTimeAt
            in
            ( { m | currentDate = newDate }, lc )

        maybeUpdateQueryParams : ( Model, List (Cmd Msg) ) -> ( Model, List (Cmd Msg) )
        maybeUpdateQueryParams ( m, lc ) =
            case m.maybeKey of
                Just key ->
                    let
                        oldQueryString =
                            m.url.query

                        newQueryString =
                            generateQueryParams m
                    in
                    if Maybe.withDefault "" oldQueryString /= newQueryString then
                        ( m, Nav.replaceUrl key ("/" ++ newQueryString) :: lc )

                    else
                        ( m, lc )

                Nothing ->
                    ( m, lc )

        fetchCacheDigestIfXMinutesSinceCheck : Int -> ( Model, List (Cmd Msg) ) -> ( Model, List (Cmd Msg) )
        fetchCacheDigestIfXMinutesSinceCheck minutes ( m, lc ) =
            let
                timeSinceLastCacheCheck =
                    Time.posixToMillis m.receivedCurrentTimeAt - Time.posixToMillis m.lastCacheCheckAt
            in
            -- fetch cache digest every X minutes
            if timeSinceLastCacheCheck > (minutes * 60000) && not (isPresent m.demo) then
                ( m, fetchCacheDigest :: lc )

            else
                ( m, lc )

        batchCmdList : ( Model, List (Cmd Msg) ) -> ( Model, Cmd Msg )
        batchCmdList ( m, lc ) =
            ( m, Cmd.batch lc )
    in
    case msg of
        Recv data ->
            case data.tag of
                "userLoggedIn" ->
                    update (UserLoggedIn data.payload) model

                "taskCreated" ->
                    update (TaskUpdated data.payload) model

                "taskUpdated" ->
                    update (TaskUpdated data.payload) model

                "taskDeleted" ->
                    update (TaskDeleted data.payload) model

                "tasksFetched" ->
                    update (LoadTasks data.payload) model

                "tagsFetched" ->
                    update (LoadTags data.payload) model

                "tagsUpdated" ->
                    update (LoadTags data.payload) model

                "localStoreFetched" ->
                    update (LocalStoreFetched data.payload) model

                "cacheDigestFetched" ->
                    update (CompareCacheDigest data.payload) model

                unknownMessage ->
                    update (LoadTasks Encode.null) { model | banner = "Recv data unrecognized: " ++ unknownMessage }

        ToggleTag tag ->
            ( model |> toggleTag tag, [] )
                |> maybeUpdateQueryParams
                |> batchCmdList

        ToggleNoteEdit ->
            ( { model | view = toggleNoteEdit model.view }, Task.attempt (\_ -> NoOp) (Dom.focus "notes-input") )

        EditTags ->
            ( { model | view = LoadedTasksView (TagSettingsView Nothing) }, Cmd.none )

        SelectTagToEdit maybeTagName ->
            case maybeTagName of
                Just tagName ->
                    ( { model | view = LoadedTasksView (TagSettingsView (Just tagName)), tagNameInput = tagName }, Cmd.none )

                Nothing ->
                    ( { model | view = LoadedTasksView (TagSettingsView Nothing), tagNameInput = "" }, Cmd.none )

        CompareCacheDigest jsonUser ->
            let
                digestCacheDecoder =
                    Decode.at [ "cacheDigest" ] <|
                        Decode.string
            in
            case Decode.decodeValue digestCacheDecoder jsonUser of
                Ok backendCacheDigest ->
                    let
                        currentCacheDigest =
                            generateCacheDigest model.tasks model

                        commands =
                            if backendCacheDigest /= currentCacheDigest then
                                Cmd.batch [ fetchTasks, fetchTags ]

                            else
                                Cmd.none
                    in
                    ( { model | lastCacheCheckAt = model.receivedCurrentTimeAt }
                    , commands
                    )

                Err _ ->
                    ( model, Cmd.none )

        CreateTag ->
            let
                addedToTagSettings =
                    BitFlags.createFlag model.tagNameInput model.tagSettings
            in
            if isPresent model.demo then
                case addedToTagSettings of
                    Ok updatedSettings ->
                        ( { model | tagNameInput = "", tagSettings = updatedSettings }, Cmd.none )

                    Err errMsg ->
                        ( { model | banner = errMsg }, Cmd.none )

            else
                case addedToTagSettings of
                    Ok updatedSettings ->
                        ( { model | tagNameInput = "" }, updateTags <| Encode.list Encode.string (BitFlags.serialize updatedSettings) )

                    Err errMsg ->
                        ( { model | banner = errMsg }, Cmd.none )

        UpdateTag oldName ->
            let
                updatedTagSettings =
                    BitFlags.updateFlag oldName model.tagNameInput model.tagSettings
            in
            if isPresent model.demo then
                ( { model | tagSettings = updatedTagSettings, view = LoadedTasksView (TagSettingsView Nothing) }, Cmd.none )

            else
                ( model, updateTags (Encode.list Encode.string (BitFlags.serialize updatedTagSettings)) )

        DeleteTag tagName ->
            let
                updatedTagSettings =
                    BitFlags.deleteFlag tagName model.tagSettings
            in
            if isPresent model.demo then
                ( { model | tagSettings = updatedTagSettings, view = LoadedTasksView (TagSettingsView Nothing) }, Cmd.none )

            else
                ( model, updateTags (Encode.list Encode.string (BitFlags.serialize updatedTagSettings)) )

        UpdatedTagNameInput updatedTagName ->
            ( { model | tagNameInput = updatedTagName }, Cmd.none )

        SelectFilter filter ->
            ( model |> selectFilter filter, [] )
                |> maybeUpdateQueryParams
                |> batchCmdList

        Search term ->
            ( model |> updateSearchTerm term, [] )
                |> maybeUpdateQueryParams
                |> batchCmdList

        ClearSearch ->
            ( model |> updateSearchTerm "", [ Task.attempt (\_ -> NoOp) (Dom.focus "search-term") ] )
                |> maybeUpdateQueryParams
                |> batchCmdList

        SavedViewAdd ->
            let
                updatedSavedViews =
                    currentView model :: model.savedViews
            in
            ( { model | savedViews = updatedSavedViews }, Cmd.none )

        SavedViewAdded _ ->
            ( model, Cmd.none )

        SavedViewRemove ->
            let
                currentSavedView =
                    currentView model

                updatedSavedViews =
                    List.filter
                        (\x ->
                            x /= currentSavedView
                        )
                        model.savedViews
            in
            ( { model | savedViews = updatedSavedViews }
                |> setSavedView defaultSavedView
            , []
            )
                |> maybeUpdateQueryParams
                |> batchCmdList

        SavedViewRemoved _ ->
            ( model, Cmd.none )

        SavedViewDropdown subMsg ->
            let
                ( updatedSavedViewDropdown, cmd ) =
                    Dropdown.update
                        savedViewDropdownConfig
                        subMsg
                        model
                        model.savedViewDropdownState
            in
            ( { model | savedViewDropdownState = updatedSavedViewDropdown }, cmd )

        SavedViewSelection maybeSavedView ->
            case maybeSavedView of
                Nothing ->
                    ( model, Cmd.none )

                Just savedView ->
                    ( model
                        |> setSavedView savedView
                    , []
                    )
                        |> maybeUpdateQueryParams
                        |> batchCmdList

        SavedViewEditTitle previousTitle ->
            ( { model
                | savedViewTitleInput = previousTitle
                , view =
                    LoadedTasksView (MainTasksView EditSavedViewTitle)
              }
            , Cmd.none
            )

        SavedViewUpdateTitleInput title ->
            ( { model | savedViewTitleInput = title }, Cmd.none )

        SavedViewUpdateTitle ->
            let
                currentSavedView =
                    currentView model

                allButCurrentView =
                    List.filter
                        (\x -> not (savedViewMatch currentSavedView x))
                        model.savedViews

                newTitle =
                    genUniqueSavedViewName model model.savedViewTitleInput
            in
            ( { model
                | savedViewTitleInput = ""
                , view = LoadedTasksView (MainTasksView Normal)
                , savedViews =
                    { currentSavedView
                        | title = Just newTitle
                    }
                        :: allButCurrentView
              }
            , Cmd.none
            )

        ClearBanner ->
            ( { model | banner = "" }, Cmd.none )

        FilterReset ->
            ( model |> setSavedView defaultSavedView, [] )
                |> maybeUpdateQueryParams
                |> batchCmdList

        ToggleSortOrder ->
            ( model |> toggleSortOrder, [] )
                |> maybeUpdateQueryParams
                |> batchCmdList

        SelectSort sortType ->
            let
                updatedModel =
                    if sortType == model.sort then
                        toggleSortOrder model

                    else
                        updateSort sortType model
            in
            ( updatedModel, [] )
                |> maybeUpdateQueryParams
                |> batchCmdList

        ViewChange viewType ->
            ( { model | view = viewType }, Cmd.none )

        MarkTaskCompleted task entryDate ->
            let
                jsonTaskMarkedCompleted =
                    lunarTaskEncoder <| markTaskCompleted task (Just entryDate)
            in
            if isPresent model.demo then
                update (TaskUpdated jsonTaskMarkedCompleted) model

            else
                let
                    updateTask : Encode.Value -> Cmd Msg
                    updateTask encodedTask =
                        taskAction ( "update", encodedTask )
                in
                ( model
                , updateTask jsonTaskMarkedCompleted
                )

        NewTaskEffect newTaskMsg ->
            updateNewTask newTaskMsg model

        EditTaskEffect editTaskMsg ->
            updateEditedTask editTaskMsg model

        DeleteTask task ->
            let
                encodedLunarTask =
                    lunarTaskEncoder task
            in
            if isPresent model.demo then
                update (TaskDeleted encodedLunarTask) model

            else
                ( model, deleteTask encodedLunarTask )

        AdjustTimeZone newZone ->
            ( { model | currentZone = newZone }, Cmd.none )

        ReceivedCurrentTime time ->
            ( { model | receivedCurrentTimeAt = time }, [] )
                |> adjustDate
                |> fetchCacheDigestIfXMinutesSinceCheck 20
                |> batchCmdList

        VisibilityChanged visibility ->
            case visibility of
                Visible ->
                    ( model, [] )
                        |> fetchCacheDigestIfXMinutesSinceCheck 5
                        |> batchCmdList

                Hidden ->
                    ( model, Cmd.none )

        TaskDeleted jsonTask ->
            case Decode.decodeValue lunarTaskDecoder jsonTask of
                Ok task ->
                    let
                        tasks =
                            deleteTaskFromList task.id model.tasks

                        modelWithTaskRemoved =
                            { model | tasks = tasks }
                    in
                    ( modelWithTaskRemoved
                    , Cmd.batch
                        [ -- report new state of task list to backend
                          updateCacheDigest (generateCacheDigest tasks modelWithTaskRemoved)
                        , -- update localStore
                          localStoreAction ( "set", localStoreEncoder modelWithTaskRemoved )
                        ]
                    )

                Err err ->
                    ( { model | banner = Decode.errorToString err }, Cmd.none )

        TaskUpdated jsonTask ->
            let
                -- delay fn from https://stackoverflow.com/a/61324383
                delay time delayMsg =
                    -- create a task that sleeps for `time`
                    Process.sleep time
                        |> -- once the sleep is over, ignore its output (using `always`)
                           -- and then we create a new task that simply returns a success, and the msg
                           Task.andThen (always <| Task.succeed delayMsg)
                        |> -- finally, we ask Elm to perform the Task, which
                           -- takes the result of the above task and
                           -- returns it to our update function
                           Task.perform identity
            in
            case Decode.decodeValue lunarTaskDecoder jsonTask of
                Ok task ->
                    let
                        tasks =
                            insertOrUpdateTask task model.tasks

                        modelWithUpdatedTask =
                            { model | tasks = tasks }
                    in
                    ( { modelWithUpdatedTask
                        | banner = "task \"" ++ task.title ++ "\" -- id  " ++ task.id ++ " touched by db"
                      }
                    , Cmd.batch
                        [ delay 5000 ClearBanner

                        -- event for incrementing demo id
                        , Task.perform DemoIdTick Time.now

                        -- report new state of task list to backend
                        , updateCacheDigest <| generateCacheDigest tasks modelWithUpdatedTask

                        -- update localStore
                        , localStoreAction ( "set", localStoreEncoder modelWithUpdatedTask )
                        ]
                    )

                Err err ->
                    ( { model | banner = Decode.errorToString err }, Cmd.none )

        LogOutUser _ ->
            ( model |> resetLogin, userLoginAction "logout" )

        DemoLoginUser _ ->
            let
                flagSettingsResult =
                    BitFlags.initSettings
                        { bitLimit = 25
                        , flags =
                            [ "indoors", "outdoors", "digital", "family" ]
                        }
            in
            ( { model
                | view = LoadingTasksView
                , demo = Just { demoId = 1 }
                , taskOwner = "demoTaskOwnerId"
                , tagSettings =
                    Result.withDefault (BitFlags.defaultSettings 25) flagSettingsResult
                , tagResourcesLoaded = True
              }
            , Cmd.batch
                [ Http.get { url = "/demo-data.json", expect = Http.expectString LoadDemo }
                , Task.perform DemoIdTick Time.now
                ]
            )

        LoadDemo result ->
            case result of
                Ok demoDataString ->
                    case Decode.decodeString (Decode.list lunarTaskDecoder) demoDataString of
                        Ok tasks ->
                            ( { model
                                | view = LoadedTasksView (MainTasksView Normal)
                                , tasks = tasks
                              }
                            , Cmd.none
                            )

                        Err errMsg ->
                            ( { model | banner = Decode.errorToString errMsg }, Cmd.none )

                Err _ ->
                    ( { model | banner = "LoadDemo Http.Error" }, Cmd.none )

        LoginUser _ ->
            ( model, userLoginAction "login" )

        UserLoggedIn jsonResp ->
            case Decode.decodeValue taskOwnerDecoder jsonResp of
                Ok taskOwnerId ->
                    ( { model
                        | taskOwner = taskOwnerId
                        , view = LoadingTasksView
                      }
                    , Cmd.batch [ localStoreAction ( "fetch", Encode.null ) ]
                    )

                Err errMsg ->
                    ( { model
                        | view = LoadingTasksFailureView
                        , banner = errorToString errMsg
                      }
                    , Cmd.none
                    )

        DemoIdTick posixTime ->
            if isPresent model.demo then
                ( { model | demo = Just { demoId = Time.posixToMillis posixTime } }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        LoadTasks jsonTasks ->
            case
                Decode.decodeValue
                    (Decode.list lunarTaskDecoder)
                    jsonTasks
            of
                Ok tasks ->
                    let
                        loadedTasksModel =
                            { model
                                | tasks = tasks
                                , view = LoadedTasksView (MainTasksView Normal)
                            }
                    in
                    ( loadedTasksModel
                    , localStoreAction ( "set", localStoreEncoder loadedTasksModel )
                    )

                Err errMsg ->
                    ( { model
                        | view = LoadingTasksFailureView
                        , banner = errorToString errMsg
                      }
                    , Cmd.none
                    )

        LoadTags jsonTags ->
            let
                updateDecoder =
                    Decode.at [ "tags" ] <|
                        Decode.oneOf [ Decode.list Decode.string, Decode.null [] ]

                fetchDecoder =
                    Decode.at [ "items" ] <|
                        Decode.index 0 <|
                            Decode.at [ "tags" ] <|
                                Decode.oneOf [ Decode.list Decode.string, Decode.null [] ]
            in
            case
                Decode.decodeValue
                    (Decode.oneOf
                        [ fetchDecoder
                        , updateDecoder
                        ]
                    )
                    jsonTags
            of
                Ok tags ->
                    let
                        modelWithLoadedTags =
                            { model
                                | tagSettings =
                                    Result.withDefault
                                        model.tagSettings
                                        (BitFlags.initSettings { bitLimit = 25, flags = tags })
                                , tagResourcesLoaded = True
                            }
                    in
                    ( modelWithLoadedTags
                    , [ -- inform backend of current cached state
                        updateCacheDigest <| generateCacheDigest model.tasks modelWithLoadedTags

                      -- update local store
                      , localStoreAction ( "set", localStoreEncoder modelWithLoadedTags )
                      ]
                    )
                        |> maybeUpdateQueryParams
                        |> batchCmdList

                Err errMsg ->
                    ( { model
                        | banner = errorToString errMsg
                      }
                    , Cmd.none
                    )

        ProcessDownKeys rawKey ->
            case Keyboard.anyKeyOriginal rawKey of
                Just Escape ->
                    update ClearSearch model

                _ ->
                    ( model, Cmd.none )

        LocalStoreFetched jsonResp ->
            case Decode.decodeValue localStoreDecoder jsonResp of
                Ok localStore ->
                    if model.taskOwner == localStore.taskOwner then
                        ( { model
                            | view = LoadedTasksView (MainTasksView Normal)
                            , tasks = localStore.tasks
                            , tagSettings = localStore.bitFlags
                            , tagResourcesLoaded = True
                          }
                        , fetchCacheDigest
                        )

                    else
                        ( { model | banner = "taskOwner not matching with local store" }, Cmd.batch [ fetchTasks, fetchTags ] )

                Err errMsg ->
                    ( { model | banner = Decode.errorToString errMsg }, Cmd.batch [ fetchTasks, fetchTags ] )

        UrlChanged url ->
            ( { model | url = url }, Cmd.none )

        UrlRequest _ ->
            ( model, Cmd.none )

        ReturnToMain ->
            case model.view of
                LoginPromptView ->
                    ( model, Cmd.none )

                LoadingTasksView ->
                    ( model, Cmd.none )

                LoadingTasksFailureView ->
                    ( model, Cmd.none )

                LoadedTasksView _ ->
                    ( { model | view = LoadedTasksView (MainTasksView Normal) }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


type NewTaskMsg
    = UpdateNewTaskTitle String
    | UpdateNewTaskNotes String
    | UpdateNewTaskPeriod String
    | SetNewTaskDatePicker DatePicker.Msg
    | NewTaskSubmit


updateNewTask : NewTaskMsg -> Model -> ( Model, Cmd Msg )
updateNewTask newTaskMsg model =
    case newTaskMsg of
        UpdateNewTaskTitle title ->
            ( { model | newTaskTitle = title }, Cmd.none )

        UpdateNewTaskNotes notes ->
            ( { model | newTaskTitle = notes }, Cmd.none )

        UpdateNewTaskPeriod rawPeriod ->
            case String.toInt rawPeriod of
                Just period ->
                    ( { model | newTaskPeriod = period }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SetNewTaskDatePicker datePickerMsg ->
            let
                ( newDatePicker, dateEvent ) =
                    DatePicker.update datePickerSettings datePickerMsg model.datePicker

                date : Date.Date
                date =
                    case dateEvent of
                        DatePicker.Picked newDate ->
                            newDate

                        _ ->
                            model.newTaskCompletedAt
            in
            ( { model
                | newTaskCompletedAt = date
                , datePicker = newDatePicker
              }
            , Cmd.none
            )

        NewTaskSubmit ->
            let
                createTask : Encode.Value -> Cmd msg
                createTask encodedTask =
                    taskAction ( "create", encodedTask )

                encodedJsonTask =
                    newLunarTaskEncoder model

                modelWithNewTaskReset =
                    resetNewTask model
            in
            if isPresent model.demo then
                update (TaskUpdated encodedJsonTask) modelWithNewTaskReset

            else
                ( modelWithNewTaskReset
                , createTask encodedJsonTask
                )


type EditTaskMsg
    = EditTask String
    | UpdateEditTaskPeriod String
    | UpdateEditTaskTitle String
    | UpdateEditTaskNotes String
    | UpdateSeasonStart Float
    | UpdateSeasonEnd Float
    | UpdateTaskType AllYearOrSeasonalOption
    | DisableTag String
    | EnableTag String
    | RemoveManualPastDueDate
    | SetManualPastDueDate DatePicker.Msg
    | RemoveCompletionEntry Date.Date
    | AddCompletionEntry DatePicker.Msg
    | ChangedTagSearchBox (SearchBox.ChangeEvent String)
    | Cancel
    | Save


updateEditedTask : EditTaskMsg -> Model -> ( Model, Cmd Msg )
updateEditedTask editTaskMsg model =
    case editTaskMsg of
        EditTask taskId ->
            case findTaskById taskId model.tasks of
                Nothing ->
                    ( model, Cmd.none )

                Just task ->
                    ( { model | view = LoadedTasksView (EditTaskView False), editedTask = Just task }, Cmd.none )

        Cancel ->
            ( { model | editedTask = Nothing, view = LoadedTasksView (MainTasksView Normal) }, Cmd.none )

        Save ->
            case model.editedTask of
                Nothing ->
                    ( model, Cmd.none )

                Just editedTask ->
                    let
                        fakeTask : LunarTask
                        fakeTask =
                            { title = ""
                            , notes = ""
                            , period = 20
                            , completionEntries = []
                            , id = "asdfasdf"
                            , bitTags = 0
                            , taskOwner = "alksdjflasd"
                            , taskType = AllYear
                            , manualPastDueDate = Nothing
                            }

                        originalTask =
                            Maybe.withDefault fakeTask (findTaskById editedTask.id model.tasks)
                    in
                    if originalTask /= editedTask then
                        if isPresent model.demo then
                            update (TaskUpdated (lunarTaskEncoder editedTask))
                                { model
                                    | editedTask = Nothing
                                    , view = LoadedTasksView (MainTasksView Normal)
                                }

                        else
                            let
                                updateTask : Encode.Value -> Cmd Msg
                                updateTask encodedTask =
                                    taskAction ( "update", encodedTask )
                            in
                            ( { model
                                | editedTask = Nothing
                                , view = LoadedTasksView (MainTasksView Normal)
                              }
                            , updateTask (lunarTaskEncoder editedTask)
                            )

                    else
                        ( { model | editedTask = Nothing, view = LoadedTasksView (MainTasksView Normal) }, Cmd.none )

        RemoveManualPastDueDate ->
            case model.editedTask of
                Just task ->
                    ( { model
                        | editedTask = Just { task | manualPastDueDate = Nothing }
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        SetManualPastDueDate datePickerMsg ->
            let
                ( updatedDatePicker, dateEvent ) =
                    DatePicker.update datePickerSettings datePickerMsg model.datePickerForManualPastDue
            in
            case model.editedTask of
                Just task ->
                    case dateEvent of
                        DatePicker.Picked pickedDate ->
                            ( { model
                                | editedTask = Just { task | manualPastDueDate = Just pickedDate }
                                , datePickerForManualPastDue = updatedDatePicker
                              }
                            , Cmd.none
                            )

                        _ ->
                            ( { model
                                | datePickerForManualPastDue = updatedDatePicker
                              }
                            , Cmd.none
                            )

                Nothing ->
                    ( { model
                        | datePickerForManualPastDue = updatedDatePicker
                      }
                    , Cmd.none
                    )

        UpdateTaskType taskTypeOption ->
            case model.editedTask of
                Just task ->
                    case taskTypeOption of
                        AllYearOption ->
                            ( { model
                                | taskTypeOption = taskTypeOption
                                , editedTask =
                                    Just { task | taskType = AllYear }
                              }
                            , Cmd.none
                            )

                        SeasonalOption ->
                            ( { model
                                | taskTypeOption = taskTypeOption
                                , editedTask =
                                    Just { task | taskType = Seasonal 100 200 }
                              }
                            , Cmd.none
                            )

                Nothing ->
                    ( model, Cmd.none )

        RemoveCompletionEntry entryDate ->
            case model.editedTask of
                Just task ->
                    ( { model
                        | editedTask =
                            Just
                                (removeCompletionEntry
                                    task
                                    entryDate
                                )
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        AddCompletionEntry datePickerMsg ->
            let
                ( newDatePicker, dateEvent ) =
                    DatePicker.update datePickerSettings datePickerMsg model.datePicker

                date : Maybe Date.Date
                date =
                    case dateEvent of
                        DatePicker.Picked newDate ->
                            Just newDate

                        _ ->
                            Nothing
            in
            case model.editedTask of
                Just task ->
                    ( { model
                        | datePicker = newDatePicker
                        , editedTask =
                            Just
                                (markTaskCompleted
                                    task
                                    date
                                )
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        EnableTag tag ->
            let
                enableTag =
                    BitFlags.enableFlag model.tagSettings tag
            in
            case model.editedTask of
                Just task ->
                    ( { model | editedTask = Just { task | bitTags = enableTag task.bitTags } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        DisableTag tag ->
            let
                disableTag =
                    BitFlags.disableFlag model.tagSettings tag
            in
            case model.editedTask of
                Just task ->
                    ( { model | editedTask = Just { task | bitTags = disableTag task.bitTags } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        UpdateEditTaskTitle titleStr ->
            case model.editedTask of
                Just task ->
                    ( { model | editedTask = Just { task | title = titleStr } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        UpdateEditTaskNotes notes ->
            case model.editedTask of
                Just task ->
                    ( { model | editedTask = Just { task | notes = notes } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        UpdateEditTaskPeriod periodString ->
            let
                period =
                    Maybe.withDefault 10 (String.toInt periodString)
            in
            case model.editedTask of
                Just task ->
                    ( { model | editedTask = Just { task | period = period } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        UpdateSeasonStart seasonStartFloat ->
            let
                seasonStart =
                    floor seasonStartFloat
            in
            case model.editedTask of
                Just task ->
                    case task.taskType of
                        AllYear ->
                            ( model, Cmd.none )

                        Seasonal _ seasonEnd ->
                            ( { model
                                | editedTask =
                                    Just
                                        { task
                                            | taskType = Seasonal seasonStart seasonEnd
                                        }
                              }
                            , Cmd.none
                            )

                Nothing ->
                    ( model, Cmd.none )

        UpdateSeasonEnd seasonEndFloat ->
            let
                seasonEnd =
                    floor seasonEndFloat
            in
            case model.editedTask of
                Just task ->
                    case task.taskType of
                        AllYear ->
                            ( model, Cmd.none )

                        Seasonal seasonStart _ ->
                            ( { model
                                | editedTask =
                                    Just
                                        { task
                                            | taskType = Seasonal seasonStart seasonEnd
                                        }
                              }
                            , Cmd.none
                            )

                Nothing ->
                    ( model, Cmd.none )

        ChangedTagSearchBox changeEvent ->
            case changeEvent of
                SearchBox.SelectionChanged tag ->
                    case model.editedTask of
                        Just task ->
                            ( { model
                                | editedTask = Just { task | bitTags = BitFlags.enableFlag model.tagSettings tag task.bitTags }
                                , tagSearchBoxText = ""
                              }
                            , Cmd.none
                            )

                        Nothing ->
                            ( model, Cmd.none )

                SearchBox.TextChanged text ->
                    ( { model
                        | tagSearchBoxSelected = Nothing
                        , tagSearchBoxText = text
                        , tagSearchBox = SearchBox.reset model.tagSearchBox
                      }
                    , Cmd.none
                    )

                SearchBox.SearchBoxChanged subMsg ->
                    ( { model
                        | tagSearchBox = SearchBox.update subMsg model.tagSearchBox
                      }
                    , Cmd.none
                    )



-- VIEW


loginlogoutButton : ViewState -> Element Msg
loginlogoutButton viewType =
    let
        buttonAttrs =
            [ Background.color color.white
            , Font.color color.googleLogin
            , Font.size 15
            , height (px 42)
            , width (fill |> minimum 150 |> maximum 500)
            , Font.center
            , Border.width 1
            , Border.color color.lightGrey
            , Border.rounded 3
            , Border.shadow
                { offset = ( 0, 0.2 )
                , size = 0.05
                , blur = 0
                , color = color.darkCharcoal
                }
            , Font.family
                [ Font.typeface "Roboto"
                , Font.sansSerif
                ]
            ]

        rowSpecs =
            [ alignRight, spacing 15 ]

        loginButton =
            row rowSpecs
                [ button
                    [ width (fill |> minimum 200)
                    , paddingEach { top = 4, bottom = 4, left = 0, right = 4 }
                    ]
                    { label =
                        row buttonAttrs
                            [ image []
                                { src = "/images/btn_google_light_normal_ios.svg"
                                , description = "google login"
                                }
                            , text "Sign in with Google"
                            ]
                    , onPress = Just (LoginUser 0)
                    }
                , button
                    buttonAttrs
                    { label = text "Demo Mode"
                    , onPress = Just (DemoLoginUser 0)
                    }
                ]

        exportButton =
            case viewType of
                LoadedTasksView (MainTasksView _) ->
                    button buttonAttrs { label = text "JSON export", onPress = Just (ViewChange (LoadedTasksView JsonExportView)) }

                _ ->
                    button buttonAttrs { label = text "Return to Main", onPress = Just (ViewChange (LoadedTasksView (MainTasksView Normal))) }

        logoutButton =
            row rowSpecs
                [ exportButton
                , button buttonAttrs { label = text "Log Out", onPress = Just (LogOutUser 0) }
                ]
    in
    case viewType of
        LoginPromptView ->
            loginButton

        _ ->
            logoutButton


viewDemoModeBanner : Bool -> Element msg
viewDemoModeBanner demo =
    if demo then
        row
            [ width fill
            , padding 3
            , Element.htmlAttribute (Html.Attributes.style "position" "fixed")
            , Element.htmlAttribute (Html.Attributes.style "z-index" "10")
            , Element.htmlAttribute (Html.Attributes.style "background" "rgba(0,0,0,0.3)")
            ]
            [ el [ Font.center, Font.color color.white, width fill, Font.semiBold, paddingXY 0 5 ] <| text "DEMO MODE" ]

    else
        Element.none


viewLayout : Model -> Element Msg -> Html Msg
viewLayout model innerContent =
    layout
        [ width fill
        , height fill
        , Font.family
            [ Font.typeface "Times New Roman"
            , Font.serif
            ]
        ]
    <|
        column [ width fill ]
            [ viewDemoModeBanner (isPresent model.demo)
            , viewHeader model
            , innerContent
            ]


viewHeader : Model -> Element Msg
viewHeader model =
    let
        headerFontColor =
            color.darkBlue

        headerBackgroundColor =
            color.lightBlue
    in
    column
        [ height <| fillPortion 2
        , width fill
        ]
        [ row
            [ width fill
            , padding 20
            , Background.color headerBackgroundColor
            ]
            [ row [ onClick ReturnToMain ]
                [ el [ Font.color headerFontColor ] viewMoon
                , el
                    [ Font.size 55
                    , paddingXY 15 0
                    , Font.color headerFontColor
                    ]
                    (text "LunarTasks")
                ]
            , loginlogoutButton model.view
            ]
        ]


viewMoon : Element msg
viewMoon =
    Icon.moon
        |> Icon.withSize 4
        |> Icon.withSizeUnit "em"
        |> Icon.withStrokeWidth 1
        |> Icon.toHtml []
        |> Element.html


view : Model -> Browser.Document Msg
view model =
    let
        innerContent =
            case model.view of
                LoginPromptView ->
                    viewLandingPage

                LoadingTasksView ->
                    el [] <| text "Loading"

                LoadingTasksFailureView ->
                    el [] (text "")

                LoadedTasksView (MainTasksView _) ->
                    viewMain model

                LoadedTasksView (EditTaskView editingNotes) ->
                    viewTask model editingNotes

                LoadedTasksView JsonExportView ->
                    viewTasksJson model

                LoadedTasksView (TagSettingsView maybeSelectedTag) ->
                    viewTagSettings maybeSelectedTag model
    in
    { title = "LunarTasks"
    , body = [ viewLayout model innerContent ]
    }


toggleNoteEdit : ViewState -> ViewState
toggleNoteEdit viewState =
    case viewState of
        LoadedTasksView loadedTasksViewState ->
            case loadedTasksViewState of
                EditTaskView editingNotes ->
                    LoadedTasksView <|
                        EditTaskView (toggle editingNotes)

                _ ->
                    viewState

        _ ->
            viewState


viewTagSettings : Maybe String -> Model -> Element Msg
viewTagSettings maybeSelectedTag model =
    let
        remainingTasksWithTag : List LunarTask -> String -> Bool
        remainingTasksWithTag tasks tagName =
            let
                tagMatch =
                    BitFlags.match model.tagSettings [ tagName ] []
            in
            List.any (\t -> tagMatch t.bitTags) tasks

        populateRows : Maybe String -> List (Html Msg)
        populateRows maybeTag =
            case maybeTag of
                Just tag ->
                    List.map
                        (\tagName ->
                            let
                                tagNameTd : Html Msg
                                tagNameTd =
                                    if tag == tagName then
                                        td []
                                            [ Html.input
                                                [ type_ "text"
                                                , value model.tagNameInput
                                                , Html.Events.onInput UpdatedTagNameInput
                                                ]
                                                []
                                            , Html.button [ Html.Events.onClick (UpdateTag tagName) ] [ Html.text "Save" ]
                                            , Html.button [ Html.Events.onClick (SelectTagToEdit Nothing) ] [ Html.text "Cancel" ]
                                            ]

                                    else
                                        td
                                            [ Html.Attributes.class "embolden"
                                            , Html.Events.onClick (SelectTagToEdit Nothing)
                                            ]
                                            [ Html.text tagName ]

                                allowDeleteTd =
                                    td
                                        []
                                        [ Icon.trash2
                                            |> Icon.toHtml
                                                [ Html.Events.onClick (DeleteTag tagName)
                                                , Html.Attributes.style "cursor" "pointer"
                                                ]
                                        ]

                                preventDeleteTd =
                                    td
                                        [ Html.Attributes.title "tasks are still associated with this tag"
                                        ]
                                        [ Icon.alertOctagon |> Icon.toHtml [] ]

                                allowOrPreventTd =
                                    if remainingTasksWithTag model.tasks tagName then
                                        preventDeleteTd

                                    else
                                        allowDeleteTd
                            in
                            tr []
                                [ tagNameTd
                                , allowOrPreventTd
                                ]
                        )
                        (BitFlags.allFlags model.tagSettings)

                Nothing ->
                    List.map
                        (\tagName ->
                            let
                                tagNameTd : Html Msg
                                tagNameTd =
                                    td
                                        [ Html.Attributes.class "embolden"
                                        , Html.Events.onClick (SelectTagToEdit (Just tagName))
                                        ]
                                        [ Html.text tagName ]

                                allowDeleteTd =
                                    td
                                        []
                                        [ Icon.trash2
                                            |> Icon.toHtml
                                                [ Html.Events.onClick (DeleteTag tagName)
                                                , Html.Attributes.style "cursor" "pointer"
                                                ]
                                        ]

                                preventDeleteTd =
                                    td
                                        [ Html.Attributes.title "tasks are still associated with this tag"
                                        ]
                                        [ Icon.alertOctagon |> Icon.toHtml [] ]

                                allowOrPreventTd =
                                    if remainingTasksWithTag model.tasks tagName then
                                        preventDeleteTd

                                    else
                                        allowDeleteTd
                            in
                            tr []
                                [ tagNameTd
                                , allowOrPreventTd
                                ]
                        )
                        (BitFlags.allFlags model.tagSettings)
    in
    case maybeSelectedTag of
        Just selectedTag ->
            el [ width fill ] <|
                Element.html <|
                    Html.div []
                        [ Html.table []
                            [ Html.thead []
                                [ tr
                                    []
                                    [ th
                                        [ Html.Attributes.style "text-align" "left"
                                        , Html.Events.onClick (SelectTagToEdit Nothing)
                                        ]
                                        [ Html.text "Tag Name" ]
                                    , th [] []
                                    ]
                                ]
                            , Html.tbody [ Html.Attributes.id "tag-table-body" ] (populateRows (Just selectedTag))
                            ]
                        ]

        Nothing ->
            el [ width fill ] <|
                Element.html <|
                    Html.div []
                        [ Html.table []
                            [ Html.thead []
                                [ tr
                                    []
                                    [ th
                                        [ Html.Attributes.style "text-align" "left"
                                        , Html.Events.onClick (SelectTagToEdit Nothing)
                                        ]
                                        [ Html.text "Tag Name" ]
                                    , th [] []
                                    ]
                                ]
                            , Html.tbody [ Html.Attributes.id "tag-table-body" ] (populateRows Nothing)
                            ]
                        , Html.div []
                            [ Html.input [ Html.Attributes.type_ "text", Html.Attributes.placeholder "New Tag Name", Html.Attributes.value model.tagNameInput, Html.Events.onInput UpdatedTagNameInput ] []
                            , Icon.plusCircle |> Icon.toHtml [ Html.Events.onClick CreateTag ]
                            ]
                        ]


viewTasksJson : Model -> Element Msg
viewTasksJson model =
    let
        encodedList =
            Encode.list lunarTaskEncoder model.tasks
    in
    Element.html <| Html.pre [] [ Html.text (Encode.encode 2 encodedList) ]


viewTask : Model -> Bool -> Element Msg
viewTask model editingNotes =
    let
        notesField notes =
            let
                editableField =
                    Input.multiline
                        [ Element.Events.onLoseFocus ToggleNoteEdit
                        , htmlAttribute (Html.Attributes.id "notes-input")
                        , Element.alignLeft
                        , Element.alignTop
                        , Element.paddingEach { top = 0, bottom = 160, left = 0, right = 0 }
                        ]
                        { label = Input.labelHidden "notes"
                        , onChange = \x -> EditTaskEffect (UpdateEditTaskNotes x)
                        , text = notes
                        , spellcheck = True
                        , placeholder = Nothing
                        }

                renderedField =
                    Markdown.default notes
                        |> Result.map
                            (Element.column
                                [ Element.width Element.fill
                                , Border.width 1
                                , Element.alignLeft
                                , Element.alignTop
                                , Element.paddingEach { top = 5, bottom = 160, left = 10, right = 10 }
                                ]
                            )
                        |> Result.withDefault (el [] <| text "Error rendering markdown")
            in
            if editingNotes then
                editableField

            else
                renderedField

        buttonAttrs =
            [ Font.semiBold
            , Font.size 15
            , height (px 42)
            , paddingXY 15 0
            , Font.center
            , Border.width 1
            , Border.color color.darkCharcoal
            , Border.rounded 3
            , Border.shadow
                { offset = ( 0, 0.2 )
                , size = 0.05
                , blur = 0
                , color = color.darkCharcoal
                }
            , Font.family
                [ Font.typeface "Roboto"
                , Font.sansSerif
                ]
            , Font.color color.darkCharcoal
            , Background.color color.white
            ]
    in
    case model.editedTask of
        Nothing ->
            el [] (text "no task found")

        Just task ->
            let
                buildSeasonalSliderInput inputVal msg label =
                    Input.slider
                        [ Element.behindContent
                            (Element.el
                                [ Element.width Element.fill
                                , Element.height (Element.px 2)
                                , Element.centerY
                                , Background.color color.lightGrey
                                , Border.rounded 2
                                ]
                                Element.none
                            )
                        ]
                        { label =
                            Input.labelAbove
                                [ Font.semiBold ]
                                (text <| label)
                        , onChange = msg
                        , min = 2
                        , max = 360
                        , value = toFloat inputVal
                        , step = Just 1
                        , thumb = Input.defaultThumb
                        }

                taskTypeInput =
                    case task.taskType of
                        AllYear ->
                            Input.text []
                                { label =
                                    Input.labelAbove
                                        [ Font.semiBold
                                        ]
                                        (text "Period (in days)")
                                , onChange = \x -> EditTaskEffect (UpdateEditTaskPeriod x)
                                , text = String.fromInt task.period
                                , placeholder = Nothing
                                }

                        Seasonal seasonStart seasonEnd ->
                            let
                                seasonalData =
                                    getSeasonalData
                                        model.currentDate
                                        seasonStart
                                        seasonEnd

                                seasonMessage =
                                    case seasonalData of
                                        CurrentSeason start end ->
                                            "This season started on "
                                                ++ Date.toIsoString start
                                                ++ " and ends on "
                                                ++ Date.toIsoString end

                                        NextSeason start end ->
                                            "This season starts on "
                                                ++ Date.toIsoString start
                                                ++ " and ends on "
                                                ++ Date.toIsoString end
                            in
                            column []
                                [ Input.text []
                                    { label =
                                        Input.labelAbove
                                            [ Font.semiBold
                                            ]
                                            (text "Period (in days)")
                                    , onChange = \x -> EditTaskEffect (UpdateEditTaskPeriod x)
                                    , text = String.fromInt task.period
                                    , placeholder = Nothing
                                    }
                                , buildSeasonalSliderInput
                                    seasonStart
                                    (\x -> EditTaskEffect (UpdateSeasonStart x))
                                    "Season Start Date"
                                , buildSeasonalSliderInput
                                    seasonEnd
                                    (\x -> EditTaskEffect (UpdateSeasonEnd x))
                                    "Season End Date"
                                , el [] (text <| seasonMessage)
                                ]

                historicalCadenceMsg =
                    case getHistoricalCadence task of
                        Nothing ->
                            ""

                        Just historicalCadence ->
                            "This task is completed on average once every " ++ String.fromInt historicalCadence ++ " days"

                getUnexpiredManualPastDueDate : Maybe Date.Date -> Date.Date -> Maybe Date.Date
                getUnexpiredManualPastDueDate maybeManualPastDueDate lastCompletedAt =
                    Maybe.andThen
                        (\manualPastDueDate ->
                            case Date.compare lastCompletedAt manualPastDueDate of
                                GT ->
                                    Nothing

                                EQ ->
                                    Nothing

                                LT ->
                                    Just manualPastDueDate
                        )
                        maybeManualPastDueDate
            in
            Element.column [ paddingXY 100 45, spacingXY 0 25 ]
                [ Input.text []
                    { label = Input.labelAbove [ Font.semiBold ] (text "Title")
                    , onChange = \x -> EditTaskEffect (UpdateEditTaskTitle x)
                    , text = task.title
                    , placeholder = Nothing
                    }
                , row [ spacingXY 15 0 ]
                    [ button buttonAttrs { label = text "Save", onPress = Just (EditTaskEffect Save) }
                    , button buttonAttrs { label = text "Cancel", onPress = Just (EditTaskEffect Cancel) }
                    ]
                , Input.radioRow
                    [ padding 10
                    , spacing 20
                    ]
                    { onChange = \x -> EditTaskEffect (UpdateTaskType x)
                    , selected = Just (getTaskTypeOption task)
                    , label = Input.labelAbove [ Font.semiBold ] (text "Cadence")
                    , options =
                        [ Input.option AllYearOption (text "All Year")
                        , Input.option SeasonalOption (text "Seasonal")
                        ]
                    }
                , taskTypeInput
                , case getUnexpiredManualPastDueDate task.manualPastDueDate (getLastCompletedAt task) of
                    Just date ->
                        row []
                            [ el [ Font.bold ] (text <| "Task is manually set to be past due on " ++ Date.toIsoString date)
                            , text " -- "
                            , button []
                                { onPress = Just (EditTaskEffect RemoveManualPastDueDate)
                                , label = Icon.trash2 |> Icon.toHtml [] |> Element.html
                                }
                            ]

                    Nothing ->
                        column []
                            [ el [ Font.bold ] (text "Ignore settings and make past due on")
                            , el [ Border.width 1, paddingXY 10 10, Border.color color.lightGrey ] <|
                                (DatePicker.view Nothing
                                    { datePickerSettings | placeholder = "Manual past due date" }
                                    model.datePickerForManualPastDue
                                    |> Html.map (\x -> EditTaskEffect (SetManualPastDueDate x))
                                    |> Element.html
                                )
                            ]
                , if editingNotes then
                    el [ Font.bold ] (text "Notes")

                  else
                    row []
                        [ el [ Font.bold ] (text "Notes ")
                        , el [ onClick ToggleNoteEdit ]
                            (Icon.edit
                                |> Icon.toHtml [ Html.Attributes.style "cursor" "pointer" ]
                                |> Element.html
                            )
                        ]
                , notesField task.notes
                , column []
                    [ el [ Font.bold ] (text "Tags")
                    , SearchBox.input []
                        { onChange = \x -> EditTaskEffect (ChangedTagSearchBox x)
                        , text = model.tagSearchBoxText
                        , filter =
                            \query optionStr ->
                                String.contains query optionStr
                        , selected = model.tagSearchBoxSelected
                        , placeholder = Just (Input.placeholder [] <| text "Create Tags from Main")
                        , state = model.tagSearchBox
                        , label = Input.labelHidden "tag selection"
                        , toLabel =
                            \str -> str
                        , options =
                            Just <|
                                Set.toList <|
                                    Set.diff
                                        (Set.fromList <| BitFlags.allFlags model.tagSettings)
                                        (Set.fromList <| BitFlags.enabledFlags model.tagSettings task.bitTags)
                        }
                    , Element.column [ spacingXY 0 15, paddingXY 0 15 ] (viewEnabledTasks task model)
                    ]
                , column []
                    [ el [ Font.bold ] (text "Completion Entries")
                    , el [] (text historicalCadenceMsg)
                    , el [ Border.width 1, paddingXY 10 10, Border.color color.lightGrey ] <|
                        (DatePicker.view Nothing
                            datePickerSettings
                            model.datePicker
                            |> Html.map (\x -> EditTaskEffect (AddCompletionEntry x))
                            |> Element.html
                        )
                    , Element.column [ spacingXY 0 15, paddingXY 0 15 ] (viewCompletionEntries task)
                    ]
                ]


viewEnabledTasks : LunarTask -> Model -> List (Element Msg)
viewEnabledTasks task model =
    BitFlags.enabledFlags model.tagSettings task.bitTags
        |> List.map enabledFlagEntryToListedItem


enabledFlagEntryToListedItem : String -> Element Msg
enabledFlagEntryToListedItem flag =
    Element.row []
        [ text flag
        , text " -- "
        , button []
            { onPress = Just <| EditTaskEffect <| DisableTag flag
            , label = Icon.trash2 |> Icon.toHtml [] |> Element.html
            }
        ]


viewCompletionEntries : LunarTask -> List (Element Msg)
viewCompletionEntries task =
    task.completionEntries
        |> List.map completedEntryToListedItem


completedEntryToListedItem : Date.Date -> Element Msg
completedEntryToListedItem entryTime =
    Element.row []
        [ text (Date.toIsoString entryTime ++ " -- ")
        , button []
            { onPress = Just <| EditTaskEffect <| RemoveCompletionEntry entryTime
            , label = Icon.trash2 |> Icon.toHtml [] |> Element.html
            }
        ]


viewLandingBulletPoint : Icon -> String -> Element msg
viewLandingBulletPoint icon message =
    let
        elIcon =
            icon
                |> Icon.withSize 2
                |> Icon.withSizeUnit "em"
                |> Icon.toHtml []
                |> Element.html
    in
    column
        [ height fill
        , spacingXY 0 30
        , paddingXY 0 30
        ]
        [ el [ centerX ] elIcon
        , paragraph [ width (fill |> minimum 350), paddingXY 10 0 ] [ text message ]
        ]


viewLandingPage : Element Msg
viewLandingPage =
    wrappedRow
        [ paddingXY 175 100
        , Font.size 28
        , Font.italic
        , Font.extraLight
        , Font.color color.darkBlue
        , Font.family
            [ Font.sansSerif
            , Font.typeface "Roboto"
            ]
        , spaceEvenly
        ]
        [ viewLandingBulletPoint
            Icon.archive
            "For recurring, non urgent tasks (that eventually become urgent)."
        , viewLandingBulletPoint
            Icon.bellOff
            "From lubricating garage door springs to shampooing unsoiled carpets, most tasks don't need to be completed in a timely fashion."
        , viewLandingBulletPoint
            Icon.calendar
            "Not every chore can or should be managed in a strict calendar week or month. Some tasks end up following more of a lunar calendar trajectory, shifting due dates throughout the year."
        , viewLandingBulletPoint
            Icon.repeat
            "Create a task, set the cadence and mark past due tasks completed. Track completion entries and observe actual vs stated cadences."
        , viewLandingBulletPoint
            Icon.copy
            "Fully exportable collection to JSON. Peace of mind with no platform lock."
        , viewLandingBulletPoint
            Icon.compass
            "When you're ready to roll up your sleeves and do some chores, LunarTasks will let you know which tasks are ready to get after (again!)."
        ]


generateSortRowRadioOptions : ListSort -> List (Input.Option ListSort msg)
generateSortRowRadioOptions listSort =
    [ SortPastDueDays DESC
    , SortPastDueDays ASC
    , SortLastCompleted DESC
    , SortLastCompleted ASC
    , SortPastDuePeriods DESC
    , SortPastDuePeriods ASC
    , NoSort ASC
    , NoSort DESC
    ]
        |> List.filter (isMatchingSortOrder (sortOrderFromListSort listSort))
        |> List.map (\x -> Input.optionWith x <| radioOption (listSortToText x) (Just <| sortOrderFromListSort listSort))


radioOption : String -> Maybe SortOrder -> Input.OptionState -> Element msg
radioOption label maybeSortOrder state =
    row [ spacing 10 ]
        [ el
            [ width <| px 30
            , height <| px 30
            , centerY
            , padding 1
            , Border.rounded 6
            , Border.width 2
            , Border.color <| rgb255 0xC0 0xC0 0xC0
            ]
          <|
            el
                [ width fill
                , height fill
                , Border.rounded 4
                , Background.color <|
                    case state of
                        Input.Idle ->
                            rgba255 0xFF 0xFF 0xFF 0.1

                        Input.Focused ->
                            rgba255 0x72 0x9F 0xCF 0.1

                        Input.Selected ->
                            color.lightBlue
                ]
                (case state of
                    Input.Selected ->
                        case maybeSortOrder of
                            Nothing ->
                                Element.none

                            Just ASC ->
                                Icon.trendingUp |> Icon.toHtml [ Html.Attributes.style "color" "gray" ] |> Element.html

                            Just DESC ->
                                Icon.trendingDown |> Icon.toHtml [ Html.Attributes.style "color" "gray" ] |> Element.html

                    _ ->
                        Element.none
                )
        , text label
        ]


viewTaskDiscovery : Model -> Element Msg
viewTaskDiscovery model =
    let
        maybeSavedView =
            findMatchingSavedView (currentView model) model.savedViews

        filterRow =
            Input.radioRow [ spacingXY 25 0, Font.semiBold ]
                { onChange = SelectFilter
                , selected = Just model.filter
                , label = Input.labelLeft [ Font.semiBold, paddingEach { top = 0, left = 0, bottom = 0, right = 15 } ] (text "Filter By: ")
                , options =
                    [ Input.optionWith FilterPastDue <| radioOption "Past Due" Nothing
                    , Input.optionWith FilterNonPastDue <| radioOption "Not Past Due" Nothing
                    , Input.optionWith FilterAll <| radioOption "All" Nothing
                    ]
                }

        sortRow =
            Input.radioRow [ spacingXY 15 0, Font.semiBold ]
                { onChange = SelectSort
                , selected = Just model.sort
                , label =
                    Input.labelLeft
                        [ Font.semiBold
                        , paddingEach { top = 0, left = 0, bottom = 0, right = 15 }
                        ]
                        (text "Sort By: ")
                , options = generateSortRowRadioOptions model.sort
                }

        searchTermInput =
            Input.text
                [ Border.width 0
                , Element.htmlAttribute <| Html.Attributes.id "search-term"
                ]
                { text = Maybe.withDefault "" model.searchTerm
                , onChange = Search
                , placeholder = Just <| Input.placeholder [] <| (Icon.search |> Icon.toHtml [] |> Element.html)
                , label = Input.labelHidden "search"
                }

        saveViewBtn =
            if not <| currentViewIsSavedView model then
                let
                    currentViewisDefaultView =
                        savedViewMatch defaultSavedView (currentView model)
                in
                button
                    [ centerY
                    , transparent currentViewisDefaultView
                    ]
                    { onPress = Just SavedViewAdd
                    , label = Icon.star |> Icon.toHtml [] |> Element.html
                    }

            else
                let
                    savedViewTitle =
                        case maybeSavedView of
                            Nothing ->
                                ""

                            Just savedView ->
                                Maybe.withDefault "" savedView.title
                in
                row []
                    [ button
                        [ centerY
                        ]
                        { onPress = Just (SavedViewEditTitle savedViewTitle)
                        , label = Icon.edit |> Icon.toHtml [] |> Element.html
                        }
                    , button
                        [ centerY
                        ]
                        { onPress = Just SavedViewRemove
                        , label = Icon.trash2 |> Icon.toHtml [] |> Element.html
                        }
                    ]

        clearSearchBtn =
            button
                [ transparent (Maybe.withDefault "" model.searchTerm == "")
                , centerY
                ]
                { onPress = Just ClearSearch
                , label = Icon.x |> Icon.toHtml [] |> Element.html
                }

        savedViewDropdown =
            Dropdown.view savedViewDropdownConfig model model.savedViewDropdownState

        getTagState : String -> TagToggleState
        getTagState tag =
            let
                showEnabledTags =
                    model.tagSettings
                        |> BitFlags.enabledFlags

                ( whitelistedTags, blacklistedTags ) =
                    model.tagsSelected
                        |> Tuple.mapBoth showEnabledTags showEnabledTags
                        |> Tuple.mapBoth Set.fromList Set.fromList
            in
            if Set.member tag whitelistedTags then
                Whitelisted

            else if Set.member tag blacklistedTags then
                Blacklisted

            else
                Unselected

        tagSettingsBtn =
            Icon.settings
                |> Icon.withSize 2
                |> Icon.withSizeUnit "em"
                |> Icon.toHtml
                    [ Html.Events.onClick EditTags
                    , Html.Attributes.style "cursor" "pointer"
                    ]
                |> Element.html

        allTags =
            BitFlags.allFlags model.tagSettings

        tagsRow =
            if not model.tagResourcesLoaded then
                Element.none

            else if List.length allTags <= 0 then
                wrappedRow [ spacingXY 10 5 ]
                    [ Element.html <|
                        Html.p
                            [ Html.Attributes.style "cursor" "pointer"
                            , Html.Attributes.style "text-decoration" "underline"
                            , Html.Events.onClick EditTags
                            ]
                            [ Html.text "Create Tags" ]
                    ]

            else
                wrappedRow [ spacingXY 10 5 ]
                    (List.map (\t -> viewTagButton (getTagState t) t)
                        allTags
                        ++ [ tagSettingsBtn ]
                    )

        savedViewsRow =
            case model.view of
                LoadedTasksView (MainTasksView EditSavedViewTitle) ->
                    case maybeSavedView of
                        Nothing ->
                            el [] (text "woops")

                        Just _ ->
                            Element.html <|
                                Html.div []
                                    [ Html.input
                                        [ Html.Attributes.type_ "text"
                                        , Html.Attributes.placeholder "Saved View Title"
                                        , Html.Attributes.value model.savedViewTitleInput
                                        , Html.Events.onInput SavedViewUpdateTitleInput
                                        ]
                                        []
                                    , Icon.plusCircle |> Icon.toHtml [ Html.Events.onClick SavedViewUpdateTitle ]
                                    ]

                _ ->
                    row [] [ row [ Border.width 1 ] [ savedViewDropdown ], saveViewBtn ]
    in
    column [ spacingXY 0 15 ]
        [ filterRow
        , sortRow
        , row [ Border.width 1, paddingXY 10 7 ]
            [ searchTermInput
            , clearSearchBtn
            ]
        , tagsRow
        , savedViewsRow
        ]


viewMain : Model -> Element Msg
viewMain model =
    let
        enabledFlags =
            model.tagSettings
                |> BitFlags.enabledFlags

        ( whitelistedTags, blacklistedTags ) =
            model.tagsSelected
                |> Tuple.mapBoth enabledFlags enabledFlags

        tasks =
            model.tasks
                |> filterByTags (BitFlags.match model.tagSettings whitelistedTags blacklistedTags)
                |> filterTaskList model.filter model.currentDate
                |> filterByTerm model.searchTerm
                |> sortTaskList model.sort model.currentDate

        taskTable =
            if List.length tasks == 0 then
                el
                    [ Font.size 32
                    , Font.center
                    , width fill
                    , paddingXY 20 50
                    ]
                <|
                    text "No Tasks Found"

            else
                viewTaskTable model.currentDate tasks
    in
    Element.column [ spacingXY 0 30, paddingXY 40 20, width fill ]
        [ viewNewTask model
        , viewTaskDiscovery model
        , taskTable
        ]


type alias Colors =
    { blue : Color
    , darkCharcoal : Color
    , green : Color
    , lightBlue : Color
    , darkBlue : Color
    , lightGrey : Color
    , orange : Color
    , red : Color
    , white : Color
    , grey : Color
    , googleLogin : Color
    }


color : Colors
color =
    { blue = rgb255 0x72 0x9F 0xCF
    , darkCharcoal = rgb255 0x2E 0x34 0x36
    , green = rgb255 0x20 0xBF 0x55
    , lightBlue = rgb255 0xC5 0xE8 0xF7
    , darkBlue = rgb255 0x1C 0x26 0x66
    , lightGrey = rgb255 0xE0 0xE0 0xE0
    , grey = rgb255 0x8D 0x8D 0x8D
    , googleLogin = rgb255 0x5F 0x63 0x68
    , orange = rgb255 0xF2 0x64 0x19
    , red = rgb255 0xAA 0x00 0x00
    , white = rgb255 0xFF 0xFF 0xFF
    }


viewTaskTable : Date -> List LunarTask -> Element Msg
viewTaskTable currentDate tasks =
    let
        periodsLapsedMessage : Date.Date -> LunarTask -> String
        periodsLapsedMessage date task =
            let
                periodsLapsed =
                    periodsPastDue date task
                        |> round
                        |> String.fromInt
            in
            if pastDue date task then
                periodsLapsed ++ " full periods lapsed"

            else
                ""

        populateRows : List LunarTask -> List (Html Msg)
        populateRows data =
            List.map
                (\task ->
                    let
                        pastDueTask =
                            pastDue currentDate task

                        pastDueTd =
                            if pastDueTask then
                                td
                                    [ Html.Attributes.title
                                        (periodsLapsedMessage currentDate task)
                                    ]
                                    [ Html.text <| String.fromInt (getDaysPastDue currentDate task) ]

                            else
                                td [] []

                        lastCompletedTd =
                            if pastDueTask then
                                td []
                                    [ Html.text <| Date.toIsoString (getLastCompletedAt task) ]

                            else
                                td
                                    [ Html.Attributes.title
                                        ("This task will be past due again on " ++ Date.toIsoString (getNextPastDueDate task))
                                    ]
                                    [ Html.text <| Date.toIsoString (getLastCompletedAt task) ]
                    in
                    tr []
                        [ td
                            [ Html.Attributes.style "cursor" "pointer"
                            , Html.Events.onClick (EditTaskEffect (EditTask task.id))
                            , Html.Attributes.class "embolden"
                            , Html.Attributes.title task.notes
                            ]
                            [ Html.text task.title ]
                        , pastDueTd
                        , td []
                            [ Html.text <| String.fromInt task.period ]
                        , lastCompletedTd
                        , td
                            [ Html.Attributes.style "cursor" "pointer"
                            , Html.Attributes.style "text-align" "center"
                            , Html.Attributes.class "embolden"
                            , Html.Events.onClick (MarkTaskCompleted task currentDate)
                            ]
                            [ Html.div
                                [ Html.Attributes.class "selective-icon-opts"
                                , Html.Attributes.class "selective-icon-opts-checkbox"
                                , Html.Attributes.title "Mark Task Completed"
                                ]
                                [ Html.div [ Html.Attributes.class "selective-icon-activated" ] [ Icon.checkSquare |> Icon.toHtml [] ]
                                , Html.div [ Html.Attributes.class "selective-icon-inactivated" ] [ Icon.square |> Icon.toHtml [] ]
                                ]
                            ]
                        , td
                            [ Html.Attributes.style "cursor" "pointer"
                            , Html.Attributes.style "text-align" "center"
                            , Html.Attributes.class "embolden"
                            , Html.Events.onClick (DeleteTask task)
                            ]
                            [ Html.div
                                [ Html.Attributes.class "selective-icon-opts"
                                , Html.Attributes.class "selective-icon-opts-checkbox"
                                , Html.Attributes.title "Delete Task"
                                ]
                                [ Html.div [ Html.Attributes.class "selective-icon-activated" ] [ Icon.trash2 |> Icon.toHtml [] ]
                                , Html.div [ Html.Attributes.class "selective-icon-inactivated" ] [ Icon.trash |> Icon.toHtml [] ]
                                ]
                            ]
                        ]
                )
                data
    in
    el [ width fill ] <|
        Element.html <|
            Html.table []
                [ Html.thead []
                    [ tr
                        []
                        [ th [ Html.Attributes.style "text-align" "left" ]
                            [ Html.text "Task" ]
                        , th [ Html.Attributes.style "text-align" "left" ] [ Html.text "Days Past Due" ]
                        , th [ Html.Attributes.style "text-align" "left" ] [ Html.text "Cadence" ]
                        , th [ Html.Attributes.style "text-align" "left" ]
                            [ Html.text "Last Completed" ]
                        , th [] [ Html.text "" ]
                        , th [] [ Html.text "" ]
                        ]
                    ]
                , Html.tbody [ Html.Attributes.id "task-table-body" ] (populateRows tasks)
                ]


type TagToggleState
    = Whitelisted
    | Blacklisted
    | Unselected


viewTagButton : TagToggleState -> String -> Element Msg
viewTagButton tagToggleState tag =
    let
        baseButtonAttrs =
            [ Font.semiBold
            , Font.size 15
            , height (px 42)
            , paddingXY 15 0
            , Font.center
            , Border.width 1
            , Border.color color.darkCharcoal
            , Border.rounded 3
            , Border.shadow
                { offset = ( 0, 0.2 )
                , size = 0.05
                , blur = 0
                , color = color.darkCharcoal
                }
            , Font.family
                [ Font.typeface "Roboto"
                , Font.sansSerif
                ]
            ]
    in
    case tagToggleState of
        Unselected ->
            let
                unSelectedAttrs =
                    [ Background.color color.white
                    , Font.color color.darkCharcoal
                    ]
                        ++ baseButtonAttrs
            in
            button unSelectedAttrs { onPress = Just (ToggleTag tag), label = text tag }

        Whitelisted ->
            let
                whitelistedAttrs =
                    [ Background.color color.darkCharcoal
                    , Font.color color.white
                    ]
                        ++ baseButtonAttrs
            in
            button whitelistedAttrs { onPress = Just (ToggleTag tag), label = text tag }

        Blacklisted ->
            let
                blacklistedAttrs =
                    [ Background.color color.darkCharcoal
                    , Font.color color.white
                    , Font.strike
                    ]
                        ++ baseButtonAttrs
            in
            button blacklistedAttrs { onPress = Just (ToggleTag tag), label = text tag }


viewNewTaskCreateBtn : Model -> Element Msg
viewNewTaskCreateBtn model =
    if newLunarTaskReady model then
        button [ alignBottom, paddingXY 0 5 ]
            { onPress = Just (NewTaskEffect NewTaskSubmit)
            , label =
                Icon.plusCircle
                    |> Icon.withSize 2
                    |> Icon.withSizeUnit "em"
                    |> Icon.toHtml []
                    |> Element.html
            }

    else
        button [ alignBottom, paddingXY 0 5 ]
            { onPress = Nothing
            , label =
                Icon.plusCircle
                    |> Icon.withSize 2
                    |> Icon.withSizeUnit "em"
                    |> Icon.withClass "disabled-new-task"
                    |> Icon.toHtml [ Html.Attributes.style "cursor" "default" ]
                    |> Element.html
            }


viewNewTask : Model -> Element Msg
viewNewTask model =
    el [ paddingXY 3 3, Border.solid, Border.width 1, width fill ] <|
        row [ Font.size 22, Border.width 1, spacing 25, paddingXY 15 20, width fill ]
            [ Input.text []
                { placeholder = Nothing
                , label = Input.labelAbove [ Font.bold ] <| text "Title"
                , text = model.newTaskTitle
                , onChange = \x -> NewTaskEffect (UpdateNewTaskTitle x)
                }
            , Input.text []
                { placeholder = Nothing
                , label = Input.labelAbove [ Font.bold ] <| text "Cadence (in days)"
                , text = String.fromInt model.newTaskPeriod
                , onChange = \x -> NewTaskEffect (UpdateNewTaskPeriod x)
                }
            , Input.text []
                { placeholder = Nothing
                , label = Input.labelAbove [ Font.bold ] <| text "Notes"
                , text = model.newTaskNotes
                , onChange = \x -> NewTaskEffect (UpdateNewTaskNotes x)
                }
            , column []
                [ el [ Font.bold, paddingXY 0 4 ] (text "Date of Last Completion")
                , el [ Border.width 1, paddingXY 10 10, Border.color color.lightGrey ] <|
                    (DatePicker.view (Just model.newTaskCompletedAt)
                        datePickerSettings
                        model.datePicker
                        |> Html.map (\x -> NewTaskEffect (SetNewTaskDatePicker x))
                        |> Element.html
                    )
                ]
            , viewNewTaskCreateBtn model
            ]



-- MISC HELPERS


isPresent : Maybe a -> Bool
isPresent item =
    case item of
        Nothing ->
            False

        Just _ ->
            True


toggle : Bool -> Bool
toggle a =
    not a
