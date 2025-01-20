port module Main exposing (..)

import BitFlags exposing (BitFlagSettings)
import Browser exposing (UrlRequest(..))
import Browser.Dom as Dom
import Browser.Events exposing (Visibility(..), onVisibilityChange)
import Browser.Navigation as Nav
import Date exposing (Date, Unit(..))
import DatePicker exposing (defaultSettings)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input exposing (OptionState(..), Thumb, button)
import FeatherIcons exposing (key)
import Html exposing (Html, hr, td, th, tr)
import Html.Attributes exposing (hidden, style, type_, value)
import Html.Events exposing (onBlur)
import Http
import Json.Decode as Decode exposing (Decoder, errorToString)
import Json.Encode as Encode
import Keyboard exposing (Key(..))
import List
import ListSettings exposing (..)
import LunarTask exposing (..)
import Markdown.Renderer.ElmUi as Markdown
import Material.Icons
import Material.Icons.Types exposing (Coloring(..))
import NewLunarTask exposing (..)
import OUI.Button as Button
import OUI.Icon
import OUI.Image as Image
import OUI.Material as Material
import OUI.Material.Color as Color
import OUI.Material.Theme as Theme
import OUI.Text as Text
import Process
import SHA1
import SearchBox
import Set exposing (Set)
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
    , tagsSelected : ( Set String, Set String )
    , searchTerm : Maybe String
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


type LoadedTasksViewState
    = JsonExportView
    | MainTasksView
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
            , filter = FilterAll
            , sort = NoSort DESC
            , tagsSelected = ( Set.empty, Set.empty )
            , tagSettings = BitFlags.defaultSettings 25
            , searchTerm = Nothing
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
        , Cmd.map NewTaskSetDatePicker datePickerCmd
        , Cmd.map EditTaskSetManualPastDueDate datePickerCmdForManualPastDue
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
    | FilterReset
    | LocalStoreFetched Decode.Value
    | ToggleSortOrder
    | ToggleNoteEdit
    | SelectFilter ListFilter
    | SelectSort ListSort
    | ViewChange ViewState
    | MarkCompleted LunarTask Date.Date
    | CreateTask
    | NewTaskUpdateTitle String
    | NewTaskUpdateNotes String
    | NewTaskUpdatePeriod String
    | NewTaskSetDatePicker DatePicker.Msg
    | EditTaskPeriod String
    | EditSeasonStart Float
    | EditSeasonEnd Float
    | EditTaskDisableTag String
    | EditTaskEnableTag String
    | EditTaskRemoveManualPastDueDate
    | EditTaskSetManualPastDueDate DatePicker.Msg
    | EditTaskTitle String
    | EditTaskNotes String
    | EditTaskRemoveCompletionEntry Date.Date
    | EditTaskAddCompletionEntry DatePicker.Msg
    | EditTaskCancel
    | EditTaskSave
    | EditTaskType AllYearOrSeasonalOption
    | EditTask String
    | EditTaskChangedTagSearchBox (SearchBox.ChangeEvent String)
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
        createTask : Encode.Value -> Cmd msg
        createTask encodedTask =
            taskAction ( "create", encodedTask )

        updateTask : Encode.Value -> Cmd msg
        updateTask encodedTask =
            taskAction ( "update", encodedTask )

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

        ClearBanner ->
            ( { model | banner = "" }, Cmd.none )

        FilterReset ->
            ( model |> resetFilter, [] )
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

        MarkCompleted task entryDate ->
            let
                jsonTaskMarkedCompleted =
                    lunarTaskEncoder <| markTaskCompleted task (Just entryDate)
            in
            if isPresent model.demo then
                update (TaskUpdated jsonTaskMarkedCompleted) model

            else
                ( model
                , updateTask jsonTaskMarkedCompleted
                )

        EditTask taskId ->
            case findTaskById taskId model.tasks of
                Nothing ->
                    ( model, Cmd.none )

                Just task ->
                    ( { model | view = LoadedTasksView (EditTaskView False), editedTask = Just task }, Cmd.none )

        EditTaskCancel ->
            ( { model | editedTask = Nothing, view = LoadedTasksView MainTasksView }, Cmd.none )

        EditTaskSave ->
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
                                    , view = LoadedTasksView MainTasksView
                                }

                        else
                            ( { model
                                | editedTask = Nothing
                                , view = LoadedTasksView MainTasksView
                              }
                            , updateTask (lunarTaskEncoder editedTask)
                            )

                    else
                        ( { model | editedTask = Nothing, view = LoadedTasksView MainTasksView }, Cmd.none )

        EditTaskRemoveManualPastDueDate ->
            case model.editedTask of
                Just task ->
                    ( { model
                        | editedTask = Just { task | manualPastDueDate = Nothing }
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        EditTaskSetManualPastDueDate datePickerMsg ->
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

        EditTaskType taskTypeOption ->
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

        EditTaskRemoveCompletionEntry entryDate ->
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

        EditTaskAddCompletionEntry datePickerMsg ->
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

        CreateTask ->
            let
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

        NewTaskUpdateTitle title ->
            ( { model | newTaskTitle = title }, Cmd.none )

        NewTaskUpdateNotes notes ->
            ( { model | newTaskNotes = notes }, Cmd.none )

        NewTaskUpdatePeriod rawPeriod ->
            case String.toInt rawPeriod of
                Just period ->
                    ( { model | newTaskPeriod = period }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        EditTaskEnableTag tag ->
            let
                enableTag =
                    BitFlags.enableFlag model.tagSettings tag
            in
            case model.editedTask of
                Just task ->
                    ( { model | editedTask = Just { task | bitTags = enableTag task.bitTags } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        EditTaskDisableTag tag ->
            let
                disableTag =
                    BitFlags.disableFlag model.tagSettings tag
            in
            case model.editedTask of
                Just task ->
                    ( { model | editedTask = Just { task | bitTags = disableTag task.bitTags } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        EditTaskTitle titleStr ->
            case model.editedTask of
                Just task ->
                    ( { model | editedTask = Just { task | title = titleStr } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        EditTaskNotes notes ->
            case model.editedTask of
                Just task ->
                    ( { model | editedTask = Just { task | notes = notes } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        EditTaskPeriod periodString ->
            let
                period =
                    Maybe.withDefault 10 (String.toInt periodString)
            in
            case model.editedTask of
                Just task ->
                    ( { model | editedTask = Just { task | period = period } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        EditSeasonStart seasonStartFloat ->
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

        EditSeasonEnd seasonEndFloat ->
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

        EditTaskChangedTagSearchBox changeEvent ->
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

        DeleteTask task ->
            let
                encodedLunarTask =
                    lunarTaskEncoder task
            in
            if isPresent model.demo then
                update (TaskDeleted encodedLunarTask) model

            else
                ( model, deleteTask encodedLunarTask )

        NewTaskSetDatePicker datePickerMsg ->
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
                                | view = LoadedTasksView MainTasksView
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
                                , view = LoadedTasksView MainTasksView
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
                            | view = LoadedTasksView MainTasksView
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
                    ( { model | view = LoadedTasksView MainTasksView }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- VIEW


theme : Theme.Theme ()
theme =
    Theme.defaultTheme


loginlogoutButton : ViewState -> Element Msg
loginlogoutButton viewType =
    let
        rowSpecs =
            [ alignRight, spacing 15 ]

        loginButton =
            row rowSpecs
                [ Button.new "Sign in with Google"
                    |> Button.onClick (LoginUser 0)
                    |> Material.button theme []
                , Button.new "Demo Mode"
                    |> Button.onClick (DemoLoginUser 0)
                    |> Material.button theme []
                ]

        exportButton =
            case viewType of
                LoadedTasksView MainTasksView ->
                    Button.new "JSON export"
                        |> Button.onClick (ViewChange (LoadedTasksView JsonExportView))
                        |> Material.button theme []

                _ ->
                    Button.new "Return to Main"
                        |> Button.onClick (ViewChange (LoadedTasksView MainTasksView))
                        |> Material.button theme []

        logoutButton =
            row rowSpecs
                [ exportButton
                , Button.new "Log Out"
                    |> Button.onClick (LogOutUser 0)
                    |> Material.button theme []
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
                [ viewMoon
                , Material.text theme (Text.displayMedium "LunarTasks")
                ]
            , loginlogoutButton model.view
            ]
        ]


viewMoon : Element msg
viewMoon =
    FeatherIcons.moon
        |> FeatherIcons.withSize 3
        |> FeatherIcons.withSizeUnit "em"
        |> FeatherIcons.withStrokeWidth 0.8
        |> FeatherIcons.toHtml []
        |> Element.html



-- viewMoon : Element msg
-- viewMoon =
--     Material.icon theme [] (OUI.Icon.elmMaterialIcons Color Material.Icons.dark_mode)


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

                LoadedTasksView MainTasksView ->
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

        populateRows : Maybe String -> List (Element Msg)
        populateRows maybeTag =
            case maybeTag of
                Just tag ->
                    List.map
                        (\tagName ->
                            let
                                tagNameTd : Element Msg
                                tagNameTd =
                                    if tag == tagName then
                                        row []
                                            [ Input.text []
                                                { onChange = UpdatedTagNameInput
                                                , placeholder = Nothing
                                                , label = Input.labelHidden "tagNameText"
                                                , text = tagName
                                                }
                                            , Input.button [] { onPress = Just (UpdateTag tagName), label = text "Save" }
                                            , Input.button [] { onPress = Just (SelectTagToEdit Nothing), label = text "Cancel" }
                                            ]

                                    else
                                        row [ Element.htmlAttribute <| Html.Attributes.class "embolden" ]
                                            [ Input.button [] { onPress = Just (SelectTagToEdit Nothing), label = text tagName }
                                            ]

                                allowDeleteTd =
                                    el []
                                        (FeatherIcons.trash2
                                            |> FeatherIcons.toHtml
                                                [ Html.Events.onClick (DeleteTag tagName)
                                                , Html.Attributes.style "cursor" "pointer"
                                                ]
                                            |> Element.html
                                        )

                                preventDeleteTd =
                                    el
                                        []
                                        (FeatherIcons.alertOctagon
                                            |> FeatherIcons.toHtml
                                                [ Html.Attributes.title "tasks are still associated with this tag"
                                                ]
                                            |> Element.html
                                        )

                                allowOrPreventTd =
                                    if remainingTasksWithTag model.tasks tagName then
                                        preventDeleteTd

                                    else
                                        allowDeleteTd
                            in
                            row []
                                [ tagNameTd
                                , allowOrPreventTd
                                ]
                        )
                        (BitFlags.allFlags model.tagSettings)

                Nothing ->
                    List.map
                        (\tagName ->
                            let
                                tagNameTd : Element Msg
                                tagNameTd =
                                    el
                                        [ Element.htmlAttribute <| Html.Attributes.class "embolden"
                                        , Element.htmlAttribute <| Html.Events.onClick (SelectTagToEdit (Just tagName))
                                        ]
                                        (text tagName)

                                allowDeleteTd : Element Msg
                                allowDeleteTd =
                                    el
                                        []
                                        (FeatherIcons.trash2
                                            |> FeatherIcons.toHtml
                                                [ Html.Events.onClick (DeleteTag tagName)
                                                , Html.Attributes.style "cursor" "pointer"
                                                ]
                                            |> Element.html
                                        )

                                preventDeleteTd =
                                    el
                                        [ Element.htmlAttribute <| Html.Attributes.title "tasks are still associated with this tag"
                                        ]
                                        (FeatherIcons.alertOctagon
                                            |> FeatherIcons.toHtml []
                                            |> Element.html
                                        )

                                allowOrPreventTd =
                                    if remainingTasksWithTag model.tasks tagName then
                                        preventDeleteTd

                                    else
                                        allowDeleteTd
                            in
                            row []
                                [ tagNameTd
                                , allowOrPreventTd
                                ]
                        )
                        (BitFlags.allFlags model.tagSettings)
    in
    case maybeSelectedTag of
        Just selectedTag ->
            column [ width fill ]
                [ row []
                    [ el
                        [ Element.htmlAttribute <| Html.Attributes.style "text-align" "left"
                        , Element.htmlAttribute <| Html.Events.onClick (SelectTagToEdit Nothing)
                        ]
                        (text "Tag Name")
                    , el [] Element.none
                    ]
                , column [ Element.htmlAttribute <| Html.Attributes.id "tag-table-body" ] (populateRows (Just selectedTag))
                ]

        Nothing ->
            column [ width fill ]
                [ row []
                    [ el
                        [ Element.htmlAttribute <| Html.Attributes.style "text-align" "left"
                        , Element.htmlAttribute <| Html.Events.onClick (SelectTagToEdit Nothing)
                        ]
                        (text "Tag Name")
                    , el [] Element.none
                    ]
                , column [ Element.htmlAttribute <| Html.Attributes.id "tag-table-body" ] (populateRows Nothing)
                , row []
                    [ Input.text []
                        { onChange = UpdatedTagNameInput
                        , text = model.tagNameInput
                        , placeholder = Just (Input.placeholder [] <| text "New Tag Name")
                        , label = Input.labelHidden "newTagName"
                        }
                    , el [] (FeatherIcons.plusCircle |> FeatherIcons.toHtml [ Html.Events.onClick CreateTag ] |> Element.html)
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
                        , onChange = EditTaskNotes
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
                                , onChange = EditTaskPeriod
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
                                    , onChange = EditTaskPeriod
                                    , text = String.fromInt task.period
                                    , placeholder = Nothing
                                    }
                                , buildSeasonalSliderInput
                                    seasonStart
                                    EditSeasonStart
                                    "Season Start Date"
                                , buildSeasonalSliderInput
                                    seasonEnd
                                    EditSeasonEnd
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
                    , onChange = EditTaskTitle
                    , text = task.title
                    , placeholder = Nothing
                    }
                , row [ spacingXY 15 0 ]
                    [ button buttonAttrs { label = text "Save", onPress = Just EditTaskSave }
                    , button buttonAttrs { label = text "Cancel", onPress = Just EditTaskCancel }
                    ]
                , Input.radioRow
                    [ padding 10
                    , spacing 20
                    ]
                    { onChange = EditTaskType
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
                                { onPress = Just EditTaskRemoveManualPastDueDate
                                , label = FeatherIcons.trash2 |> FeatherIcons.toHtml [] |> Element.html
                                }
                            ]

                    Nothing ->
                        column []
                            [ el [ Font.bold ] (text "Ignore settings and make past due on")
                            , el [ Border.width 1, paddingXY 10 10, Border.color color.lightGrey ] <|
                                (DatePicker.view Nothing
                                    { datePickerSettings | placeholder = "Manual past due date" }
                                    model.datePickerForManualPastDue
                                    |> Html.map EditTaskSetManualPastDueDate
                                    |> Element.html
                                )
                            ]
                , if editingNotes then
                    el [ Font.bold ] (text "Notes")

                  else
                    row []
                        [ el [ Font.bold ] (text "Notes ")
                        , el [ onClick ToggleNoteEdit ] (FeatherIcons.edit |> FeatherIcons.toHtml [] |> Element.html)
                        ]
                , notesField task.notes
                , column []
                    [ el [ Font.bold ] (text "Tags")
                    , SearchBox.input []
                        { onChange = EditTaskChangedTagSearchBox
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
                            |> Html.map EditTaskAddCompletionEntry
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
            { onPress = Just (EditTaskDisableTag flag)
            , label = FeatherIcons.trash2 |> FeatherIcons.toHtml [] |> Element.html
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
            { onPress = Just (EditTaskRemoveCompletionEntry entryTime)
            , label = FeatherIcons.trash2 |> FeatherIcons.toHtml [] |> Element.html
            }
        ]


viewLandingBulletPoint : FeatherIcons.Icon -> String -> Element msg
viewLandingBulletPoint icon message =
    let
        elIcon =
            icon
                |> FeatherIcons.withSize 2
                |> FeatherIcons.withSizeUnit "em"
                |> FeatherIcons.toHtml []
                |> Element.html
    in
    column
        [ height fill
        , paddingXY 0 30
        , spacingXY 0 30
        ]
        [ el [ centerX ] elIcon
        , paragraph
            [ width (fill |> minimum 350)
            , paddingXY 10 0
            ]
            [ Material.text theme (Text.headlineMedium message) ]
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
            FeatherIcons.archive
            "For recurring, non urgent tasks (that eventually become urgent)."
        , viewLandingBulletPoint
            FeatherIcons.bellOff
            "From lubricating garage door springs to shampooing unsoiled carpets, most tasks don't need to be completed in a timely fashion."
        , viewLandingBulletPoint
            FeatherIcons.calendar
            "Not every chore can or should be managed in a strict calendar week or month. Some tasks end up following more of a lunar calendar trajectory, shifting due dates throughout the year."
        , viewLandingBulletPoint
            FeatherIcons.repeat
            "Create a task, set the cadence and mark past due tasks completed. Track completion entries and observe actual vs stated cadences."
        , viewLandingBulletPoint
            FeatherIcons.copy
            "Fully exportable collection to JSON. Peace of mind with no platform lock."
        , viewLandingBulletPoint
            FeatherIcons.compass
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
                                FeatherIcons.trendingUp |> FeatherIcons.toHtml [ Html.Attributes.style "color" "gray" ] |> Element.html

                            Just DESC ->
                                FeatherIcons.trendingDown |> FeatherIcons.toHtml [ Html.Attributes.style "color" "gray" ] |> Element.html

                    _ ->
                        Element.none
                )
        , text label
        ]


viewTaskDiscovery : Model -> Element Msg
viewTaskDiscovery model =
    let
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
                , placeholder = Just <| Input.placeholder [] <| (FeatherIcons.search |> FeatherIcons.toHtml [] |> Element.html)
                , label = Input.labelHidden "search"
                }

        clearSearchBtn =
            button
                [ transparent (Maybe.withDefault "" model.searchTerm == "")
                , centerY
                ]
                { onPress = Just ClearSearch
                , label = FeatherIcons.x |> FeatherIcons.toHtml [] |> Element.html
                }

        displayResetOption : Bool
        displayResetOption =
            (model.filter == FilterAll)
                && (model.sort == NoSort DESC)
                && (model.searchTerm == Nothing)
                && Set.isEmpty (Tuple.first model.tagsSelected)
                && Set.isEmpty (Tuple.second model.tagsSelected)

        resetOption =
            button
                [ transparent displayResetOption ]
                { label = text "(x) reset all selections", onPress = Just FilterReset }

        getTagState : String -> TagToggleState
        getTagState tag =
            let
                ( whitelistedTags, blacklistedTags ) =
                    model.tagsSelected
            in
            if Set.member tag whitelistedTags then
                Whitelisted

            else if Set.member tag blacklistedTags then
                Blacklisted

            else
                Unselected

        tagSettingsBtn =
            FeatherIcons.settings
                |> FeatherIcons.withSize 2
                |> FeatherIcons.withSizeUnit "em"
                |> FeatherIcons.toHtml
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
    in
    column [ spacingXY 0 15 ]
        [ filterRow
        , sortRow
        , row [ Border.width 1, paddingXY 10 7 ]
            [ searchTermInput
            , clearSearchBtn
            ]
        , tagsRow
        , resetOption
        ]


viewMain : Model -> Element Msg
viewMain model =
    let
        ( whitelistedTags, blacklistedTags ) =
            model.tagsSelected

        tasks =
            model.tasks
                |> filterByTags (BitFlags.match model.tagSettings (Set.toList whitelistedTags) (Set.toList blacklistedTags))
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


type alias CustomColors =
    { blue : Element.Color
    , darkCharcoal : Element.Color
    , green : Element.Color
    , lightBlue : Element.Color
    , darkBlue : Element.Color
    , lightGrey : Element.Color
    , orange : Element.Color
    , red : Element.Color
    , white : Element.Color
    , grey : Element.Color
    , googleLogin : Element.Color
    }


color : CustomColors
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

        populateRows : List LunarTask -> List (Element Msg)
        populateRows data =
            List.map
                (\task ->
                    let
                        pastDueTask =
                            pastDue currentDate task

                        pastDueTd =
                            if pastDueTask then
                                el
                                    [ Element.htmlAttribute <|
                                        Html.Attributes.title
                                            (periodsLapsedMessage currentDate task)
                                    ]
                                    (text <|
                                        String.fromInt
                                            (getDaysPastDue currentDate task)
                                    )

                            else
                                Element.none

                        lastCompletedTd =
                            if pastDueTask then
                                el [] <|
                                    text (Date.toIsoString (getLastCompletedAt task))

                            else
                                el
                                    [ Element.htmlAttribute <|
                                        Html.Attributes.title
                                            ("This task will be past due again on " ++ Date.toIsoString (getNextPastDueDate task))
                                    ]
                                    (text <|
                                        Date.toIsoString (getLastCompletedAt task)
                                    )
                    in
                    row []
                        [ el
                            [ Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
                            , Element.htmlAttribute <| Html.Events.onClick (EditTask task.id)
                            , Element.htmlAttribute <| Html.Attributes.class "embolden"
                            , Element.htmlAttribute <| Html.Attributes.title task.notes
                            ]
                          <|
                            text task.title
                        , pastDueTd
                        , el [] <| text (String.fromInt task.period)
                        , lastCompletedTd
                        , row
                            [ Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
                            , Element.htmlAttribute <| Html.Attributes.style "text-align" "center"
                            , Element.htmlAttribute <| Html.Attributes.class "embolden"
                            , Element.htmlAttribute <| Html.Events.onClick (MarkCompleted task currentDate)
                            , Element.htmlAttribute <| Html.Attributes.class "selective-icon-opts"
                            , Element.htmlAttribute <| Html.Attributes.class "selective-icon-opts-checkbox"
                            , Element.htmlAttribute <| Html.Attributes.title "Mark Task Completed"
                            ]
                            [ el [ Element.htmlAttribute <| Html.Attributes.class "selective-icon-activated" ] (FeatherIcons.checkSquare |> FeatherIcons.toHtml [] |> Element.html)
                            , el [ Element.htmlAttribute <| Html.Attributes.class "selective-icon-inactivated" ] (FeatherIcons.square |> FeatherIcons.toHtml [] |> Element.html)
                            ]
                        , row
                            [ Element.htmlAttribute <| Html.Attributes.style "cursor" "pointer"
                            , Element.htmlAttribute <| Html.Attributes.style "text-align" "center"
                            , Element.htmlAttribute <| Html.Attributes.class "embolden"
                            , Element.htmlAttribute <| Html.Events.onClick (DeleteTask task)
                            , Element.htmlAttribute <| Html.Attributes.class "selective-icon-opts"
                            , Element.htmlAttribute <| Html.Attributes.class "selective-icon-opts-checkbox"
                            , Element.htmlAttribute <| Html.Attributes.title "Delete Task"
                            ]
                            [ el [ Element.htmlAttribute <| Html.Attributes.class "selective-icon-activated" ] (FeatherIcons.trash2 |> FeatherIcons.toHtml [] |> Element.html)
                            , el [ Element.htmlAttribute <| Html.Attributes.class "selective-icon-inactivated" ] (FeatherIcons.trash |> FeatherIcons.toHtml [] |> Element.html)
                            ]
                        ]
                )
                data
    in
    el [ width fill ] <|
        column []
            [ row
                []
                [ el [ Element.htmlAttribute (Html.Attributes.style "text-align" "left") ] <| text "Task"
                , el [ Element.htmlAttribute (Html.Attributes.style "text-align" "left") ] <| text "Days Past Due"
                , el [ Element.htmlAttribute (Html.Attributes.style "text-align" "left") ] <| text "Cadence"
                , el [ Element.htmlAttribute (Html.Attributes.style "text-align" "left") ] <| text "Last Completed"
                , el [] <| text ""
                , el [] <| text ""
                ]
            , column [ Element.htmlAttribute (Html.Attributes.id "task-table-body") ] (populateRows tasks)
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
            { onPress = Just CreateTask
            , label =
                FeatherIcons.plusCircle
                    |> FeatherIcons.withSize 2
                    |> FeatherIcons.withSizeUnit "em"
                    |> FeatherIcons.toHtml []
                    |> Element.html
            }

    else
        button [ alignBottom, paddingXY 0 5 ]
            { onPress = Nothing
            , label =
                FeatherIcons.plusCircle
                    |> FeatherIcons.withSize 2
                    |> FeatherIcons.withSizeUnit "em"
                    |> FeatherIcons.withClass "disabled-new-task"
                    |> FeatherIcons.toHtml [ Html.Attributes.style "cursor" "default" ]
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
                , onChange = NewTaskUpdateTitle
                }
            , Input.text []
                { placeholder = Nothing
                , label = Input.labelAbove [ Font.bold ] <| text "Cadence (in days)"
                , text = String.fromInt model.newTaskPeriod
                , onChange = NewTaskUpdatePeriod
                }
            , Input.text []
                { placeholder = Nothing
                , label = Input.labelAbove [ Font.bold ] <| text "Notes"
                , text = model.newTaskNotes
                , onChange = NewTaskUpdateNotes
                }
            , column []
                [ el [ Font.bold, paddingXY 0 4 ] (text "Date of Last Completion")
                , el [ Border.width 1, paddingXY 10 10, Border.color color.lightGrey ] <|
                    (DatePicker.view (Just model.newTaskCompletedAt)
                        datePickerSettings
                        model.datePicker
                        |> Html.map NewTaskSetDatePicker
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
