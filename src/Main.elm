port module Main exposing (..)

import BitFlags exposing (BitFlagSettings)
import Browser
import Browser.Events exposing (Visibility(..), onVisibilityChange)
import Date exposing (Date, Unit(..))
import DatePicker exposing (defaultSettings)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input exposing (OptionState(..), button)
import FeatherIcons as Icon exposing (Icon)
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
import NewLunarTask exposing (..)
import Process
import SHA1
import SearchBox
import Set exposing (Set)
import Task
import Time exposing (utc)



-- MAIN


main : Program ( Int, Bool ) Model Msg
main =
    Browser.element
        { init = \( a, b ) -> init a b
        , update = update
        , view = view
        , subscriptions = subscriptions
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



-- MODEL


type alias Model =
    { tasks : List LunarTask
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
    , banner : String
    , editedTask : Maybe LunarTask
    , tagSettings : BitFlagSettings
    , demo : Bool
    , demoId : Int
    , datePicker : DatePicker.DatePicker
    , tagNameInput : String
    , tagSearchBox : SearchBox.State
    , tagSearchBoxText : String
    , tagSearchBoxSelected : Maybe String
    , tagResourcesLoaded : Bool
    , lastCacheCheckAt : Time.Posix
    , receivedCurrentTimeAt : Time.Posix
    }


type LoadedTasksViewState
    = JsonExportView
    | LoadedTasksView
    | EditTaskView
    | TagSettingsView (Maybe String)


type ViewState
    = LoginPrompt
    | LoadingTasksView
    | LoadingTasksFailureView
    | LoadedTasks LoadedTasksViewState



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
        , demo : Bool
        , tagSettings : BitFlagSettings
        , banner : String
        , tagResourcesLoaded : Bool
    }


resetLogin : LoginAttributes r -> LoginAttributes r
resetLogin attrs =
    { attrs
        | tasks = []
        , demo = False
        , taskOwner = ""
        , view = LoginPrompt
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



-- CACHE DIGEST


generateCacheDigest : List LunarTask -> Model -> String
generateCacheDigest tasks model =
    let
        tags =
            model.tagSettings
                |> BitFlags.allFlags
                |> String.join ","
    in
    tasks
        |> Encode.list lunarTaskEncoder
        |> Encode.encode 0
        |> (++) tags
        |> SHA1.fromString
        |> SHA1.toHex



-- INIT


currentTags : BitFlagSettings -> List String
currentTags settings =
    BitFlags.allFlags settings


init : Int -> Bool -> ( Model, Cmd Msg )
init currentTimeinMillis validAuth =
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
                LoginPrompt

        currentZone =
            utc

        currentDate =
            Date.fromPosix currentZone (Time.millisToPosix currentTimeinMillis)

        currentTime =
            Time.millisToPosix currentTimeinMillis

        ( newDatePicker, datePickerCmd ) =
            DatePicker.init

        model : Model
        model =
            { tasks = []
            , taskOwner = ""
            , currentDate = currentDate
            , currentZone = currentZone
            , filter = FilterAll
            , sort = NoSort DESC
            , tagsSelected = ( Set.fromList [], Set.fromList [] )
            , tagSettings = BitFlags.defaultSettings 25
            , searchTerm = Nothing
            , datePicker = newDatePicker
            , view = loadingOrLoginView
            , newTaskTitle = ""
            , newTaskNotes = ""
            , newTaskPeriod = 15
            , newTaskCompletedAt = currentDate
            , banner = ""
            , editedTask = Nothing
            , demo = False
            , demoId = 0
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
    , Cmd.batch
        [ loginCmd
        , Time.now |> Task.perform ReceivedCurrentTime
        , Time.here |> Task.perform AdjustTimeZone
        , Task.perform DemoIdTick Time.now
        , Cmd.map NewTaskSetDatePicker datePickerCmd
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
    | ToggleSortOrder
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
    | EditTaskDisableTag String
    | EditTaskEnableTag String
    | EditTaskTitle String
    | EditTaskNotes String
    | EditTaskRemoveCompletionEntry Date.Date
    | EditTaskAddCompletionEntry DatePicker.Msg
    | EditTaskCancel
    | EditTaskSave
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

        maybeFetchCacheDigestAfterXMinutes : Int -> ( Model, List (Cmd Msg) ) -> ( Model, List (Cmd Msg) )
        maybeFetchCacheDigestAfterXMinutes minutes ( m, lc ) =
            let
                timeSinceLastCacheCheck =
                    Time.posixToMillis m.receivedCurrentTimeAt - Time.posixToMillis m.lastCacheCheckAt
            in
            -- fetch cache digest every X minutes
            if timeSinceLastCacheCheck > (minutes * 60000) && not m.demo then
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

                "cacheDigestFetched" ->
                    update (CompareCacheDigest data.payload) model

                unknownMessage ->
                    update (LoadTasks Encode.null) { model | banner = "Recv data unrecognized: " ++ unknownMessage }

        ToggleTag tag ->
            ( model |> toggleTag tag, Cmd.none )

        EditTags ->
            ( { model | view = LoadedTasks (TagSettingsView Nothing) }, Cmd.none )

        SelectTagToEdit maybeTagName ->
            case maybeTagName of
                Just tagName ->
                    ( { model | view = LoadedTasks (TagSettingsView (Just tagName)), tagNameInput = tagName }, Cmd.none )

                Nothing ->
                    ( { model | view = LoadedTasks (TagSettingsView Nothing), tagNameInput = "" }, Cmd.none )

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
                    in
                    if backendCacheDigest /= currentCacheDigest then
                        ( { model | lastCacheCheckAt = model.receivedCurrentTimeAt }
                        , Cmd.batch [ fetchTasks, fetchTags ]
                        )

                    else
                        ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        CreateTag ->
            let
                addedToTagSettings =
                    BitFlags.createFlag model.tagNameInput model.tagSettings
            in
            if model.demo then
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
            if model.demo then
                ( { model | tagSettings = updatedTagSettings, view = LoadedTasks (TagSettingsView Nothing) }, Cmd.none )

            else
                ( model, updateTags (Encode.list Encode.string (BitFlags.serialize updatedTagSettings)) )

        DeleteTag tagName ->
            let
                updatedTagSettings =
                    BitFlags.deleteFlag tagName model.tagSettings
            in
            if model.demo then
                ( { model | tagSettings = updatedTagSettings, view = LoadedTasks (TagSettingsView Nothing) }, Cmd.none )

            else
                ( model, updateTags (Encode.list Encode.string (BitFlags.serialize updatedTagSettings)) )

        UpdatedTagNameInput updatedTagName ->
            ( { model | tagNameInput = updatedTagName }, Cmd.none )

        SelectFilter filter ->
            ( model |> selectFilter filter, Cmd.none )

        Search term ->
            ( model |> updateSearchTerm term, Cmd.none )

        ClearSearch ->
            ( model |> updateSearchTerm "", Cmd.none )

        ClearBanner ->
            ( { model | banner = "" }, Cmd.none )

        FilterReset ->
            ( model |> resetFilter, Cmd.none )

        ToggleSortOrder ->
            ( model |> toggleSortOrder, Cmd.none )

        SelectSort sortType ->
            if sortType == model.sort then
                ( toggleSortOrder model, Cmd.none )

            else
                ( model |> updateSort sortType, Cmd.none )

        ViewChange viewType ->
            ( { model | view = viewType }, Cmd.none )

        MarkCompleted task entryDate ->
            let
                jsonTaskMarkedCompleted =
                    lunarTaskEncoder <| markTaskCompleted task (Just entryDate)
            in
            if model.demo then
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
                    ( { model | view = LoadedTasks EditTaskView, editedTask = Just task }, Cmd.none )

        EditTaskCancel ->
            ( { model | editedTask = Nothing, view = LoadedTasks LoadedTasksView }, Cmd.none )

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
                            }

                        originalTask =
                            Maybe.withDefault fakeTask (findTaskById editedTask.id model.tasks)
                    in
                    if originalTask /= editedTask then
                        if model.demo then
                            update (TaskUpdated (lunarTaskEncoder editedTask))
                                { model
                                    | editedTask = Nothing
                                    , view = LoadedTasks LoadedTasksView
                                }

                        else
                            ( { model
                                | editedTask = Nothing
                                , view = LoadedTasks LoadedTasksView
                              }
                            , updateTask (lunarTaskEncoder editedTask)
                            )

                    else
                        ( { model | editedTask = Nothing, view = LoadedTasks LoadedTasksView }, Cmd.none )

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

        EditTaskAddCompletionEntry subMsg ->
            let
                ( newDatePicker, dateEvent ) =
                    DatePicker.update datePickerSettings subMsg model.datePicker

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
            if model.demo then
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

        EditTaskChangedTagSearchBox changeEvent ->
            case changeEvent of
                SearchBox.SelectionChanged tag ->
                    case model.editedTask of
                        Just task ->
                            ( { model
                                | editedTask = Just { task | bitTags = BitFlags.enableFlag model.tagSettings tag task.bitTags }
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
                        , tagSearchBoxText = ""
                      }
                    , Cmd.none
                    )

        DeleteTask task ->
            let
                encodedLunarTask =
                    lunarTaskEncoder task
            in
            if model.demo then
                update (TaskDeleted encodedLunarTask) model

            else
                ( model, deleteTask encodedLunarTask )

        NewTaskSetDatePicker subMsg ->
            let
                ( newDatePicker, dateEvent ) =
                    DatePicker.update datePickerSettings subMsg model.datePicker

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
                |> maybeFetchCacheDigestAfterXMinutes 20
                |> batchCmdList

        VisibilityChanged visibility ->
            case visibility of
                Visible ->
                    ( model, [] )
                        |> maybeFetchCacheDigestAfterXMinutes 5
                        |> batchCmdList

                Hidden ->
                    ( model, Cmd.none )

        TaskDeleted jsonTask ->
            case Decode.decodeValue lunarTaskDecoder jsonTask of
                Ok task ->
                    let
                        tasks =
                            deleteTaskFromList task.id model.tasks
                    in
                    ( { model | tasks = tasks }
                    , -- report new state of task list to backend
                      updateCacheDigest (generateCacheDigest tasks model)
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
                    in
                    ( { model
                        | banner = "task \"" ++ task.title ++ "\" -- id  " ++ task.id ++ " touched by db"
                        , tasks = tasks
                      }
                    , Cmd.batch
                        [ delay 5000 ClearBanner

                        -- event for incrementing demo id
                        , Task.perform DemoIdTick Time.now

                        -- report new state of task list to backend
                        , updateCacheDigest <| generateCacheDigest tasks model
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
                , demo = True
                , taskOwner = "demoTaskOwnerId"
                , tagSettings =
                    Result.withDefault (BitFlags.defaultSettings 25) flagSettingsResult
                , tagResourcesLoaded = True
              }
            , Http.get { url = "/demo-data.json", expect = Http.expectString LoadDemo }
            )

        LoadDemo result ->
            case result of
                Ok demoDataString ->
                    case Decode.decodeString (Decode.list lunarTaskDecoder) demoDataString of
                        Ok tasks ->
                            ( { model
                                | view = LoadedTasks LoadedTasksView
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
                    , Cmd.batch [ fetchTasks, fetchTags ]
                    )

                Err errMsg ->
                    ( { model
                        | view = LoadingTasksFailureView
                        , banner = errorToString errMsg
                      }
                    , Cmd.none
                    )

        DemoIdTick posixTime ->
            ( { model | demoId = Time.posixToMillis posixTime }
            , Cmd.none
            )

        LoadTasks jsonTasks ->
            case
                Decode.decodeValue
                    (Decode.list lunarTaskDecoder)
                    jsonTasks
            of
                Ok tasks ->
                    ( { model
                        | tasks = tasks
                        , view = LoadedTasks LoadedTasksView
                      }
                    , Cmd.none
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
                    ( { model
                        | tagSettings =
                            Result.withDefault
                                model.tagSettings
                                (BitFlags.initSettings { bitLimit = 25, flags = tags })
                        , tagResourcesLoaded = True
                      }
                      -- inform backend of current cached state
                    , updateCacheDigest <| generateCacheDigest model.tasks model
                    )

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

        ReturnToMain ->
            case model.view of
                LoginPrompt ->
                    ( model, Cmd.none )

                LoadingTasksView ->
                    ( model, Cmd.none )

                LoadingTasksFailureView ->
                    ( model, Cmd.none )

                LoadedTasks _ ->
                    ( { model | view = LoadedTasks LoadedTasksView }, Cmd.none )



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
                LoadedTasks LoadedTasksView ->
                    button buttonAttrs { label = text "JSON export", onPress = Just (ViewChange (LoadedTasks JsonExportView)) }

                _ ->
                    button buttonAttrs { label = text "Return to Main", onPress = Just (ViewChange (LoadedTasks LoadedTasksView)) }

        logoutButton =
            row rowSpecs
                [ exportButton
                , button buttonAttrs { label = text "Log Out", onPress = Just (LogOutUser 0) }
                ]
    in
    case viewType of
        LoginPrompt ->
            loginButton

        _ ->
            logoutButton


viewDemoModeBanner : Bool -> Element msg
viewDemoModeBanner demo =
    if demo then
        row
            [ width fill
            , Background.color color.green
            ]
            [ el [ Font.center, Font.color color.white, width fill, Font.semiBold, paddingXY 0 5 ] <| text "DEMO MODE" ]

    else
        Element.none


viewLayout : Model -> Element Msg -> Html Msg
viewLayout model innerContent =
    layout
        [ width fill
        , height fill
        , inFront (viewDemoModeBanner model.demo)
        , Font.family
            [ Font.typeface "Times New Roman"
            , Font.serif
            ]
        ]
    <|
        column [ width fill ]
            [ viewHeader model
            , innerContent
            ]


viewHeader : Model -> Element Msg
viewHeader model =
    column
        [ height <| fillPortion 2
        , width fill
        ]
        [ row
            [ width fill
            , padding 20
            , Background.color color.lightBlue
            ]
            [ row [ onClick ReturnToMain ]
                [ viewMoon
                , el
                    [ Font.size 55
                    , paddingXY 15 0
                    , Font.glow color.blue 0.3
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


view : Model -> Html Msg
view model =
    let
        innerContent =
            case model.view of
                LoginPrompt ->
                    viewLandingPage

                LoadingTasksView ->
                    el [] <| text "Loading"

                LoadingTasksFailureView ->
                    el [] (text "")

                LoadedTasks LoadedTasksView ->
                    viewMain model

                LoadedTasks EditTaskView ->
                    viewTask model

                LoadedTasks JsonExportView ->
                    viewTasksJson model

                LoadedTasks (TagSettingsView maybeSelectedTag) ->
                    viewTagSettings maybeSelectedTag model
    in
    viewLayout model innerContent


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


viewTask : Model -> Element Msg
viewTask model =
    let
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
                , Input.text []
                    { label = Input.labelAbove [ Font.semiBold ] (text "Cadence (in days)")
                    , onChange = EditTaskPeriod
                    , text = String.fromInt task.period
                    , placeholder = Nothing
                    }
                , Input.multiline []
                    { label = Input.labelAbove [ Font.semiBold ] (text "Notes")
                    , onChange = EditTaskNotes
                    , text = task.notes
                    , spellcheck = True
                    , placeholder = Nothing
                    }
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
            { onPress = Just (EditTaskRemoveCompletionEntry entryTime)
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
        , Font.glow color.blue 0.8
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


radioOption : String -> Input.OptionState -> Element msg
radioOption label state =
    row [ spacing 10 ]
        [ el
            [ width <| px 30
            , height <| px 30
            , centerY
            , padding 4
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
                            rgb255 0xFF 0xFF 0xFF

                        Input.Focused ->
                            rgba255 0x72 0x9F 0xCF 0.1

                        Input.Selected ->
                            color.lightBlue
                ]
                none
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
                    [ Input.optionWith FilterPastDue <| radioOption "Past Due"
                    , Input.optionWith FilterNonPastDue <| radioOption "Not Past Due"
                    , Input.optionWith FilterAll <| radioOption "All"
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
                , options =
                    [ Input.optionWith (SortPastDueDays DESC) <| radioOption "Days Past Due"
                    , Input.optionWith (SortLastCompleted DESC) <| radioOption "Last Completed"
                    , Input.optionWith (SortPastDuePeriods DESC) <| radioOption "Periods Lapsed"
                    , Input.optionWith (NoSort DESC) <| radioOption "Default"
                    ]
                }

        searchTermInput =
            Input.text [ Border.width 0 ]
                { text = Maybe.withDefault "" model.searchTerm
                , onChange = Search
                , placeholder = Just <| Input.placeholder [] <| (Icon.search |> Icon.toHtml [] |> Element.html)
                , label = Input.labelHidden "search"
                }

        clearSearchBtn =
            button
                [ transparent (Maybe.withDefault "" model.searchTerm == "")
                , centerY
                ]
                { onPress = Just ClearSearch
                , label = Icon.x |> Icon.toHtml [] |> Element.html
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
        whitelistedTags =
            Tuple.first model.tagsSelected

        blacklistedTags =
            Tuple.second model.tagsSelected

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


type alias Colors =
    { blue : Color
    , darkCharcoal : Color
    , green : Color
    , lightBlue : Color
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
                    in
                    tr []
                        [ td
                            [ Html.Attributes.style "cursor" "pointer"
                            , Html.Events.onClick (EditTask task.id)
                            , Html.Attributes.class "embolden"
                            , Html.Attributes.title task.notes
                            ]
                            [ Html.text task.title ]
                        , pastDueTd
                        , td []
                            [ Html.text <| Date.toIsoString (getLastCompletedAt task) ]
                        , td
                            [ Html.Attributes.style "cursor" "pointer"
                            , Html.Attributes.style "text-align" "center"
                            , Html.Attributes.class "embolden"
                            , Html.Events.onClick (MarkCompleted task currentDate)
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
                                , Html.Attributes.title "Mark Task Completed"
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
                    , Font.color color.red
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
