port module Main exposing (..)

import BitFlags exposing (BitFlagSettings)
import Browser
import Date exposing (Date, Unit(..))
import DatePicker exposing (defaultSettings)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (button)
import FeatherIcons as Icon exposing (Icon)
import Html exposing (Html, td, th, tr)
import Html.Attributes exposing (style)
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
import Set
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
        ]



-- PORTS


port taskAction : ( String, Encode.Value ) -> Cmd msg


port messageReceiver : ({ tag : String, payload : Decode.Value } -> msg) -> Sub msg


port userLoginAction : String -> Cmd msg



-- MODEL


type alias Model =
    { tasks : List LunarTask
    , taskOwner : String
    , currentDate : Date.Date
    , allTags : Set.Set String
    , filter : ListFilter
    , sort : ListSort
    , tagSelected : Maybe String
    , searchTerm : Maybe String
    , view : ViewType
    , newTaskTitle : String
    , newTaskPeriod : Int
    , newTaskTag : Maybe String
    , newTaskCompletedAt : Date.Date
    , banner : String
    , editedTask : Maybe LunarTask
    , tagSettings : BitFlagSettings
    , demo : Bool
    , demoId : Int
    , datePicker : DatePicker.DatePicker
    }


type ViewType
    = LoginPrompt
    | LoadingTasksView
    | LoadingTasksFailureView
    | LoadedTasksView
    | JsonExportView
    | LoggedOut



-- MISC DECODING / ENCODING


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
        , view : ViewType
        , demo : Bool
        , tagSettings : BitFlagSettings
    }


resetLogin : LoginAttributes r -> LoginAttributes r
resetLogin attrs =
    { attrs
        | tasks = []
        , demo = False
        , taskOwner = ""
        , view = LoginPrompt
        , tagSettings = BitFlags.defaultSettings
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


currentTags : List LunarTask -> Set.Set String
currentTags tasks =
    List.map .tag tasks
        |> List.map (Maybe.withDefault "")
        |> List.map String.trim
        |> List.filter (\n -> not (String.isEmpty n))
        |> Set.fromList


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

        currentDate =
            Date.fromPosix utc (Time.millisToPosix currentTimeinMillis)

        ( newDatePicker, datePickerCmd ) =
            DatePicker.init

        model : Model
        model =
            { tasks = []
            , taskOwner = ""
            , currentDate = currentDate
            , allTags = currentTags []
            , filter = FilterAll
            , sort = NoSort DESC
            , tagSelected = Nothing
            , tagSettings = BitFlags.defaultSettings
            , searchTerm = Nothing
            , datePicker = newDatePicker
            , view = loadingOrLoginView
            , newTaskTitle = ""
            , newTaskPeriod = 15
            , newTaskTag = Nothing
            , newTaskCompletedAt = currentDate
            , banner = ""
            , editedTask = Nothing
            , demo = False
            , demoId = 0
            }
    in
    ( model
    , Cmd.batch
        [ Date.today |> Task.perform ReceivedCurrentDate
        , loginCmd
        , Task.perform DemoIdTick Time.now
        , Cmd.map NewTaskSetDatePicker datePickerCmd
        ]
    )



-- UPDATE


type Msg
    = Recv { tag : String, payload : Decode.Value }
    | SelectTag (Maybe String)
    | Search String
    | ClearSearch
    | ClearBanner
    | FilterReset
    | ToggleSortOrder
    | SelectFilter ListFilter
    | SelectSort ListSort
    | ViewChange ViewType
    | MarkCompleted LunarTask Date.Date
    | CreateTask
    | NewTaskUpdateTitle String
    | NewTaskUpdatePeriod String
    | NewTaskUpdateTag String
    | NewTaskSetDatePicker DatePicker.Msg
    | EditTaskPeriod String
    | EditTaskTag String
    | EditTaskTitle String
    | EditTaskRemoveCompletionEntry Date.Date
    | EditTaskAddCompletionEntry DatePicker.Msg
    | EditTaskCancel
    | EditTaskSave
    | EditTask String
    | DeleteTask LunarTask
    | ProcessDownKeys Keyboard.RawKey
    | ReceivedCurrentDate Date
    | TaskUpdated Decode.Value
    | TaskDeleted Decode.Value
    | LogOutUser Int
    | LoadTasks Decode.Value
    | LoginUser Int
    | DemoLoginUser Int
    | LoadDemo (Result Http.Error String)
    | UserLoggedIn Decode.Value
    | DemoIdTick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        createTask encodedTask =
            taskAction ( "create", encodedTask )

        updateTask encodedTask =
            taskAction ( "update", encodedTask )

        deleteTask encodedTask =
            taskAction ( "delete", encodedTask )

        fetchTasks =
            taskAction ( "fetch", Encode.null )
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

                _ ->
                    update (LoadTasks Encode.null) model

        SelectTag maybeTagName ->
            ( model |> selectTag maybeTagName, Cmd.none )

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
                ( model, updateTask jsonTaskMarkedCompleted )

        EditTask taskId ->
            case findTaskById taskId model.tasks of
                Nothing ->
                    ( model, Cmd.none )

                Just task ->
                    ( { model | editedTask = Just task }, Cmd.none )

        EditTaskCancel ->
            ( { model | editedTask = Nothing }, Cmd.none )

        EditTaskSave ->
            case model.editedTask of
                Nothing ->
                    ( model, Cmd.none )

                Just editedTask ->
                    let
                        fakeTask : LunarTask
                        fakeTask =
                            { title = ""
                            , period = 20
                            , completionEntries = []
                            , id = "asdfasdf"
                            , tag = Nothing
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
                                }

                        else
                            ( { model
                                | editedTask = Nothing
                              }
                            , updateTask (lunarTaskEncoder editedTask)
                            )

                    else
                        ( { model | editedTask = Nothing }, Cmd.none )

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
                ( modelWithNewTaskReset, createTask encodedJsonTask )

        NewTaskUpdateTitle title ->
            ( { model | newTaskTitle = title }, Cmd.none )

        NewTaskUpdatePeriod rawPeriod ->
            case String.toInt rawPeriod of
                Just period ->
                    ( { model | newTaskPeriod = period }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        NewTaskUpdateTag rawTag ->
            let
                tag =
                    if String.trim rawTag == "" then
                        Nothing

                    else
                        Just rawTag
            in
            ( { model | newTaskTag = tag }, Cmd.none )

        EditTaskTag stringTag ->
            let
                tag =
                    if String.trim stringTag == "" then
                        Nothing

                    else
                        Just stringTag
            in
            case model.editedTask of
                Just task ->
                    ( { model | editedTask = Just { task | tag = tag } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        EditTaskTitle titleStr ->
            case model.editedTask of
                Just task ->
                    ( { model | editedTask = Just { task | title = titleStr } }, Cmd.none )

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

        ReceivedCurrentDate date ->
            ( { model | currentDate = date }, Cmd.none )

        TaskDeleted jsonTask ->
            case Decode.decodeValue lunarTaskDecoder jsonTask of
                Ok task ->
                    ( { model | tasks = deleteTaskFromList task.id model.tasks }, Cmd.none )

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
                    ( let
                        tasks =
                            insertOrUpdateTask task model.tasks
                      in
                      { model
                        | banner = "task \"" ++ task.title ++ "\" -- id  " ++ task.id ++ " touched by db"
                        , tasks = tasks
                        , allTags = currentTags tasks
                      }
                    , Cmd.batch
                        [ delay 5000 ClearBanner

                        -- event for incrementing demo id
                        , Task.perform DemoIdTick Time.now
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
                        { bitLimit = 20
                        , flags =
                            [ "trash-grab"
                            , "laundry"
                            , "landscaping"
                            , "digital-hygiene"
                            ]
                        }
            in
            ( { model
                | view = LoadingTasksView
                , demo = True
                , taskOwner = "demoTaskOwnerId"
                , tagSettings =
                    Result.withDefault BitFlags.defaultSettings flagSettingsResult
              }
            , Http.get { url = "/demo-data.json", expect = Http.expectString LoadDemo }
            )

        LoadDemo result ->
            case result of
                Ok demoDataString ->
                    case Decode.decodeString (Decode.list lunarTaskDecoder) demoDataString of
                        Ok tasks ->
                            ( { model
                                | view = LoadedTasksView
                                , tasks = tasks
                                , allTags = currentTags tasks
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
                    , fetchTasks
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
                        , view = LoadedTasksView
                        , allTags = currentTags tasks
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

        ProcessDownKeys rawKey ->
            case Keyboard.anyKeyOriginal rawKey of
                Just Escape ->
                    update ClearSearch model

                _ ->
                    ( model, Cmd.none )



-- VIEW


loginlogoutButton : ViewType -> Element Msg
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
            if viewType == JsonExportView then
                button buttonAttrs { label = text "Return to Main", onPress = Just (ViewChange LoadedTasksView) }

            else
                button buttonAttrs { label = text "JSON export", onPress = Just (ViewChange JsonExportView) }

        logoutButton =
            row rowSpecs
                [ exportButton
                , button buttonAttrs { label = text "Log Out", onPress = Just (LogOutUser 0) }
                ]
    in
    case viewType of
        LoggedOut ->
            logoutButton

        LoginPrompt ->
            loginButton

        _ ->
            logoutButton


viewDemoModeBanner : Bool -> Element msg
viewDemoModeBanner demo =
    if demo then
        row
            [ width fill
            , Background.color color.darkCharcoal
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
            [ viewMoon
            , el
                [ Font.size 55
                , paddingXY 15 0
                , Font.glow color.blue 0.3
                ]
                (text "LunarTasks")
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
                LoadingTasksView ->
                    el [] <| text "Loading"

                LoadedTasksView ->
                    case model.editedTask of
                        Nothing ->
                            viewMain model

                        Just _ ->
                            viewTask model

                LoginPrompt ->
                    viewLandingPage

                JsonExportView ->
                    viewTasksJson model

                _ ->
                    el [] (text "")
    in
    viewLayout model innerContent


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
                , Input.text []
                    { label = Input.labelAbove [ Font.semiBold ] (text "Category")
                    , onChange = EditTaskTag
                    , text = Maybe.withDefault "" task.tag
                    , placeholder = Nothing
                    }
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
            , label = text "x"
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
                    , Input.optionWith (SortPastDuePeriods DESC) <| radioOption "Past Due Periods"
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

        resetOption =
            button
                [ transparent
                    (model.filter
                        == FilterAll
                        && model.sort
                        == NoSort DESC
                        && model.searchTerm
                        == Nothing
                    )
                ]
                { label = text "(x) reset all selections", onPress = Just FilterReset }
    in
    column [ spacingXY 0 15 ]
        [ filterRow
        , sortRow
        , row [ Border.width 1, paddingXY 10 7 ]
            [ searchTermInput
            , clearSearchBtn
            ]
        , wrappedRow [ spacingXY 10 5 ]
            (List.map (viewTagButton (Maybe.withDefault "" model.tagSelected))
                (Set.toList model.allTags)
            )
        , resetOption
        ]


viewMain : Model -> Element Msg
viewMain model =
    let
        tasks =
            model.tasks
                |> filterByTag model.tagSelected
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
        populateRows : List LunarTask -> List (Html Msg)
        populateRows data =
            List.map
                (\task ->
                    tr []
                        [ td
                            [ Html.Attributes.style "cursor" "pointer"
                            , Html.Events.onClick (EditTask task.id)
                            , Html.Attributes.class "embolden"
                            ]
                            [ Html.text task.title ]
                        , td []
                            [ Html.text <| String.fromInt (periodsPastDue currentDate task) ]
                        , td []
                            [ Html.text <| String.fromInt task.period ]
                        , td []
                            [ Html.text <| Date.toIsoString (getLastCompletedAt task) ]
                        , td [] [ Html.text <| String.fromInt (getDaysPastDue currentDate task) ]
                        , td
                            [ Html.Events.onClick (SelectTag task.tag)
                            , Html.Attributes.class "embolden"
                            ]
                            [ Html.span [ Html.Attributes.style "cursor" "pointer" ]
                                [ Html.text <| Maybe.withDefault "" task.tag ]
                            ]
                        , td
                            [ Html.Attributes.style "cursor" "pointer"
                            , Html.Attributes.style "text-align" "center"
                            , Html.Attributes.class "embolden"
                            , Html.Events.onClick (MarkCompleted task currentDate)
                            ]
                            [ Html.text "Mark Completed" ]
                        , td
                            [ Html.Attributes.style "cursor" "pointer"
                            , Html.Attributes.style "text-align" "center"
                            , Html.Attributes.class "embolden"
                            , Html.Events.onClick (DeleteTask task)
                            ]
                            [ Html.text "Remove" ]
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
                        [ th []
                            [ Html.text "Task" ]
                        , th []
                            [ Html.text "Full Periods Lapsed" ]
                        , th []
                            [ Html.text "Cadence" ]
                        , th []
                            [ Html.text "Last Completed" ]
                        , th [] [ Html.text "Days Past Due" ]
                        , th [] [ Html.text "Category" ]
                        , th [] [ Html.text "Mark Completed" ]
                        , th [] [ Html.text "Remove" ]
                        ]
                    ]
                , Html.tbody [ Html.Attributes.id "task-table-body" ] (populateRows tasks)
                ]


viewTagButton : String -> String -> Element Msg
viewTagButton selectedTag tag =
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

        unSelectedAttrs =
            [ Background.color color.white
            , Font.color color.darkCharcoal
            ]
                ++ baseButtonAttrs

        selectedAttrs =
            [ Background.color color.darkCharcoal
            , Font.color color.white
            ]
                ++ baseButtonAttrs

        buttonAttrs =
            if tag == selectedTag then
                selectedAttrs

            else
                unSelectedAttrs
    in
    button buttonAttrs { onPress = Just (SelectTag (Just tag)), label = text tag }


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
                , text = Maybe.withDefault "" model.newTaskTag
                , onChange = NewTaskUpdateTag
                , label = Input.labelAbove [ Font.bold ] <| text "Category"
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
            , button [ alignBottom, paddingXY 0 5 ]
                { onPress = Just CreateTask
                , label =
                    Icon.plusCircle
                        |> Icon.withSize 2
                        |> Icon.withSizeUnit "em"
                        |> Icon.toHtml []
                        |> Element.html
                }
            ]
