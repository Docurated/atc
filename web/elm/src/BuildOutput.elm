module BuildOutput exposing (init, update, view, subscriptions, Model, Msg, OutMsg(..))

import Ansi.Log
import Date exposing (Date)
import Html exposing (Html)
import Html.App
import Html.Attributes exposing (action, class, classList, href, id, method, title)
import Http
import Json.Encode
import Json.Decode exposing ((:=))
import Task exposing (Task)

import Concourse
import Concourse.Build
import Concourse.BuildPlan
import Concourse.BuildStatus
import Concourse.BuildResources exposing (empty, fetch)
import LoadingIndicator
import StepTree exposing (StepTree)

import Phoenix.Channel
import Phoenix.Socket

type alias Model =
  { build : Concourse.Build
  , steps : Maybe StepTree.Model
  , errors : Maybe Ansi.Log.Model
  , state : OutputState
  , eventSourceOpened : Bool
  , eventsSocket : Phoenix.Socket.Socket Msg
  }

type OutputState
  = StepsLoading
  | StepsLiveUpdating
  | StepsComplete
  | LoginRequired

type Msg
  = Noop
  | PlanAndResourcesFetched (Result Http.Error (Concourse.BuildPlan, Concourse.BuildResources))
  | PhoenixMsg (Phoenix.Socket.Msg Msg)
  | StepTreeMsg StepTree.Msg
  | ChannelOpened String
  | ChannelErrored String
  | ChannelClosed String
  | BuildEventMsg BuildEvent Json.Encode.Value

type BuildEvent
  = LogEvent

type OutMsg
  = OutNoop
  | OutBuildStatus Concourse.BuildStatus Date

type alias Origin =
  { source : String
  , id : String
  }

type alias BuildEventWrapper =
  { origin : Origin
  , output : String
  }


init : Concourse.Build -> (Model, Cmd Msg)
init build =
  let
    outputState =
      if Concourse.BuildStatus.isRunning build.status then
        StepsLiveUpdating
      else
        StepsLoading

    model =
      { build = build
      , steps = Nothing
      , errors = Nothing
      , state = outputState
      , eventSourceOpened = False
      , eventsSocket = initSocket
      }

    fetch =
      if build.job /= Nothing then
        fetchBuildPlanAndResources model.build.id
      else
        fetchBuildPlan model.build.id
  in
    (model, fetch)

update : Msg -> Model -> (Model, Cmd Msg, OutMsg)
update action model =
  case action of
    Noop ->
      (model, Cmd.none, OutNoop)

    PlanAndResourcesFetched (Err (Http.BadResponse 404 _)) ->
      let
        (eventsSocket, cmd) = subscribeToEvents model.build.id model.eventsSocket
      in
        ( { model | eventsSocket = eventsSocket }
        , cmd
        , OutNoop
        )

    PlanAndResourcesFetched (Err err) ->
      Debug.log ("failed to fetch plan: " ++ toString err) <|
        (model, Cmd.none, OutNoop)

    PlanAndResourcesFetched (Ok (plan, resources)) ->
      let
        (eventsSocket, cmd) = subscribeToEvents model.build.id model.eventsSocket
      in
        ( { model | steps = Just (StepTree.init resources plan), eventsSocket = eventsSocket }
        , cmd
        , OutNoop
        )

    PhoenixMsg msg ->
      let
        ( eventsSocket, phxCmd ) = Phoenix.Socket.update msg model.eventsSocket
      in
        ( { model | eventsSocket = eventsSocket }
        , Cmd.map PhoenixMsg phxCmd
        , OutNoop
        )

    ChannelOpened channel ->
      ({ model | eventSourceOpened = True }, Cmd.none, OutNoop)

    ChannelErrored channel ->
      if model.eventSourceOpened then
        -- connection could have dropped out of the blue; just let the browser
        -- handle reconnecting
        (model, Cmd.none, OutNoop)
      else
        -- assume request was rejected because auth is required; no way to
        -- really tell
        ({ model | state = LoginRequired }, Cmd.none, OutNoop)

    ChannelClosed channel ->
      ({ model | state = StepsComplete }, Cmd.none, OutNoop)

    StepTreeMsg action ->
      ( { model | steps = Maybe.map (StepTree.update action) model.steps }
      , Cmd.none
      , OutNoop
      )

    BuildEventMsg eventType raw ->
      case Json.Decode.decodeValue eventsDecoder raw of
        Ok event ->
          handleEvent eventType event model
        Err error ->
          Debug.log error
          (model, Cmd.none, OutNoop)

subscriptions : Model -> Sub Msg
subscriptions model =
  Phoenix.Socket.listen model.eventsSocket PhoenixMsg

decodeOrigin : Json.Decode.Decoder Origin
decodeOrigin =
  Json.Decode.object2 Origin
    (Json.Decode.map (Maybe.withDefault "") << Json.Decode.maybe <| "source" := Json.Decode.string)
    ("id" := Json.Decode.string)

eventsDecoder : Json.Decode.Decoder BuildEventWrapper
eventsDecoder =
  Json.Decode.object2 BuildEventWrapper
    ("origin" := decodeOrigin)
    ("payload" := Json.Decode.string)

handleEvent : BuildEvent -> BuildEventWrapper -> Model -> (Model, Cmd Msg, OutMsg)
handleEvent eventType event model =
  case eventType of
    LogEvent ->
      ( updateStep event.origin.id (setRunning << appendStepLog event.output) model
      , Cmd.none
      , OutNoop
      )

      {-
    Concourse.BuildEvents.Error event.origin message ->
      ( updateStep event.origin.id (setStepError message) model
      , Cmd.none
      , OutNoop
      )

    Concourse.BuildEvents.InitializeTask event.origin ->
      ( updateStep event.origin.id setRunning model
      , Cmd.none
      , OutNoop
      )

    Concourse.BuildEvents.StartTask event.origin ->
      ( updateStep event.origin.id setRunning model
      , Cmd.none
      , OutNoop
      )

    Concourse.BuildEvents.FinishTask event.origin exitStatus ->
      ( updateStep event.origin.id (finishStep exitStatus) model
      , Cmd.none
      , OutNoop
      )

    Concourse.BuildEvents.InitializeGet event.origin ->
      ( updateStep event.origin.id setRunning model
      , Cmd.none
      , OutNoop
      )

    Concourse.BuildEvents.FinishGet event.origin exitStatus version metadata ->
      ( updateStep event.origin.id (finishStep exitStatus << setResourceInfo version metadata) model
      , Cmd.none
      , OutNoop
      )

    Concourse.BuildEvents.InitializePut event.origin ->
      ( updateStep event.origin.id setRunning model
      , Cmd.none
      , OutNoop
      )

    Concourse.BuildEvents.FinishPut event.origin exitStatus version metadata ->
      ( updateStep event.origin.id (finishStep exitStatus << setResourceInfo version metadata) model
      , Cmd.none
      , OutNoop
      )

    Concourse.BuildEvents.BuildStatus status date ->
      ( { model
        | steps =
            if not <| Concourse.BuildStatus.isRunning status then
              Maybe.map (StepTree.update StepTree.Finished) model.steps
            else
              model.steps
        }
      , Cmd.none
      , OutBuildStatus status date
      )

    Concourse.BuildEvents.BuildError message ->
      ( { model |
          errors =
            Just <|
              Ansi.Log.update message <|
                Maybe.withDefault (Ansi.Log.init Ansi.Log.Cooked) model.errors
        }
      , Cmd.none
      , OutNoop
      )
-}
initSocket : Phoenix.Socket.Socket Msg
initSocket =
  Phoenix.Socket.init "ws://localhost:7777/events/websocket"
    |> Phoenix.Socket.withDebug

updateStep : StepTree.StepID -> (StepTree -> StepTree) -> Model -> Model
updateStep id update model =
  { model | steps = Maybe.map (StepTree.updateAt id update) model.steps }

setRunning : StepTree -> StepTree
setRunning = setStepState StepTree.StepStateRunning

appendStepLog : String -> StepTree -> StepTree
appendStepLog output tree =
  StepTree.map (\step -> { step | log = Ansi.Log.update output step.log }) tree

setStepError : String -> StepTree -> StepTree
setStepError message tree =
  StepTree.map
    (\step ->
      { step
      | state = StepTree.StepStateErrored
      , error = Just message
      })
    tree

finishStep : Int -> StepTree -> StepTree
finishStep exitStatus tree =
  let
    stepState =
      if exitStatus == 0 then
        StepTree.StepStateSucceeded
      else
        StepTree.StepStateFailed
  in
    setStepState stepState tree

setResourceInfo : Concourse.Version -> Concourse.Metadata -> StepTree -> StepTree
setResourceInfo version metadata tree =
  StepTree.map (\step -> { step | version = Just version, metadata = metadata }) tree

setStepState : StepTree.StepState -> StepTree -> StepTree
setStepState state tree =
  StepTree.map (\step -> { step | state = state }) tree

fetchBuildPlanAndResources : Int -> Cmd Msg
fetchBuildPlanAndResources buildId =
  Cmd.map PlanAndResourcesFetched << Task.perform Err Ok <|
    Task.map2 (,) (Concourse.BuildPlan.fetch buildId) (Concourse.BuildResources.fetch buildId)

fetchBuildPlan : Int -> Cmd Msg
fetchBuildPlan buildId =
  Cmd.map PlanAndResourcesFetched << Task.perform Err Ok <|
    Task.map (flip (,) Concourse.BuildResources.empty) (Concourse.BuildPlan.fetch buildId)

subscribeToEvents : Int -> Phoenix.Socket.Socket Msg -> (Phoenix.Socket.Socket Msg, Cmd Msg)
subscribeToEvents buildId socket =
  let
    channelName = "build:" ++ (toString buildId)
    channel =
      Phoenix.Channel.init channelName
        |> Phoenix.Channel.onJoin (always (ChannelOpened channelName))
        |> Phoenix.Channel.onClose (always (ChannelClosed channelName))
        |> Phoenix.Channel.onError (always (ChannelErrored channelName))
    (socket, cmd) =
      Phoenix.Socket.on "log" channelName (BuildEventMsg LogEvent) socket
        |> Phoenix.Socket.join channel
  in
    (socket
    , Cmd.map PhoenixMsg cmd
    )

view : Model -> Html Msg
view {build, steps, errors, state} =
  Html.div [class "steps"]
    [ viewErrors errors
    , viewStepTree build steps state
    ]

viewStepTree : Concourse.Build -> Maybe StepTree.Model -> OutputState -> Html Msg
viewStepTree build steps state =
  case (state, steps) of
    (StepsLoading, _) ->
      LoadingIndicator.view

    (LoginRequired, _) ->
      viewLoginButton build

    (StepsLiveUpdating, Just root) ->
      Html.App.map StepTreeMsg (StepTree.view root)

    (StepsComplete, Just root) ->
      Html.App.map StepTreeMsg (StepTree.view root)

    (_, Nothing) ->
      Html.div [] []

viewErrors : Maybe Ansi.Log.Model -> Html msg
viewErrors errors =
  case errors of
    Nothing ->
      Html.div [] []

    Just log ->
      Html.div [class "build-step"]
        [ Html.div [class "header"]
            [ Html.i [class "left fa fa-fw fa-exclamation-triangle"] []
            , Html.h3 [] [Html.text "error"]
            ]
        , Html.div [class "step-body build-errors-body"] [Ansi.Log.view log]
        ]

viewLoginButton : Concourse.Build -> Html msg
viewLoginButton build =
  Html.form
    [ class "build-login"
    , Html.Attributes.method "get"
    , Html.Attributes.action "/login"
    ]
    [ Html.input
        [ Html.Attributes.type' "submit"
        , Html.Attributes.value "log in to view"
        ] []
    , Html.input
        [ Html.Attributes.type' "hidden"
        , Html.Attributes.name "redirect"
        , Html.Attributes.value (Concourse.Build.url build)
        ] []
    ]
