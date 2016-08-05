module Pipeline exposing (..)

import AnimationFrame
import Dict exposing (Dict)
import Graph exposing (Graph)
import Html exposing (Html)
import Html.Attributes exposing (class, href, style, rowspan)
import Http
import Set
import Task
import Matrix

import Concourse.Job exposing (Job)
import Concourse.BuildStatus exposing (BuildStatus)

import Grid exposing (Grid)

type alias Model =
  { fit : Maybe (() -> Cmd Msg)
  , pipelineLocator : Concourse.Job.PipelineLocator
  , jobs : List Job
  , graph : Graph Node ()
  , error : Maybe String
  }

type Node
  = JobNode Job
  | InputNode
      { resourceName : String
      , dependentJob : Job
      }
  | OutputNode
      { resourceName : String
      , upstreamJob : Job
      }
  | ConstrainedInputNode
      { resourceName : String
      , dependentJob : Job
      , upstreamJob : Job
      }

type alias Flags =
  { teamName : String
  , pipelineName : String
  }

type Msg
  = Noop
  | JobsFetched (Result Http.Error (List Job))
  | Frame

init : (() -> Cmd Msg) -> Flags -> (Model, Cmd Msg)
init fit flags =
  ( { fit = Just fit
    , pipelineLocator = flags
    , jobs = []
    , graph = Graph.empty
    , error = Nothing
    }
  , fetchJobs flags
  )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Noop ->
      (model, Cmd.none)

    JobsFetched (Ok jobs) ->
      let
        filtered =
          jobs
          -- List.filter (List.member "develop" << .groups) jobs
      in
        ({ model | jobs = filtered, graph = initGraph filtered }, Cmd.none)

    JobsFetched (Err msg) ->
      ({ model | error = Just (toString msg) }, Cmd.none)

    Frame ->
      case model.fit of
        Just fit ->
          ({ model | fit = Nothing }, fit ())
        Nothing ->
          (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  if List.isEmpty model.jobs || model.fit == Nothing then
    Sub.none
  else
    AnimationFrame.times (always Frame)

view : Model -> Html Msg
view model =
  case model.error of
    Just msg ->
      Html.text ("error: " ++ msg)

    Nothing ->
      -- Html.table [class "pipeline-table"] (
      --   model.graph
      --     |> Grid.fromGraph
      --     |> Grid.toMatrix nodeHeight
      --     |> Matrix.toList
      --     |> List.map viewRow
      -- )
      Html.div [class "pipeline-grid"] [
        viewGrid (Grid.fromGraph model.graph)
      ]

nodeHeight : Graph.Node Node -> Int
nodeHeight {label} =
  case label of
    JobNode job ->
      max 1 (jobResources job)

    _ ->
      1

viewRow : List (Grid.MatrixCell Node ()) -> Html Msg
viewRow row =
  Html.tr [] <|
    List.map viewMatrixCell row

viewMatrixCell : Grid.MatrixCell Node () -> Html Msg
viewMatrixCell mnode =
  case mnode of
    Grid.MatrixSpacer ->
      Html.td [class "spacer"] []

    Grid.MatrixNode {node} ->
      Html.td [rowspan (nodeHeight node)] [
        viewNode node
      ]

    Grid.MatrixFilled ->
      Html.text ""

viewGrid : Grid Node () -> Html Msg
viewGrid grid =
  case grid of
    Grid.Cell {node} ->
      viewNode node

    Grid.Serial prev next ->
      Html.div [class "serial-grid"]
        (viewSerial prev ++ viewSerial next)

    Grid.Parallel grids ->
      Html.div [class "parallel-grid"] <|
        List.map viewGrid grids

    Grid.End ->
      Html.text ""

viewSerial : Grid Node () -> List (Html Msg)
viewSerial grid =
  case grid of
    Grid.Serial prev next ->
      viewSerial prev ++ viewSerial next

    _ ->
      [viewGrid grid]

viewNode : Graph.Node Node -> Html Msg
viewNode {id,label} =
  let
    idAttr =
      Html.Attributes.id ("node-" ++ toString id)
  in
    case label of
      JobNode job ->
        Html.div [class "node job", idAttr] [
          viewJobNode job
        ]

      InputNode {resourceName} ->
        Html.div [class "node input", idAttr] [
          viewInputNode resourceName
        ]

      ConstrainedInputNode {resourceName} ->
        Html.div [class "node input constrained", idAttr] [
          viewConstrainedInputNode resourceName
        ]

      OutputNode {resourceName} ->
        Html.div [class "node output", idAttr] [
          viewOutputNode resourceName
        ]

viewJobNode : Job -> Html Msg
viewJobNode job =
  let
    linkAttrs =
      case (job.finishedBuild, job.nextBuild) of
        (Just fb, Just nb) ->
          [ class (Concourse.BuildStatus.show fb.status ++ " started")
          , href nb.url
          ]

        (Just fb, Nothing) ->
          [ class (Concourse.BuildStatus.show fb.status)
          , href fb.url
          ]

        (Nothing, Just nb) ->
          [ class "no-builds started"
          , href nb.url
          ]

        (Nothing, Nothing) ->
          [ class "no-builds"
          , href job.url
          ]
  in
    Html.a linkAttrs [ --(style [("line-height", toString (30 * jobResources job - 10) ++ "px")] :: linkAttrs) [
      Html.text job.name
    ]

jobResources : Job -> Int
jobResources {inputs,outputs} =
  Set.size (Set.fromList (List.map .resource inputs ++ List.map .resource outputs))

viewInputNode : String -> Html Msg
viewInputNode resourceName =
  Html.a [href "#"] [Html.text resourceName]

viewConstrainedInputNode : String -> Html Msg
viewConstrainedInputNode resourceName =
  Html.a [href "#"] [Html.text resourceName]

viewOutputNode : String -> Html Msg
viewOutputNode resourceName =
  Html.a [href "#"] [Html.text resourceName]

fetchJobs : Concourse.Job.PipelineLocator -> Cmd Msg
fetchJobs locator =
  Concourse.Job.fetchJobs locator
    |> Task.perform Err Ok
    |> Cmd.map JobsFetched

type alias ByName a =
  Dict String a

initGraph : List Job -> Graph Node ()
initGraph jobs =
  let
    jobNodes =
      List.map JobNode jobs

    jobsByName =
      List.foldl (\job dict -> Dict.insert job.name job dict) Dict.empty jobs

    resourceNodes =
      List.concatMap (jobResourceNodes jobsByName) jobs

    graphNodes =
      List.indexedMap Graph.Node (List.concat [jobNodes, resourceNodes])
  in
    Graph.fromNodesAndEdges
      graphNodes
      (List.concatMap (nodeEdges graphNodes) graphNodes)

jobResourceNodes : ByName Job -> Job -> List Node
jobResourceNodes jobs job =
  List.concatMap (inputNodes jobs job) job.inputs ++
    List.concatMap (outputNodes job) job.outputs

inputNodes : ByName Job -> Job -> Concourse.Job.Input -> List Node
inputNodes jobs job {resource,passed} =
  if List.isEmpty passed then
    [InputNode { resourceName = resource, dependentJob = job }]
  else
    List.map (constrainedInputNode jobs resource job) passed

outputNodes : Job -> Concourse.Job.Output -> List Node
outputNodes job {resource} =
  []
  -- [OutputNode { resourceName = resource, upstreamJob = job }]

constrainedInputNode : ByName Job -> String -> Job -> String -> Node
constrainedInputNode jobs resourceName dependentJob upstreamJobName =
  case Dict.get upstreamJobName jobs of
    Just upstreamJob ->
      ConstrainedInputNode
        { resourceName = resourceName
        , dependentJob = dependentJob
        , upstreamJob = upstreamJob
        }

    Nothing ->
      Debug.crash "impossible: job name not found; invalid pipeline?"

nodeEdges : List (Graph.Node Node) -> Graph.Node Node -> List (Graph.Edge ())
nodeEdges allNodes {id,label} =
  case label of
    JobNode _ ->
      []

    InputNode {dependentJob} ->
      [Graph.Edge id (jobId allNodes dependentJob) ()]

    ConstrainedInputNode {dependentJob,upstreamJob} ->
      [ Graph.Edge (jobId allNodes upstreamJob) id ()
      , Graph.Edge id (jobId allNodes dependentJob) ()
      ]

    OutputNode {upstreamJob} ->
      [Graph.Edge (jobId allNodes upstreamJob) id ()]

jobId : List (Graph.Node Node) -> Job -> Int
jobId nodes job =
  case List.filter ((==) (JobNode job) << .label) nodes of
    {id} :: _ ->
      id

    [] ->
      Debug.crash "impossible: job index not found"
