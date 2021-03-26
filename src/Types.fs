module App.Types

open App.Geometry
open App.Graph
open Thoth.Elmish

[<Struct>]
type RenderOptions =
    { CanvasWidth: int option
      CanvasHeight: int option
      ActualCanvasWidth: int
      ActualCanvasHeight: int
      ShowIds: bool }
    static member Default: RenderOptions =
        { CanvasWidth = None
          CanvasHeight = None
          ActualCanvasWidth = 1
          ActualCanvasHeight = 1
          ShowIds = false }

type PortEntry =
    { port: Port
      ownerNode: Id
      index: uint8
      position: Pos
      direction: Direction }

[<RequireQualifiedAccess>]
type GraphState = Ready | PendingLayout

type Model =
    { graph: Graph
      options: RenderOptions
      nodeSizes: Map<Id, Rect>
      ports: Map<Id, PortEntry>
      highlightedId: Id option
      selectedId: Id option
      deltaPos: Pos option
      edgeCandidate: Pos option
      isBurgerOpen: bool
      Debouncer: Debouncer.State
      GraphState: GraphState }
    member this.selectedNode: Node option =
        Option.bind (fun id -> this.graph.nodes |> Map.tryFind id) this.selectedId

    member this.selectedEdge: Edge option =
        Option.bind (fun id -> this.graph.edges |> Map.tryFind id) this.selectedId

    member this.selectedPort: Port option =
        this.selectedId
        |> Option.bind (fun id -> this.ports |> Map.tryFind id)
        |> Option.map (fun x -> x.port)


let newModel (g: Graph) =
    { graph = g
      options = RenderOptions.Default
      nodeSizes = Map.empty
      selectedId = None
      highlightedId = None
      ports = Map.empty
      /// Delta between node pos (top left corner) and actual mouse click (eg. the node center) used when moving a node
      deltaPos = None
      edgeCandidate = None
      isBurgerOpen = false
      Debouncer = Debouncer.create ()
      GraphState = GraphState.PendingLayout }

let (|SelectedNode|_|) (model: Model) =
    Option.bind (fun id -> model.graph.nodes |> Map.tryFind id) model.selectedId

let (|SelectedEdge|_|) (model: Model) =
    Option.bind (fun id -> model.graph.edges |> Map.tryFind id) model.selectedId

let (|SelectedPort|_|) (model: Model) =
    Option.bind (fun id -> model.ports |> Map.tryFind id) model.selectedId

[<RequireQualifiedAccess>]
type Format =
    | Json
    | B64

type Msg =
    | Move of Id * Pos
    | AddNode of Id * string
    | SelectNode of Id option * Pos option
    | ChangeOptions of RenderOptions
    | ChangeNode of Node
    | ChangeEdge of Edge
    | EdgeCandidate of Pos * Id option
    | Highlight of Id option
    | CreateEdge of Id * Id
    | Delete
    | Duplicate
    | UndoRedo of bool
    | Layout
    | EndDragAndDrop
    | ReadClipboard
    | LoadJson of string * Format
    | ToggleBurger
    | DebouncerSelfMsg of Debouncer.SelfMessage<Msg>
    | UpdateUrl
    | GetShortUrl
    | GotShortUrl of Result<string, string>
