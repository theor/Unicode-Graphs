module App.Types

open App.Geometry
open App.Graph
[<Struct>]
type RenderOptions = {
    CanvasWidth: int option
    CanvasHeight: int option
    ActualCanvasWidth : int
    ActualCanvasHeight : int
    NodeBorders: bool
    ShowPorts: bool
    Margin: int }
    with
        static member Default: RenderOptions = {
            CanvasWidth=None
            CanvasHeight=None
            Margin=1
            ActualCanvasWidth=1
            ActualCanvasHeight=1
            NodeBorders = false
            ShowPorts = true
        }
type Model = {
    graph: Graph
    options: RenderOptions
    nodeSizes: Map<Id,Rect>
    ports: Map<Id,Port>
    selectedId: Id option
    deltaPos: Pos option

    }
    with
        member this.selectedNode: Node option =
            Option.bind (fun id -> this.graph.nodes |> Map.tryFind id) this.selectedId
        member this.selectedPort: Port option = 
            Option.bind (fun id -> this.ports |> Map.tryFind id) this.selectedId
let newModel(g:Graph) =
    { graph=g
      options=RenderOptions.Default
      nodeSizes = Map.empty
      selectedId=None
      ports = Map.empty
      /// Delta between node pos (top left corner) and actual mouse click (eg. the node center) used when moving a node
      deltaPos = None }
type Msg =
| Move of Id * Pos
| AddNode of Id * string
| SelectNode of Id option * Pos option
| ChangeOptions of RenderOptions
| ChangeNode of Node
| Layout

