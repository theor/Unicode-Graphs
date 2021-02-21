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
    Margin: int }
    with
        static member Default: RenderOptions = {
            CanvasWidth=None
            CanvasHeight=None
            Margin=1
            ActualCanvasWidth=1
            ActualCanvasHeight=1
            NodeBorders = false
        }
type Model = {
    graph: Graph
    options: RenderOptions
    nodeSizes: Map<Id,Rect>
    selectedId: Id option
    startPos: Pos option

    }
    with
        member this.selectedNode: Node option =
            Option.bind (fun id -> this.graph.nodes |> Map.tryFind id) this.selectedId
let newModel(g:Graph) =
    { graph=g
      options=RenderOptions.Default
      nodeSizes = Map.empty
      selectedId=None
      startPos = None }
type Msg =
| Move of Id * Pos
| AddNode of Id * string
| SelectNode of Id option * Pos option
| ChangeOptions of RenderOptions
| ChangeNode of Node
| Layout


//let mutable selectedNode: Node option = None
//let selectedId() = selectedNode |> Option.map (fun n -> n.guid)