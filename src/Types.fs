module App.Types

open App.Geometry
open App.Graph
[<Struct>]
type RenderOptions = {
    CanvasWidth: int option
    CanvasHeight: int option
    ActualCanvasWidth : int
    ActualCanvasHeight : int
    Margin: int }
    with
        static member Default: RenderOptions = { CanvasWidth=None;CanvasHeight=None;Margin=1; ActualCanvasWidth=1; ActualCanvasHeight=1 }
type Model = {
    graph: Graph
    options: RenderOptions
    nodeSizes: Map<Id,Rect>
    selectedNode: Node option
    startPos: Pos option

    }
    with
        member this.selectedId() = this.selectedNode |> Option.map (fun n -> n.guid)
let newModel(g:Graph) =
    { graph=g
      options=RenderOptions.Default
      nodeSizes = Map.empty
      selectedNode=None
      startPos = None }
type Msg =
| Move of Id * Pos
| AddNode of Id * string
| SelectNode of Node option * Pos option
| Layout


//let mutable selectedNode: Node option = None
//let selectedId() = selectedNode |> Option.map (fun n -> n.guid)