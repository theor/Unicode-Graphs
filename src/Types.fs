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
}
type Msg =
| Move of Id * Pos
| Layout