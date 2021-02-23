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
type PortEntry ={port:Port; ownerNode:Id; index:uint} 
type Model = {
    graph: Graph
    options: RenderOptions
    nodeSizes: Map<Id,Rect>
    ports: Map<Id,PortEntry>
    selectedId: Id option
    deltaPos: Pos option
    edgeCandidate: Pos option

    }
    with
        member this.selectedNode: Node option =
            Option.bind (fun id -> this.graph.nodes |> Map.tryFind id) this.selectedId
        member this.selectedPort: Port option = 
             this.selectedId |> Option.bind (fun id -> this.ports |> Map.tryFind id)
             |> Option.map (fun x -> x.port)
            
let newModel(g:Graph) =
    { graph=g
      options=RenderOptions.Default
      nodeSizes = Map.empty
      selectedId=None
      ports = Map.empty
      /// Delta between node pos (top left corner) and actual mouse click (eg. the node center) used when moving a node
      deltaPos = None
      edgeCandidate = None }
    
let (|SelectedNode|_|) (model : Model) = Option.bind (fun id -> model.graph.nodes |> Map.tryFind id) model.selectedId
let (|SelectedEdge|_|) (model : Model) = Option.bind (fun id -> model.graph.edges |> Map.tryFind id) model.selectedId
let (|SelectedPort|_|) (model : Model) = Option.bind (fun id -> model.ports |> Map.tryFind id) model.selectedId
    
type Msg =
| Move of Id * Pos
| AddNode of Id * string
| SelectNode of Id option * Pos option
| ChangeOptions of RenderOptions
| ChangeNode of Node
| EdgeCandidate of Pos
| Layout

