module App

open System
open System.Text
open Browser.Types
open Elmish
open Elmish.React
open Fable.Core
open Fable.Import
open Fable.Import
open Fable.React
open Fable.React.Props
open App.Graph
open App.Geometry


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
// Mutable variable to count the number of times we clicked the button




let emptyChar = ' '


let layout (model:Model) =
    let mutable nodeSizes = Map.empty<Id,Rect>
    let measureNode guid n =
        let nw,nh = n.title.Length + 2 + 2*model.options.Margin,3
        let x,y = n.pos
        nodeSizes <- Map.add guid (Rect.Create(x,y,nw,nh)) nodeSizes
    model.graph.nodes |> Map.iter measureNode
    let maxW = 2 + (nodeSizes |> Map.fold (fun max _ n -> Math.Max(max, (n.X + n.W))) 0)
    let maxH = 1 + (nodeSizes |> Map.fold (fun max _ n -> Math.Max(max, (n.Y + n.H))) 0)
    let w = Option.defaultValue maxW model.options.CanvasWidth
    let h = Option.defaultValue maxH model.options.CanvasHeight
    { model with
        options = {model.options with ActualCanvasWidth=w; ActualCanvasHeight=h}
        nodeSizes = nodeSizes}
let render (model:Model) =
    let options = model.options
    let g = model.graph

    JS.console.log(Map.toArray model.nodeSizes)
    
    let mutable b = Array.create (options.ActualCanvasWidth*options.ActualCanvasHeight) (emptyChar, Id.Default)
    let set x y c (id:Id) = b.[x + y*options.ActualCanvasWidth] <- (c,id)
    
    let renderNode guid n =
        let r = Map.find guid model.nodeSizes
        for j in 0..r.H-1 do
        for i in 0..r.W-1 do
            let c = match (i,j) with
                    | 0,0 -> '\u250c' // top left
                    | (0, _) when j = r.H - 1 -> '\u2514' // bottom left
                    | _,0 when i = r.W-1 -> '\u2510' // top right
                    | _,_ when i = r.W-1 && j = r.H-1 -> '\u2518' // bottom right
                    | _,_ when j = 0 || j = r.H-1 -> '\u2500' // top or bottom
                    | _,_ when i = 0 || i = r.W-1 -> '\u2502'  // left or right
                    | _ -> '.'
            set (r.X+i)(r.Y+j) (if i = 0 || i = (r.W-1) || j = (r.H - 1) || j = 0 then c else ' ') guid
        for i in 0.. n.title.Length-1 do
            set (r.X+1+i+options.Margin) (r.Y+1) n.title.[i] guid
        ()
        
    
    let renderEdge (e:Edge) =
        if not e.isNodeEdge then failwith "NOT NODE EDGE"
        let rf = model.nodeSizes.Item e.fromPort
        let rt = model.nodeSizes.Item e.toPort
        let mutable (i,j) = rf.Center in
        let rtx,rty = rt.Center
        while i <> rtx || j <> rty do
            
            let dx,dy = (rtx-i),(rty-j)
            let sx,sy = Math.Sign dx, Math.Sign dy
            
            if not (Rect.contains (i,j) rf) && not (Rect.contains (i,j) rt)
            then
                let c = match sx,sy with
                         | 1, 1 | -1, -1 -> '\\'// '\u22F1'
                         | 1, -1 | -1, 1 -> '/'// '\u22F0'
                         | 0, -1 | 0, 1 -> '|'//'\u22ee'
                         | 1, 0 | -1, 0 -> '\u2500'// '\u22ef'
                         | _ -> 'o'
                set i j c EdgeId
            
            
            if dx <> 0 then i <- i+sx
            if dy <> 0 then j <- j+sy
            ()
    
    g.nodes |> Map.iter renderNode
    g.edges |> List.iter renderEdge
    seq {
        for line in Seq.chunkBySize options.ActualCanvasWidth b do
            let mutable (c,id) = line.[0]
            let mutable i = 1
            let mutable sb = StringBuilder(options.ActualCanvasWidth)
            sb <- sb.Append c
            
            let lineContent = seq {
            
                while i < line.Length do
                    let c,nextId = line.[i]
                    if id <> nextId then
                        yield span [ Data("nid", id.Value) ] [str <| sb.ToString()]
                        sb <- sb.Clear()
                    sb <- sb.Append c
                    id <- nextId
                    i <- i + 1
                if sb.Length > 0 then 
                    yield span [ Data("nid", id.Value) ] [str <| sb.ToString()]
            }
            yield pre [] lineContent
    }



let init() : Model =
    let gb = GraphBuilder()
    let a = gb.AddNode("A Node", (1,1))
    let b = gb.AddNode("B Node", (15,10))
    let c = gb.AddNode("C Node", (15,1))
    let d = gb.AddNode("D Node", (25,5))
    gb.AddNodeEdge(a,b)
    gb.AddNodeEdge(a,c)
    gb.AddNodeEdge(c,b)
    gb.AddNodeEdge(c,d)
    { graph=gb.Build()
      options=RenderOptions.Default
      nodeSizes = Map.empty } |> layout

// UPDATE

let update (msg:Msg) (model:Model) =
    match msg with
    | Layout -> layout model
    | Move(n, newPos) -> {model with graph = {model.graph with nodes = Map.change n (fun x -> { x.Value with pos = newPos } |> Some) model.graph.nodes}} |> layout
    | _ -> model

// VIEW (rendered with React)

let mutable selectedNode: Node option = None
let mutable startPos: Pos option = None
[<RequireQualifiedAccess>]
type MouseState = None | Down | Move | Up
let onMouseMove (dispatch: Msg -> unit) (model:Model) (state:MouseState) (e:MouseEvent) =
    let graphElt = e.currentTarget :?> HTMLElement
    let getId (): Id option =
        let elem = e.target :?> Browser.Types.HTMLElement
        if not <| JsInterop.isNullOrUndefined elem then
            match elem.dataset.Item "nid" with
            | "" | null -> None
            | s when JsInterop.isNullOrUndefined s -> None
            | s -> UInt32.Parse s |> Id |> Some
        else None
    let getNode() : Node option =
        getId() |> Option.bind (fun id -> Map.tryFind id (model.graph.nodes))
    let getCurrentCoords(): Pos =
         let rect = graphElt.getBoundingClientRect()
         float model.options.ActualCanvasWidth * (e.clientX - rect.left) / rect.width |> int32,
         float model.options.ActualCanvasHeight * (e.clientY - rect.top) / rect.height |> int32 
    match state with
    | MouseState.Down ->
        selectedNode <- getNode()
        startPos <- getCurrentCoords() |> Some
        printfn "Selected: %A cur pos: %A" selectedNode startPos
    | MouseState.Up ->
        selectedNode <- None
        startPos <- None
    | MouseState.Move when startPos.IsNone -> ()
    | MouseState.Move ->
         
         let sx,sy = startPos.Value;
         let x,y = getCurrentCoords()
         let dx,dy = x-sx, y-sy
//         JS.console.log (getCurrentCoords())
//         JS.console.log(e.clientX - graphElt.clientLeft, e.clientY - graphElt.clientTop, graphElt.clientWidth, graphElt.clientHeight)
         match selectedNode with
         | None -> ()
         | Some n ->
             let nx,ny = n.pos
             dispatch <| Move(n.guid, (nx + dx, ny + dy))
    | _ -> failwith "todo"
//    JS.console.log("button",e.buttons)
   

let view (model:Model) dispatch =
  let doCopy () =
      let text = Browser.Dom.document.getElementById("graph-output").innerText
      Browser.Navigator.navigator.
      match Browser.Navigator.navigator.clipboard with
      | None -> JS.console.log(text)
      | Some clipboard -> clipboard.writeText(text) |> ignore
  div []
      [ div [HTMLAttr.Id "graph-output"; ClassName "graph-output"
             OnMouseMove (onMouseMove dispatch model MouseState.Move)
             OnMouseDown (onMouseMove dispatch model MouseState.Down)
             OnMouseUp (onMouseMove dispatch model MouseState.Up) ]
             (render model)
        button [ OnClick (fun _ -> doCopy ()) ] [ str "Copy" ]]
//        button [ OnClick (fun _ -> dispatch Increment) ] [ str "+" ]
//        div [] [ str (string model) ]
//        button [ OnClick (fun _ -> dispatch Decrement) ] [ str "-" ] ]

printfn "asd"
// App
Program.mkSimple init update view
|> Program.withReactSynchronous "elmish-app"
|> Program.withConsoleTrace
|> Program.run