module App.GraphRender
open System
open System.Text
open Fable.Core
open Fable.React
open Fable.React.Props
open Browser.Types
open App.Geometry
open App.Graph
open App.Types

let emptyChar = ' '

let layout (model:Model) =
    let mutable nodeSizes = Map.empty<Id,Rect>
    let measureNode guid n =
        let nw,nh = n.title.Length + 2*model.options.Margin + (if model.options.NodeBorders then 2 else 0),
                    if model.options.NodeBorders then 3 else 1
        let x,y = n.pos
        nodeSizes <- Map.add guid (Rect.Create(x,y,nw,nh)) nodeSizes
    model.graph.nodes |> Map.iter measureNode
    let maxW = 2 + (nodeSizes |> Map.fold (fun max _ n -> Math.Max(max, (n.X + n.W))) 0)
    let maxH = 2 + (nodeSizes |> Map.fold (fun max _ n -> Math.Max(max, (n.Y + n.H))) 0)
    let w = Option.defaultValue maxW model.options.CanvasWidth
    let h = Option.defaultValue maxH model.options.CanvasHeight
//    JS.console.log(nodeSizes |> Map.toSeq |> Seq.map (fun (_,x) -> x) |> Seq.toArray, nodeSizes |> Map.toArray |> Array.map (fun (a,b) -> b.Center))
    { model with
        options = {model.options with ActualCanvasWidth=w; ActualCanvasHeight=h}
        nodeSizes = nodeSizes}


[<RequireQualifiedAccess>]
type MouseState = None | Down | Move | Up

let getId (e:MouseEvent): Id option =
    let elem = e.target :?> Browser.Types.HTMLElement
    if not <| JsInterop.isNullOrUndefined elem then
        match elem.dataset.Item "nid" with
        | "" | null -> None
        | s when JsInterop.isNullOrUndefined s -> None
        | s -> UInt32.Parse s |> Id |> Some
    else None

let getNode (model:Model) (e:MouseEvent) : Node option =
    getId e |> Option.bind (fun id -> Map.tryFind id (model.graph.nodes))
let onMouseMove (dispatch: Msg -> unit) (model:Model) (state:MouseState) (e:MouseEvent) =
    let graphElt = e.currentTarget :?> HTMLElement

    let getCurrentCoords(): Pos =
         let rect = graphElt.getBoundingClientRect()
         float model.options.ActualCanvasWidth * (e.clientX - rect.left) / rect.width |> int32,
         float model.options.ActualCanvasHeight * (e.clientY - rect.top) / rect.height |> int32

    match state with
    | MouseState.Down ->
        let newSelectedNode = getId e
        let newPos = newSelectedNode |> Option.bind (model.graph.nodes.TryFind) |> Option.map (fun n -> let x,y = getCurrentCoords() in let nx,ny = n.pos in nx-x, ny-y)
        JS.console.log("START POS", newPos)
        dispatch (SelectNode(newSelectedNode, newPos))
    | MouseState.Up -> dispatch <| SelectNode(model.selectedId, None)
    | MouseState.Move when model.deltaPos.IsNone -> JS.console.log("CURRENT", getCurrentCoords()); ()
    | MouseState.Move ->
//         JS.console.log(e.clientX - graphElt.clientLeft, e.clientY - graphElt.clientTop, graphElt.clientWidth, graphElt.clientHeight)
         match model.selectedNode with
         | None -> ()
         | Some n ->
             JS.console.log("CURRENT", getCurrentCoords(), "START", model.deltaPos, "NODE", n.pos)

             let sx,sy = model.deltaPos.Value;
             let x,y = getCurrentCoords()
             dispatch <| Move(n.guid, (sx+x,sy+y))
    | _ -> failwith "todo"

let render dispatch (model:Model) =
    let options = model.options
    let g = model.graph

    let mutable b = Array.create (options.ActualCanvasWidth*options.ActualCanvasHeight) (emptyChar, Id.Default)
    let set x y c (id:Id) = b.[x + y*options.ActualCanvasWidth] <- (c,id)

    let renderNode guid n =
        let r = Map.find guid model.nodeSizes
        if model.options.NodeBorders then
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
            set (r.X + i + options.Margin + if model.options.NodeBorders then 1 else 0)
                (r.Y + if model.options.NodeBorders then 1 else 0)
                n.title.[i] guid
        ()

    let renderEdge id (e:Edge) =
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
                set i j c id


            if dx <> 0 then i <- i+sx
            if dy <> 0 then j <- j+sy
            ()

    g.nodes |> Map.iter renderNode
    g.edges |> Map.iter renderEdge

    seq {
        yield div [HTMLAttr.Id "graph-output"; ClassName "graph-output"
                   OnMouseMove (onMouseMove dispatch model MouseState.Move)
                   OnMouseDown (onMouseMove dispatch model MouseState.Down)
                   OnMouseUp (onMouseMove dispatch model MouseState.Up)
                   OnDoubleClick ((getNode model) >> (fun x -> JS.console.log("DOUBLE",x))) ]

            (seq {
                for line in Seq.chunkBySize options.ActualCanvasWidth b do
                    let mutable (c,id) = line.[0]
                    let mutable i = 1
                    let mutable sb = StringBuilder(options.ActualCanvasWidth)
                    sb <- sb.Append c

                    let lineContent = seq {

                        while i < line.Length do
                            let c,nextId = line.[i]
                            if id <> nextId then
                                yield span (seq { yield Data("nid", id.Value); if model.selectedId = Some(id) then yield Class "selected" }) [str <| sb.ToString()]
                                sb <- sb.Clear()
                            sb <- sb.Append c
                            id <- nextId
                            i <- i + 1
                        if sb.Length > 0 then
                            yield span [ Data("nid", id.Value) ] [str <| sb.ToString()]
                    }
                    yield pre [] lineContent
                    })
        yield div [ HTMLAttr.Id "hidden-output" ] (
                b |> Seq.map fst |> Seq.chunkBySize options.ActualCanvasWidth |> Seq.map (Seq.toArray >> String) |> Seq.map (fun s -> pre [] [str s])
        )
    }
