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

let emptyChar = '.'
let portChar = "\u25CC"

let hasTitle n = not <| String.IsNullOrWhiteSpace n.title
let layout (model:Model) =
    let ifBordersThen2 = (if model.options.NodeBorders then 2 else 0)
    let mutable nodeSizes = Map.empty<Id,Rect>
    let measureNode guid n =
        let titleHeight = if hasTitle n then 1 else 0
        let portWidth =
            if model.options.ShowPorts then
                seq {
                    for i in 0..Math.Max(n.inputs.Length,n.outputs.Length)-1 do
                        match List.tryItem i n.inputs, List.tryItem i n.outputs with
                        | Some(a),Some(b) -> a.title.Length+b.title.Length+3 (* 2*port char + space between them*) + ifBordersThen2
                        | Some(x),None | None,Some(x) -> x.title.Length+1+1 + ifBordersThen2
                        | _ -> failwith "Impossible"
                }
//                |> Seq.map (fun x -> JS.console.log(x); x)
                |> (fun l -> if Seq.isEmpty l then Seq.replicate 1 0 else l)
                |> Seq.max
            else 0
        
        
        let nw,nh = n.title.Length + 2*model.options.Margin + ifBordersThen2,
                    (if model.options.NodeBorders then 2 else 0) + titleHeight + if model.options.ShowPorts then Math.Max(n.inputs.Length, n.outputs.Length) else 0
        let nw = Math.Max(nw, portWidth)
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
        nodeSizes = nodeSizes
        ports = model.graph.nodes
                |> Map.toSeq
                |> Seq.map (fun (k,n) -> (n,Seq.concat [Seq.indexed n.inputs; Seq.indexed n.outputs]))
                |> Seq.collect (fun (n,ports) -> ports |> Seq.map (fun (i,p) -> p.guid,{port=p; ownerNode=n.guid;index=uint i}))
                |> Map.ofSeq
    }


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
    let ifBorderThenOne = if model.options.NodeBorders then 1 else 0

    match state with
    | MouseState.Down ->
        let pickedId = getId e
        let newSelectedId,newPos =  
            match pickedId |> Option.bind (model.graph.nodes.TryFind) with
            | None ->
                match pickedId |> Option.bind (model.ports.TryFind) with
                | None -> None,None
                | Some p -> pickedId, Some <| getCurrentCoords() 
            | Some n ->
                let x,y = getCurrentCoords() in
                let nx,ny = n.pos in
                pickedId, Some (nx-x, ny-y)

        JS.console.log("START POS", newPos)
        dispatch (SelectNode(newSelectedId, newPos))
    | MouseState.Up ->
        let pickedId = getId e
        match model.selectedPort, model.edgeCandidate with
        | Some from, Some _pos ->
            match pickedId |> Option.bind (model.ports.TryFind) with
            | Some targetPort -> dispatch (CreateEdge(from.guid, targetPort.port.guid))
            | _ -> ()
        | _ -> dispatch <| SelectNode(pickedId, None)
    | MouseState.Move when model.deltaPos.IsNone ->  ()
    | MouseState.Move ->
        let x,y = getCurrentCoords()
        match model with
        | SelectedNode n ->
            let sx,sy = model.deltaPos.Value;
            dispatch <| Move(n.guid, (sx+x,sy+y))
        | SelectedPort p ->
            dispatch <| EdgeCandidate (x,y)
//         JS.console.log(e.clientX - graphElt.clientLeft, e.clientY - graphElt.clientTop, graphElt.clientWidth, graphElt.clientHeight)
//        model.selectedNode |> Option.iter (fun n ->
//            let sx,sy = model.deltaPos.Value;
//            let x,y = getCurrentCoords()
//            dispatch <| Move(n.guid, (sx+x,sy+y)))
            
    | _ -> failwith "todo"

let render dispatch (model:Model) =
    let options = model.options
    let g = model.graph
    let ifBorderThenOne = if model.options.NodeBorders then 1 else 0

    let mutable b = Array.create (options.ActualCanvasWidth*options.ActualCanvasHeight) (emptyChar, Id.Default)
    let set x y c (id:Id) =
        if x + y*options.ActualCanvasWidth < b.Length then
            b.[x + y*options.ActualCanvasWidth] <- (c,id)
    
    let renderLabel x y (s:string) (id:Id) =
        for i in 0..s.Length - 1 do
            set (x+i) (y) s.[i] id

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
        let titleHeight = if hasTitle n then 1 else 0
        if hasTitle n then
            renderLabel (r.X + r.W / 2 - n.title.Length / 2)// (r.X  + options.Margin + if model.options.NodeBorders then 1 else 0)
                (r.Y + ifBorderThenOne)
                n.title guid
//        for i in 0.. n.title.Length-1 do
//            set (r.X  + options.Margin + if model.options.NodeBorders then 1 else 0+ i)
//                (r.Y + if model.options.NodeBorders then 1 else 0)
//                n.title.[i] guid
        if model.options.ShowPorts then
            n.inputs |> List.iteri (fun i p -> renderLabel (r.X+1) (r.Y + titleHeight + i + ifBorderThenOne) (portChar + p.title) p.guid)
            n.outputs |> List.iteri (fun i p -> renderLabel (r.X + r.W - 2*ifBorderThenOne - p.title.Length) (r.Y + titleHeight + i + ifBorderThenOne) (p.title + portChar) p.guid)
        ()
        
    let getPortPosition (r:Rect) (isInput:bool) (index:uint) =
        let y = r.Y + (*if hasTitle n then 1 else 0*) 1 + int index + ifBorderThenOne
        let x = if isInput then r.X else (r.X+r.W)
        x,y

    let renderEdgeFromTo id rfx rfy rtx rty offset =
        let mutable (i,j) = rfx,rfy
        let edgeCenterX, edgeCenterY = (i+rtx)/2+offset, (j+rty)/2
        let dirX, dirY = sign (rtx-i), sign (rty-j)
        let needHorizontalMove = i <> edgeCenterX 
        
        // horiz bar until before the corner
        if needHorizontalMove then
            while i <> edgeCenterX-dirX do
                set i j '\u2500' id
                i <- i + dirX
            
        // 1st corner
        match dirY with
        | 1 ->
            set i j '\u2510' id
            j <- j + dirY
        | -1 ->
            set i j '\u2518' id
            j <- j + dirY
        | _ -> set i j '\u2500' id
            
        // vertical bar until 2nd corner
        while j <> rty do
            set i j '\u2502' id
            j <- j + dirY
            
        // 2nd corner
        match dirY with
        | 1 ->
            set i j '\u2514' id
            i <- i + dirX
        | -1 ->
            set i j '\u250c' id
            i <- i + dirX
        | _ -> set i j '\u2500' id
        
        // from 2nd corner to other port
        
        if needHorizontalMove then
            while i <> rtx do
                set i j '\u2500' id
                i <- i + dirX
//        while i <> rtx || j <> rty do
//
//            let dx,dy = (rtx-i),(rty-j)
//            let sx,sy = Math.Sign dx, Math.Sign dy
//
//            if not (Rect.contains (i,j) rf) && not (Rect.contains (i,j) rt)
//            then
//                let c = match sx,sy with
//                         | 1, 1 | -1, -1 -> '\\'// '\u22F1'
//                         | 1, -1 | -1, 1 -> '/'// '\u22F0'
//                         | 0, -1 | 0, 1 -> '|'//'\u22ee'
//                         | 1, 0 | -1, 0 -> '\u2500'// '\u22ef'
//                         | _ -> 'o'
//                set i j c id
//
//
//            if dx <> 0 then i <- i+sx
//            if dy <> 0 then j <- j+sy
//            ()

    let renderEdge id (e:Edge) =
        let fromNode, fromIndex = e.fromNode
        let rf = model.nodeSizes.Item fromNode
        let rfx,rfy = if not options.ShowPorts || fromIndex = UInt32.MaxValue then rf.Center else getPortPosition rf false fromIndex in

        let toNode, toIndex = e.toNode
        let rt = model.nodeSizes.Item toNode
        let rtx,rty =  if not options.ShowPorts || toIndex = UInt32.MaxValue then rt.Center else getPortPosition rt true toIndex

        renderEdgeFromTo id rfx rfy rtx rty e.offset

    let renderEdgeCandidate (fromNode:Id) (fromIndex:uint) toX toY =
        let rf = model.nodeSizes.Item fromNode
        let rfx,rfy = if not options.ShowPorts || fromIndex = UInt32.MaxValue then rf.Center else getPortPosition rf false fromIndex in

        renderEdgeFromTo (Id 0u) rfx rfy toX toY 0
        
    g.nodes |> Map.iter renderNode
    g.edges |> Map.iter renderEdge
    if model.selectedPort.IsSome then
        model.edgeCandidate |> Option.iter (fun (x,y) ->
            let {port=port;ownerNode=node;index=index} = Map.find model.selectedPort.Value.guid model.ports
            renderEdgeCandidate node index x y
        )

    seq {
        yield div [HTMLAttr.Id "graph-output"; ClassName "graph-output"
                   OnMouseMove (onMouseMove dispatch model MouseState.Move)
                   OnMouseDown (onMouseMove dispatch model MouseState.Down)
                   OnMouseUp (onMouseMove dispatch model MouseState.Up)
                   OnDoubleClick ((getNode model) >> (fun x -> JS.console.log("DOUBLE",x))) ]

            (seq {
                for line in Seq.chunkBySize options.ActualCanvasWidth b do
                    match Array.tryItem 0 line with
                    | None -> ()
                    | Some(c,id) ->
                        let mutable i = 1
                        let mutable id = id
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
