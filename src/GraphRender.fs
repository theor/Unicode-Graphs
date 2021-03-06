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
let portChar = "\u25CC"

[<RequireQualifiedAccess>]
type MouseState =
    | None
    | Down
    | Move
    | Up

let getId (e: MouseEvent): Id option =
    let elem = e.target :?> HTMLElement
    //    JS.console.log("Under cursor:", elem, e)
    if not <| JsInterop.isNullOrUndefined elem then
        match elem.dataset.Item "nid" with
        | ""
        | null -> None
        | s when JsInterop.isNullOrUndefined s -> None
        | s -> UInt32.Parse s |> Id |> Some
    else
        None

let getNode (model: Model) (e: MouseEvent): Node option =
    getId e
    |> Option.bind (fun id -> Map.tryFind id (model.graph.nodes))

let keyToMessage (e: KeyboardEvent): Msg option =
    //    JS.console.log(e.key, e.code, e.ctrlKey)
    match e.key, e.ctrlKey with
    | "z", true -> Some (UndoRedo true)
    | "y", true -> Some (UndoRedo false)
    | "d", true -> Some Duplicate
    | "Delete", _ | "Backspace",_ -> Some Delete
    | _ -> None

let onMouseMove (dispatch: Msg -> unit) (model: Model) (state: MouseState) (e: MouseEvent) =
//    e.preventDefault ()
    let graphElt = e.currentTarget :?> HTMLElement

    let getCurrentCoords (): Pos =
        let rect = graphElt.getBoundingClientRect ()

        float model.options.ActualCanvasWidth
        * (e.clientX - rect.left)
        / rect.width
        |> int32,
        float model.options.ActualCanvasHeight
        * (e.clientY - rect.top)
        / rect.height
        |> int32

    match state with
    | MouseState.Down ->
        let pickedId = getId e

        let newSelectedId, newPos =
            match pickedId
                  |> Option.bind (model.graph.nodes.TryFind) with
            | None ->
                match pickedId |> Option.bind (model.ports.TryFind) with
                | None -> None, None
                | Some _ -> pickedId, Some <| getCurrentCoords ()
            | Some n ->
                let x, y = getCurrentCoords () in
                let nx, ny = n.pos in
                pickedId, Some(nx - x, ny - y)

        //        JS.console.log("START POS", newPos, pickedId.ToString())
        dispatch (SelectNode(newSelectedId, newPos))
    | MouseState.Up ->
        let pickedId = getId e
        //        JS.console.log("PICKED", pickedId)
        match model.selectedPort, model.edgeCandidate with
        | Some from, Some _pos ->
            match pickedId |> Option.bind (model.ports.TryFind) with
            | Some targetPort ->
                dispatch
                    (if targetPort.direction = Direction.Output
                     then CreateEdge(targetPort.port.guid, from.guid)
                     else CreateEdge(from.guid, targetPort.port.guid))
            | _ -> dispatch <| SelectNode(pickedId, None)
        | _ -> dispatch <| SelectNode(pickedId, None)
    | MouseState.Move when model.deltaPos.IsNone ->
        let pickedId = getId e
        dispatch <| Highlight pickedId
    | MouseState.Move ->
        let x, y = getCurrentCoords ()
        let pickedId = getId e

        match model with
        | SelectedNode n ->
            let sx, sy = model.deltaPos.Value
            dispatch <| Move(n.guid, (sx + x, sy + y))
        | SelectedPort _ -> dispatch <| EdgeCandidate((x, y), pickedId)
        | _ -> ()
    //         JS.console.log(e.clientX - graphElt.clientLeft, e.clientY - graphElt.clientTop, graphElt.clientWidth, graphElt.clientHeight)
//        model.selectedNode |> Option.iter (fun n ->
//            let sx,sy = model.deltaPos.Value;
//            let x,y = getCurrentCoords()
//            dispatch <| Move(n.guid, (sx+x,sy+y)))

    | _ -> failwith "todo"

let render key editable dispatch (model: Model) =
    let options = model.options
    let g = model.graph

    let ifBorderThenOne = 1

    let mutable b =
        Array.create
            (options.ActualCanvasWidth
             * options.ActualCanvasHeight)
            (emptyChar, Id.Default)

    let set x y c (id: Id) =
        if x + y * options.ActualCanvasWidth < b.Length
        then b.[x + y * options.ActualCanvasWidth] <- (c, id)

    let renderLabel x y (s: string) (id: Id) =
        if not model.options.ShowIds then
            for i in 0 .. s.Length - 1 do
                set (x + i) (y) s.[i] id
        else
            let sid = id.ToString()

            for i in 0 .. sid.Length - 1 do
                set (x + i) (y) sid.[i] id

    let renderNode guid n =
        let r = Map.find guid model.nodeSizes

        for j in 0 .. r.H - 1 do
            for i in 0 .. r.W - 1 do
                let c =
                    match (i, j) with
                    | 0, 0 -> '\u250c' // top left
                    | (0, _) when j = r.H - 1 -> '\u2514' // bottom left
                    | _, 0 when i = r.W - 1 -> '\u2510' // top right
                    | _, _ when i = r.W - 1 && j = r.H - 1 -> '\u2518' // bottom right
                    | _, _ when j = 0 || j = r.H - 1 -> '\u2500' // top or bottom
                    | _, _ when i = 0 || i = r.W - 1 -> '\u2502' // left or right
                    | _ -> '.'

                set
                    (r.X + i)
                    (r.Y + j)
                    (if i = 0 || i = (r.W - 1) || j = (r.H - 1) || j = 0
                     then c
                     else ' ')
                    guid

        let titleHeight = if GraphLayout.hasTitle n then 1 else 0

        if GraphLayout.hasTitle n then
            renderLabel
                (r.X + r.W / 2 - n.title.Length / 2)
                (r.Y + ifBorderThenOne)
                n.title
                guid
        n.inputs
        |> List.iteri (fun i p ->
            renderLabel (r.X + 1) (r.Y + titleHeight + i + ifBorderThenOne) (portChar + p.title) p.guid)

        n.outputs
        |> List.iteri (fun i p ->
            renderLabel
                (r.X + r.W - 2 * ifBorderThenOne - p.title.Length)
                (r.Y + titleHeight + i + ifBorderThenOne)
                (p.title + portChar)
                p.guid)

        ()

    let renderEdgeFromTo id rfx rfy rtx rty (offset: int8) =
        let mutable (i, j) = rfx, rfy
        let edgeCenterX = (i + rtx) / 2 + (int offset)
        let dirX, dirY = sign (rtx - i), sign (rty - j)
        let needHorizontalMove = i <> edgeCenterX

        // horiz bar until before the corner
        if needHorizontalMove then
            while i <> edgeCenterX - dirX do
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

    let getPortPosition (nodeId: Id) (portIndex: uint8) (dir: Direction): Pos =
        //        JS.console.log(sprintf "Get port position %A %A %A" nodeId portIndex dir)
        let node = Map.find nodeId model.graph.nodes

        let portList =
            (if dir = Direction.Input then node.inputs else node.outputs)

        let port = portList |> List.tryItem (int portIndex)

        port
        |> Option.map (fun x -> x.guid)
        |> Option.bind (fun guid -> Map.tryFind guid model.ports)
        |> Option.map (fun e -> e.position)
        |> Option.defaultWith (fun () ->
            JS.console.error (sprintf "Cannot find %A port %A of node %A" dir portIndex nodeId)
            0, 0)

    let renderEdge id (e: Edge) =
        let fromNodeId, fromIndex = e.fromNode
        let toNodeId, toIndex = e.toNode

        Option.map2 (fun a b -> a, b) (Map.tryFind fromNodeId model.nodeSizes) (Map.tryFind toNodeId model.nodeSizes)
        |> Option.iter (fun (rf, rt) ->
            let rfx, rfy =
                if fromIndex = Byte.MaxValue
                then rf.Center
                else getPortPosition fromNodeId fromIndex Direction.Output

            let rtx, rty =
                if toIndex = Byte.MaxValue
                then rt.Center
                else getPortPosition toNodeId toIndex Direction.Input


            renderEdgeFromTo id rfx rfy rtx rty e.offset)

    let renderEdgeCandidate (fromNode: Id) (fromIndex: uint8) (fromDir: Direction) toX toY =
        let rf = model.nodeSizes.Item fromNode

        let rfx, rfy =
            if fromIndex = Byte.MaxValue
            then rf.Center
            else getPortPosition fromNode fromIndex fromDir

        match fromDir with
        | Direction.Output -> renderEdgeFromTo (Id 0u) rfx rfy toX toY 0y
        | Direction.Input -> renderEdgeFromTo (Id 0u) (toX + 1) toY rfx rfy 0y

    g.nodes |> Map.iter renderNode
    g.edges |> Map.iter renderEdge

    if editable && model.selectedPort.IsSome then
        model.edgeCandidate
        |> Option.iter (fun (x, y) ->
            let { direction = direction
                  ownerNode = node
                  index = index } =
                Map.find model.selectedPort.Value.guid model.ports

            renderEdgeCandidate node index direction x y)

    div
        [ yield Prop.Key key
          yield HTMLAttr.Id "graph-output"
          yield ClassName "graph-output"
          if editable then
              yield OnMouseMove(onMouseMove dispatch model MouseState.Move)
              yield OnMouseDown(onMouseMove dispatch model MouseState.Down)
              yield OnMouseUp(onMouseMove dispatch model MouseState.Up)
              yield OnDoubleClick
                  ((getNode model)
                   >> (fun x -> JS.console.log ("DOUBLE", x))) ]

        (seq {
            for line in Seq.chunkBySize options.ActualCanvasWidth b do
                match Array.tryItem 0 line with
                | None -> ()
                | Some (c, id) ->
                    let mutable i = 1
                    let mutable id = id
                    let mutable sb = StringBuilder(options.ActualCanvasWidth)
                    sb <- sb.Append c

                    let makeSpan () =
                        span
                            (seq {
                                yield Data("nid", id.Value)
                                if id.Value <> 0u then yield Class "is-clickable"
                                if editable && model.selectedId = Some(id) then yield Class "selected has-text-primary"

                                if editable && model.highlightedId = Some(id)
                                   && model.highlightedId <> model.selectedId then
                                    yield Class "highlighted has-text-info"
                             })
                            [ str <| sb.ToString() ]

                    let lineContent =
                        seq {

                            while i < line.Length do
                                let c, nextId = line.[i]

                                if id <> nextId then
                                    yield makeSpan ()
                                    sb <- sb.Clear()

                                sb <- sb.Append c
                                id <- nextId
                                i <- i + 1

                            if sb.Length > 0 then yield makeSpan ()
                        }

                    yield pre [] lineContent
         })
let renderReadOnly key (model: Model) = render key false (fun _ -> ()) model
let renderEditable key dispatch (model: Model) = render key true dispatch model
