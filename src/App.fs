module App

open Elmish
open Elmish.React
open Elmish.Debug
open App.Graph
open App.Types
open App.GraphLayout
open App.View

let init() : Model * Cmd<Msg> =
    let gb = GraphBuilder()
    let a = gb.AddNode("", (1,1), [], ["O1"])
    let b = gb.AddNode("B Node", (25,1), ["BI"; "BI2"], ["BO1"])
//    let c = gb.AddNode("C Node", (15,1))
//    let d = gb.AddNode("D Node", (25,5))
    gb.AddEdge(a, 0u,b, 1u) |> ignore
//    gb.AddEdge(a, 1u,b, 0u) |> ignore
//    gb.AddNodeEdge(a,c) |> ignore
//    gb.AddNodeEdge(c,b) |> ignore
//    gb.AddNodeEdge(c,d) |> ignore
    newModel(gb.Build()) |> layout |> (fun m -> {m with selectedId = Some b}), []

// UPDATE

let update (msg:Msg) (model:Model): Model * Cmd<Msg> =
    match msg with
    | Layout -> layout model, Cmd.none
    | Move(n, newPos) -> {model with graph = {model.graph with nodes = Map.change n (fun x -> { x.Value with pos = newPos } |> Some) model.graph.nodes}} |> layout, Cmd.none
    | SelectNode(n,startPos) -> {model with selectedId = n; deltaPos = startPos}, Cmd.none
    | AddNode(id, title) ->
        let gb = GraphBuilder(model.graph)
        ignore <| gb.AddNode(title=title, id=id)
        {model with graph = gb.Build() } |> layout, Cmd.none
    | ChangeOptions options -> { model with options = options } |> layout, Cmd.none
    | ChangeEdge n ->
        { model with graph = GraphBuilder(model.graph).UpdateEdge(n).Build() } |> layout, Cmd.none
    | ChangeNode n ->
        { model with graph = GraphBuilder(model.graph).UpdateNode(n).Build() } |> layout, Cmd.none
//        |> (fun m -> if m.selectedId() = Some(n.guid) then {m with selectedNode})
    | EdgeCandidate pos -> { model with edgeCandidate = Some pos }, Cmd.none
    | CreateEdge(fromId, toId) ->
        let {ownerNode=fromNode; index=fromIndex; direction=fromDir} = (Map.find fromId model.ports)
        let {ownerNode=toNode; index=toIndex; direction=toDir} = (Map.find toId model.ports)
        let newGraph = if fromNode = toNode || fromDir = toDir
                       then model.graph
                       else GraphBuilder(model.graph).AddEdge(fromNode, fromIndex, toNode, toIndex).Build()
        { model with
            graph = newGraph
            edgeCandidate = None
            deltaPos = None
        } |> layout, Cmd.none
    | Duplicate -> match model with
                    | SelectedNode n ->
                        let x,y = n.pos
                        {model with graph = GraphBuilder(model.graph).DuplicateNode({n with pos = (x+1,y+1)}).Build()} |> layout, Cmd.none
                    | _ -> model, Cmd.none
    | Delete -> match model with
                    | SelectedNode n -> {model with graph = GraphBuilder(model.graph).RemoveNode(n.guid).Build()} |> layout, Cmd.none
                    | SelectedEdge e -> {model with graph = GraphBuilder(model.graph).RemoveEdge(e.id).Build()} |> layout, Cmd.none
                    | _ -> model, Cmd.none
    | EndDragAndDrop -> { model with
                            edgeCandidate = None
                            deltaPos = None }, Cmd.none
    | ReadClipboard -> printfn "READ CLIP"; model, Cmd.OfAsync.result (App.View.readClipboard ())
    | LoadJson json ->
        printfn "LOAD JSON %s" json
        match App.Serialization.fromJson json with
        | Ok m -> m |> layout, Cmd.none
        | Error e -> eprintfn "Error: %A" e; model, Cmd.none
    | _ -> failwithf "Message not implemented: %A" msg

// App
Program.mkProgram init update view
|> Program.withSubscription (fun _ ->
    Cmd.ofSub (fun dispatch ->
        Browser.Dom.window.onkeydown <- (fun e ->
            App.GraphRender.keyToMessage e |> Option.iter (fun x -> e.preventDefault(); dispatch x)
        )
        Browser.Dom.window.onmouseup <- (fun _ -> dispatch EndDragAndDrop )
    ))
//|> Program.withReactSynchronous "elmish-app"
|> Program.withReactBatched "elmish-app"
#if DEBUG
//|> Program.withConsoleTrace
|> Program.withDebugger
#endif
|> Program.run