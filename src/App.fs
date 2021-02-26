module App

open Elmish
open Elmish.React
open Elmish.Debug
open Elmish.Navigation
open App.Graph
open App.Types
open App.GraphLayout
open App.View

type Route = Home | Graph of string
let init(initRoute: Route option) : Model * Cmd<Msg> =
    let route = initRoute |> Option.defaultValue Route.Home
    printfn "Init Route: %A" route
    
    match route with
    | Route.Home ->
    
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
    | Route.Graph b64 -> newModel(emptyGraph()), Cmd.ofMsg (LoadJson(b64, Format.B64))

// UPDATE

let cmdLayout m =
    let m2 = layout m
    m2, Navigation.modifyUrl (sprintf "#/graph/%s" (App.Serialization.toBase64String <| App.Serialization.toJson m2))

let update (msg:Msg) (model:Model): Model * Cmd<Msg> =
    match msg with
    | Layout -> cmdLayout model
    | Move(n, newPos) -> {model with graph = {model.graph with nodes = Map.change n (fun x -> { x.Value with pos = newPos } |> Some) model.graph.nodes}} |> layout, Cmd.none
    | SelectNode(n,startPos) -> {model with selectedId = n; deltaPos = startPos}, Cmd.none
    | AddNode(id, title) ->
        let gb = GraphBuilder(model.graph)
        ignore <| gb.AddNode(title=title, id=id)
        {model with graph = gb.Build() } |> cmdLayout
    | ChangeOptions options -> { model with options = options } |> cmdLayout
    | ChangeEdge n ->
        { model with graph = GraphBuilder(model.graph).UpdateEdge(n).Build() } |> cmdLayout
    | ChangeNode n ->
        { model with graph = GraphBuilder(model.graph).UpdateNode(n).Build() } |> cmdLayout
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
        } |> cmdLayout
    | Duplicate -> match model with
                    | SelectedNode n ->
                        let x,y = n.pos
                        {model with graph = GraphBuilder(model.graph).DuplicateNode({n with pos = (x+1,y+1)}).Build()} |> cmdLayout
                    | _ -> model, Cmd.none
    | Delete -> match model with
                    | SelectedNode n -> {model with graph = GraphBuilder(model.graph).RemoveNode(n.guid).Build()} |> cmdLayout
                    | SelectedEdge e -> {model with graph = GraphBuilder(model.graph).RemoveEdge(e.id).Build()} |> cmdLayout
                    | _ -> model, Cmd.none
    | EndDragAndDrop -> { model with
                            edgeCandidate = None
                            deltaPos = None }, Cmd.none
    | ReadClipboard -> printfn "READ CLIP"; model, Cmd.OfAsync.result (App.View.readClipboard ())
    | LoadJson(data, format) ->
        let json = match format with | Format.Json -> data | Format.B64 -> App.Serialization.fromBase64String data
        printfn "LOAD JSON %s" json
        match App.Serialization.fromJson json with
        | Ok m -> m |> cmdLayout
        | Error e -> eprintfn "Error: %A" e; model, Cmd.none
    | _ -> failwithf "Message not implemented: %A" msg






open Elmish.UrlParser
let route = oneOf [
    map Route.Graph (s "graph" </> str)
    map Route.Home top
]

let urlUpdate (result: Route option) model =
    printfn "URL UPDATE %A" result
    (model, Cmd.none)// Navigation.modifyUrl "#")
// App
Program.mkProgram init update view
|> Program.withSubscription (fun _ ->
    Cmd.ofSub (fun dispatch ->
        Browser.Dom.window.onkeydown <- (fun e ->
            App.GraphRender.keyToMessage e |> Option.iter (fun x -> e.preventDefault(); dispatch x)
        )
        Browser.Dom.window.onmouseup <- (fun _ -> dispatch EndDragAndDrop )
    ))
|> Program.toNavigable (parseHash route) urlUpdate
//|> Program.withReactSynchronous "elmish-app"
|> Program.withReactBatched "elmish-app"
#if DEBUG
//|> Program.withConsoleTrace
|> Program.withDebugger
#endif
|> Program.run