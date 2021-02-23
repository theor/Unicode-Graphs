module App

open Elmish
open Elmish.React
open Elmish.Debug
open App.Graph
open App.Types
open App.GraphRender
open App.View

let init() : Model =
    let gb = GraphBuilder()
    let a = gb.AddNode("A Node", (1,1), ["I1"], ["O1";"1234 1234 O2"])
    let b = gb.AddNode("B Node", (25,1), ["BI"; "BI2"], ["BO1"])
//    let c = gb.AddNode("C Node", (15,1))
//    let d = gb.AddNode("D Node", (25,5))
//    gb.AddEdge(a, 0u,b, 1u)
//    gb.AddEdge(a, 1u,b, 0u)
//    gb.AddNodeEdge(a,c)
//    gb.AddNodeEdge(c,b)
//    gb.AddNodeEdge(c,d)
    newModel(gb.Build()) |> layout |> (fun m -> {m with selectedId = Some b})

// UPDATE

let update (msg:Msg) (model:Model) =
    match msg with
    | Layout -> layout model
    | Move(n, newPos) -> {model with graph = {model.graph with nodes = Map.change n (fun x -> { x.Value with pos = newPos } |> Some) model.graph.nodes}} |> layout
    | SelectNode(n,startPos) -> {model with selectedId = n; deltaPos = startPos}
    | AddNode(id, title) ->
        let gb = GraphBuilder(model.graph)
        ignore <| gb.AddNode(title=title, id=id)
        {model with graph = gb.Build() } |> layout
    | ChangeOptions options -> { model with options = options } |> layout
    | ChangeNode n ->
        { model with graph = GraphBuilder(model.graph).UpdateNode(n).Build() } |> layout
//        |> (fun m -> if m.selectedId() = Some(n.guid) then {m with selectedNode})
    | EdgeCandidate pos -> { model with edgeCandidate = Some pos }
    | CreateEdge(fromId, toId) ->
        let {ownerNode=fromNode; index=fromIndex} = (Map.find fromId model.ports)
        let {ownerNode=toNode; index=toIndex} = (Map.find toId model.ports)
        let newGraph = if fromNode = toNode
                       then model.graph
                       else GraphBuilder(model.graph).AddEdge(fromNode, fromIndex, toNode, toIndex).Build()
        { model with
            graph = newGraph
            edgeCandidate = None
            deltaPos = None
        } |> layout
    | _ -> failwithf "Message not implemented: %A" msg

// App
Program.mkSimple init update view
//|> Program.withReactSynchronous "elmish-app"
|> Program.withReactBatched "elmish-app"
#if DEBUG
//|> Program.withConsoleTrace
|> Program.withDebugger
#endif
|> Program.run