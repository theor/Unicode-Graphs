module App

open Elmish
open Elmish.React
open App.Graph
open App.Types
open App.GraphRender
open App.View

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
    newModel(gb.Build()) |> layout

// UPDATE

let update (msg:Msg) (model:Model) =
    match msg with
    | Layout -> layout model
    | Move(n, newPos) -> {model with graph = {model.graph with nodes = Map.change n (fun x -> { x.Value with pos = newPos } |> Some) model.graph.nodes}} |> layout
    | SelectNode(n,startPos) -> {model with selectedId = n; startPos = startPos}
    | AddNode(id, title) ->
        let gb = GraphBuilder(model.graph)
        ignore <| gb.AddNode(title=title, id=id)
        {model with graph = gb.Build() } |> layout
    | ChangeOptions options -> { model with options = options } |> layout
    | ChangeNode n ->
        { model with graph = GraphBuilder(model.graph).UpdateNode(n).Build() } |> layout
//        |> (fun m -> if m.selectedId() = Some(n.guid) then {m with selectedNode})
    | _ -> failwithf "Message not implemented: %A" msg

// App
Program.mkSimple init update view
//|> Program.withReactSynchronous "elmish-app"
|> Program.withReactBatched "elmish-app"
|> Program.withConsoleTrace
|> Program.run