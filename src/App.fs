module App

open System
open Elmish
open Elmish.React
open Elmish.Debug
open Elmish.Navigation
open App.Graph
open App.Types
open App.GraphLayout
open App.View
open Thoth.Elmish
open Thoth.Json

Fable.Core.JsInterop.importAll "bulma-tooltip"
Fable.Core.JsInterop.importAll "@fortawesome/fontawesome-free/js/all"

let minSize = 30,10

type Route =
    | Home
    | Graph of string

let init (initRoute: Route option): Model * Cmd<Msg> =
    let route =
        initRoute |> Option.defaultValue Route.Home
    //    printfn "Init Route: %A" route

    match route with
    | Route.Home ->

        let gb = GraphBuilder()
        let a = gb.AddNode("", (1, 1), [], [ "O1" ])

        let b =
            gb.AddNode("B Node", (25, 1), [ "BI"; "BI2" ], [ "BO1" ])
        //    let c = gb.AddNode("C Node", (15,1))
        //    let d = gb.AddNode("D Node", (25,5))
        gb.AddEdge(a, 0uy, b, 1uy) |> ignore
        //    gb.AddEdge(a, 1u,b, 0u) |> ignore
        //    gb.AddNodeEdge(a,c) |> ignore
        //    gb.AddNodeEdge(c,b) |> ignore
        //    gb.AddNodeEdge(c,d) |> ignore
        newModel (gb.Build())
        |> layout (fst minSize) (snd minSize)
        |> (fun m -> { m with selectedId = Some b }),
        [] 
    | Route.Graph b64 -> newModel (emptyGraph ()), Cmd.ofMsg (LoadJson(b64, Format.B64))

// UPDATE

let cmdLayout m =
    let m2 = layout (fst minSize) (snd minSize) m

    let (debouncerModel, debouncerCmd) =
        m.Debouncer
        |> Debouncer.bounce (TimeSpan.FromSeconds 0.8) "user_input" UpdateUrl

    { m2 with GraphState = GraphState.PendingLayout; Debouncer = debouncerModel }, Cmd.batch [ Cmd.map DebouncerSelfMsg debouncerCmd ]


let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
//    Fable.Core.JS.console.log("update", string msg)
    //    App.Serialization.toBin model
    match msg with
    | Layout -> cmdLayout model
    | InstantiateTemplate templateName ->
        let gb = GraphBuilder(model.graph)
        let (_,_,tplBuilder) = App.View.Templates.templates |> List.find (fun (name,_,_) -> name = templateName)
        tplBuilder gb |> ignore
        { model with graph = gb.Build() } |> cmdLayout
    | UndoRedo isUndo ->
        if isUndo then Browser.Dom.history.back() else Browser.Dom.history.forward()
        model, Cmd.none
    | Move (n, newPos) ->
        { model with
              graph =
                  { model.graph with
                        nodes = Map.change n (fun x -> { x.Value with pos = newPos } |> Some) model.graph.nodes } }
        |> cmdLayout
    | SelectNode (n, startPos) ->
        { model with
              selectedId = n
              deltaPos = startPos },
        Cmd.none
    | AddNode (id, title) ->
        let gb = GraphBuilder(model.graph)
        ignore <| gb.AddNode(title = title, id = id)
        { model with graph = gb.Build() } |> cmdLayout
    | ChangeOptions options -> { model with options = options } |> cmdLayout
    | ChangeEdge n ->
        { model with
              graph = GraphBuilder(model.graph).UpdateEdge(n).Build() }
        |> cmdLayout
    | ChangeNode n ->
        { model with
              graph = GraphBuilder(model.graph).UpdateNode(n).Build() }
        |> cmdLayout
    //        |> (fun m -> if m.selectedId() = Some(n.id) then {m with selectedNode})
    | EdgeCandidate (pos, highlightedId) ->
        { model with
              edgeCandidate = Some pos
              highlightedId = highlightedId },
        Cmd.none
    | CreateEdge (fromId, toId) ->
        let { ownerNode = fromNode
              index = fromIndex
              direction = fromDir } =
            (Map.find fromId model.ports)

        let { ownerNode = toNode
              index = toIndex
              direction = toDir } =
            (Map.find toId model.ports)

        let newGraph =
            if fromNode = toNode || fromDir = toDir then
                model.graph
            else
                GraphBuilder(model.graph)
                    .AddEdge(fromNode, fromIndex, toNode, toIndex)
                    .Build()

        { model with
              graph = newGraph
              edgeCandidate = None
              deltaPos = None }
        |> cmdLayout
    | CreateAndConnectNode(portId, pos) ->
        let { ownerNode = fromNode
              index = fromIndex
              direction = fromDir } =
            (Map.find portId model.ports)
        let b = GraphBuilder(model.graph) 
        let newNode = b.AddNode("", pos, (if fromDir = Direction.Input then [] else ["O"]), if fromDir = Direction.Input then ["I"] else [])
        let newGraph = (if fromDir <> Direction.Input then b.AddEdge(fromNode, fromIndex, newNode, 0uy) else b.AddEdge(newNode, 0uy, fromNode, fromIndex))
                        .Build()

        { model with
              graph = newGraph
              selectedId = Some newNode
              edgeCandidate = None
              deltaPos = None }
        |> cmdLayout
    | Duplicate ->
        match model with
        | SelectedNode n ->
            let x, y = n.pos

            { model with
                  graph =
                      GraphBuilder(model.graph)
                          .DuplicateNode({ n with pos = (x + 1, y + 1) })
                          .Build() }
            |> cmdLayout
        | _ -> model, Cmd.none
    | AddInput ->
        match model with
        | SelectedNode n -> {model with graph = GraphBuilder(model.graph).UpdateNode({n with inputs = n.inputs @ [GraphBuilder(model.graph).newPort "new"]}).Build()} |> cmdLayout
        | _ -> model, Cmd.none
    | AddOutput ->
        match model with
        | SelectedNode n -> {model with graph = GraphBuilder(model.graph).UpdateNode({n with outputs = n.outputs @ [GraphBuilder(model.graph).newPort "new"]}).Build()} |> cmdLayout
        | _ -> model, Cmd.none
    | Delete ->
        match model with
        | SelectedNode n ->
            { model with
                  graph =
                      GraphBuilder(model.graph)
                          .RemoveNode(n.id)
                          .Build() }
            |> cmdLayout
        | SelectedEdge e ->
            { model with
                  graph = GraphBuilder(model.graph).RemoveEdge(e.id).Build() }
            |> cmdLayout
        | _ -> model, Cmd.none
    | EndDragAndDrop ->
        { model with
              edgeCandidate = None
              deltaPos = None },
        Cmd.none
    | Highlight id -> { model with highlightedId = id }, Cmd.none
    | ReadClipboard ->
        printfn "READ CLIP"
        model, Cmd.OfAsync.result (readClipboard ())
    | LoadJson (data, format) ->
        let res =
            match format with
            | Format.Json -> App.Serialization.fromJson data
            | Format.B64 -> App.BinarySerialization.fromBase64 data
        match res with
                | Ok m -> cmdLayout m
                | Error e ->
                    eprintfn "Error: %A" e
                    init None

    | ToggleBurger ->
        { model with
              isBurgerOpen = not model.isBurgerOpen },
        Cmd.none
    | DebouncerSelfMsg debouncerMsg ->
        let (debouncerModel, debouncerCmd) =
            Debouncer.update debouncerMsg model.Debouncer

        { model with
              Debouncer = debouncerModel },
        debouncerCmd
    | UpdateUrl ->
        let url = sprintf "#/graph/%s" (App.BinarySerialization.toBase64 model) in
        let alreadyThere = Browser.Dom.window.location.hash = url in
//        printf "ALREADY THERE: %O"
        if not alreadyThere then 
            Browser.Dom.history.pushState(null, url=url)
        { model with GraphState = GraphState.Ready }, Cmd.none// Navigation.modifyUrl url
    | GotShortUrl url ->
        model, match url with
               | Result.Error e -> sprintf "Error getting the short url: %s" e
                                   |> Toast.message |> Toast.noTimeout |> Toast.withCloseButton
                                   |> Toast.error
               | Result.Ok url ->
                   App.View.copyClipboard url
                   sprintf "Short link copied to clipboard: %s" url |> Toast.message |> Toast.info

    | GetShortUrl ->
        let a = async {
            let! x = App.View.getShortUrl ()
            return GotShortUrl x
            }
        in
        model, Cmd.OfAsync.result a |> Cmd. map (fun x -> x)

//    | _ -> failwithf "Message not implemented: %A" msg


open Elmish.UrlParser

let route: State<(Route -> Route)> -> State<Route> list =
    let star: Parser<(string -> Route), Route> = (fun s ->
        [{visited = List.concat [s.visited; s.unvisited]; unvisited = []; args = s.args; value = s.value (String.Join("/", s.unvisited))}])
        
    
    oneOf [ map Route.Graph (s "graph" </> star)
            map Route.Home top ]

let urlUpdate (result: Route option) model =
//    printfn "URL UPDATE %A %A" result model
    match result with
    | Some (Route.Graph b64) -> newModel (emptyGraph ()), Cmd.ofMsg (LoadJson(b64, Format.B64))
    | Some Home -> (newModel (emptyGraph ()), Cmd.none)
    | None -> (model, Cmd.none)

Program.mkProgram init update view
|> Program.withSubscription (fun _ ->
    Cmd.ofSub (fun dispatch ->
        Browser.Dom.window.onkeydown <-
            (fun e ->
                App.GraphRender.keyToMessage e
                |> Option.iter (fun x ->
                    e.preventDefault ()
                    dispatch x))

        Browser.Dom.window.onmouseup <- (fun _ -> dispatch EndDragAndDrop)))
|> Program.toNavigable (parseHash route) urlUpdate
// |> Toast.Program.withToast Toast.render
//|> Program.withReactSynchronous "elmish-app"
|> Program.withReactBatched "elmish-app"
#if DEBUG
//|> Program.withConsoleTrace
// |> Program.withDebugger
#endif
|> Program.run
