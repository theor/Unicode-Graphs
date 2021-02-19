module App

open System
open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props

// Mutable variable to count the number of times we clicked the button
let mutable count = 0

[<Struct>]
type Port = {
    title: string
    guid: Guid
}

type Pos = int*int

[<Struct>]
type Node = {
    title: string
    guid: Guid
    pos: Pos
    inputs: Port List
    outputs: Port List
}

[<Struct>]
type Edge = {
    fromPort: Guid
    toPort: Guid
}

type Graph = {
    nodes: Node list
    edges: Edge list
}

let emptyGraph(): Graph = {
    nodes= []
    edges= []
}

let newPort (title: string) : Port =
    let guid = Guid.NewGuid() in
    {
        guid= guid
        title = guid.ToString()
    }
let newNode () : Node =
    let guid = Guid.NewGuid() in
    {
        guid = guid
        title = guid.ToString()
        pos = 0,0
        inputs = []
        outputs = []
    }
type GraphBuilder() =
    let mutable g: Graph = emptyGraph()
    member this.AddNode(?title: string, ?pos: Pos, ?inputs: string List, ?outputs: string List) =
       let guid = Guid.NewGuid() in
       let n = { guid = guid
                 title = defaultArg title (guid.ToString())
                 pos = defaultArg pos (0,0)
                 inputs = defaultArg inputs [] |> List.map newPort
                 outputs = defaultArg outputs [] |> List.map newPort }
       g <- { g with nodes =  n :: g.nodes }
       this
    member this.Build() = g

let gb = GraphBuilder().AddNode("A", (1,1)).AddNode("B", (20, 20))
let g: Graph = gb.Build()


let render (g:Graph) =
    let w = 100
    let h = 50
    let mutable b = Array.create (w*h) '_'
    let set x y c = b.[x + y*w] <- c
    
    let renderNode n =
        let nw,nh = 6,4
        let x,y = n.pos
        for i in 0..nw-1 do
        for j in 0..nh-1 do
            set (x+i)(y+j) (if i = 0 || i = (nw-1) || j = (nh - 1) || j = 0 then 'X' else '.')
        ()
    
    g.nodes |> Seq.iter renderNode
    seq {
        for line in Seq.chunkBySize w b do
            yield span [] [str <| String(line)]
        
    }
//    b |> Seq.chunkBySize w |> Seq.map (fun line -> String(line)) |> String.concat "\\A"

//// Get a reference to our button and cast the Element to an HTMLButtonElement
//let myButton = document.querySelector(".my-button") :?> Browser.Types.HTMLButtonElement
//
//// Register our listener
//myButton.onclick <- fun _ ->
//    count <- count + 1
//    myButton.innerText <- sprintf "You clicked: %i time(s)" count

type Model = int

type Msg =
| Increment
| Decrement

let init() : Model = 0

// UPDATE

let update (msg:Msg) (model:Model) =
    match msg with
    | Increment -> model + 1
    | Decrement -> model - 1

// VIEW (rendered with React)

let view (model:Model) dispatch =

  div []
      [ p [] [ str <| g.ToString() ]
        p [ClassName "graph-output"] (render g)// [  |> render |> str ]
        button [ OnClick (fun _ -> dispatch Increment) ] [ str "+" ]
        div [] [ str (string model) ]
        button [ OnClick (fun _ -> dispatch Decrement) ] [ str "-" ] ]

printfn "asd"
// App
Program.mkSimple init update view
|> Program.withReactSynchronous "elmish-app"
|> Program.withConsoleTrace
|> Program.run