module App

open System
open System.Drawing
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
    isNodeEdge: bool
}

type Graph = {
    nodes: Map<Guid,Node>
    edges: Edge list
}

let emptyGraph(): Graph = {
    nodes= Map.empty
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
    member this.AddNodeEdge(fromNode:Guid, toNode:Guid) =
        g <- {g with edges = {fromPort=fromNode; toPort=toNode; isNodeEdge = true} :: g.edges}
    member this.AddNode(?title: string, ?pos: Pos, ?inputs: string List, ?outputs: string List) =
       let guid = Guid.NewGuid() in
       let n = { guid = guid
                 title = defaultArg title (guid.ToString())
                 pos = defaultArg pos (0,0)
                 inputs = defaultArg inputs [] |> List.map newPort
                 outputs = defaultArg outputs [] |> List.map newPort }
       g <- { g with nodes = Map.add n.guid n g.nodes }
       n.guid
    member this.Build() = g

let gb = GraphBuilder()
let a = gb.AddNode("A Node", (1,1))
let b = gb.AddNode("B Node", (20,20))
gb.AddNodeEdge(a,b)
let g: Graph = gb.Build()

[<Struct>]
type Rect =
    { X:int
      Y:int
      W:int
      H:int }
    member this.Center = (this.X+this.W/2, this.Y+this.H/2)
    
    static member Create(x,y,w,h):Rect = {X=x;Y=y;W=w;H=h}
module Rect =
    let contains (x,y) (r:Rect) = x >= r.X && x < r.X+r.W && y >= r.Y && y < r.Y + r.H 

let render (g:Graph) =
    let w = 100
    let h = 50
    let margin = 1
    let mutable b = Array.create (w*h) '.'
    let set x y c = b.[x + y*w] <- c
    
    let mutable nodeSizes = Map.empty<Guid,Rect>
    
    let renderEdge (e:Edge) =
        if not e.isNodeEdge then failwith "NOT NODE EDGE"
        let rf = nodeSizes.Item e.fromPort
        let rt = nodeSizes.Item e.toPort
        let mutable (i,j) = rf.Center in
        let rtx,rty = rt.Center
        while i <> rtx || j <> rty do
            if not (Rect.contains (i,j) rf) && not (Rect.contains (i,j) rt)
            then set i j 'O'
            if i <> rtx
            then i <- i+1
            else j <- j+1
            
            ()
    let renderNode guid n =
        let nw,nh = n.title.Length + 2 + 2*margin,3
        let x,y = n.pos
        nodeSizes <- Map.add guid (Rect.Create(x,y,nw,nh)) nodeSizes 
        for i in 0..nw-1 do
        for j in 0..nh-1 do
            let c = match (i,j) with
                    | 0,0 -> '\u250c'
                    | (0, _) when j = nh - 1 -> '\u2514'
                    
                    | _,0 when i = nw-1 -> '\u2510'
                    | _,_ when i = nw-1 && j = nh-1 -> '\u2518'
                    | _,_ when j = 0 || j = nh-1 -> '\u2500'
                    | _,_ when i = 0 || i = nw-1 -> '\u2502'
                    | _ -> '.'
            set (x+i)(y+j) (if i = 0 || i = (nw-1) || j = (nh - 1) || j = 0 then c else ' ')
        for i in 0.. n.title.Length-1 do
            set (x+1+i+margin) (y+1) n.title.[i]
        ()
    
    g.nodes |> Map.iter renderNode
    g.edges |> List.iter renderEdge
    seq {
        for line in Seq.chunkBySize w b do
            yield pre [] [str <| String(line)]
        
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