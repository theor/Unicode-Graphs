module App

open System
open Elmish
open Elmish.React
open Fable.Core
open Fable.React
open Fable.React.Props
open App.Graph
open App.Geometry

// Mutable variable to count the number of times we clicked the button
let mutable count = 0



let gb = GraphBuilder()
let a = gb.AddNode("A Node", (1,1))
let b = gb.AddNode("B Node", (15,10))
let c = gb.AddNode("C Node", (15,1))
let d = gb.AddNode("D Node", (25,5))
gb.AddNodeEdge(a,b)
gb.AddNodeEdge(a,c)
gb.AddNodeEdge(c,b)
gb.AddNodeEdge(c,d)
let g: Graph = gb.Build()

let emptyChar = '.'

type RenderOptions = {
    CanvasWidth: int option
    CanvasHeight: int option
    Margin: int }
    with
        static member Default: RenderOptions = { CanvasWidth=None;CanvasHeight=None;Margin=1 }

let render (options:RenderOptions) (g:Graph) =
    
    let mutable nodeSizes = Map.empty<Guid,Rect>
    let measureNode guid n =
        let nw,nh = n.title.Length + 2 + 2*options.Margin,3
        let x,y = n.pos
        nodeSizes <- Map.add guid (Rect.Create(x,y,nw,nh)) nodeSizes
    g.nodes |> Map.iter measureNode
    let maxW = 2 + (nodeSizes |> Map.fold (fun max _ n -> Math.Max(max, (n.X + n.W))) 0)
    let maxH = 1 + (nodeSizes |> Map.fold (fun max _ n -> Math.Max(max, (n.Y + n.H))) 0)
    let w = Option.defaultValue maxW options.CanvasWidth
    let h = Option.defaultValue maxH options.CanvasHeight
    JS.console.log(Map.toArray nodeSizes)
    
    let mutable b = Array.create (w*h) emptyChar
    let set x y c = b.[x + y*w] <- c
    
    let renderNode guid n =
        let r = Map.find guid nodeSizes
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
            set (r.X+i)(r.Y+j) (if i = 0 || i = (r.W-1) || j = (r.H - 1) || j = 0 then c else ' ')
        for i in 0.. n.title.Length-1 do
            set (r.X+1+i+options.Margin) (r.Y+1) n.title.[i]
        ()
        
    
    let renderEdge (e:Edge) =
        if not e.isNodeEdge then failwith "NOT NODE EDGE"
        let rf = nodeSizes.Item e.fromPort
        let rt = nodeSizes.Item e.toPort
        let mutable (i,j) = rf.Center in
        let rtx,rty = rt.Center
        while i <> rtx || j <> rty do
            if not (Rect.contains (i,j) rf) && not (Rect.contains (i,j) rt)
            then set i j 'O'
            let dx,dy = (rtx-i),(rty-j)
            let sx,sy = Math.Sign dx, Math.Sign dy
            if Math.Abs(dx) > Math.Abs(dy)
            then
                i <- i+sx
            else
                j <- j+sy
            ()
    
//    let isEmptyChar c = c = emptyChar
    g.nodes |> Map.iter renderNode
    g.edges |> List.iter renderEdge
    seq {
        for line in Seq.chunkBySize w b do
//            let firstIsEmpty = isEmptyChar line.[0]
//            let mutable i = 0
//            while i < line.Length do
//                match line |> Seq.skip i |> Seq.tryFindIndex (fun c -> )
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
      [ div [ClassName "graph-output"; OnMouseMove (fun e -> JS.console.log(e.clientX, e.y, e))] (render RenderOptions.Default g) ]
//        button [ OnClick (fun _ -> dispatch Increment) ] [ str "+" ]
//        div [] [ str (string model) ]
//        button [ OnClick (fun _ -> dispatch Decrement) ] [ str "-" ] ]

printfn "asd"
// App
Program.mkSimple init update view
|> Program.withReactSynchronous "elmish-app"
|> Program.withConsoleTrace
|> Program.run