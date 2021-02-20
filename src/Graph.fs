module App.Graph

open System
open App.Geometry

type Id = Id of uint
with
    static member Default = Id 0u
    member this.Value = match this with Id(i) -> i
// skip 1 as it's the reserved edge id
let EdgeId = Id 1u
let mutable next = 1u
let nextId() =
    next <- next + 1u
    Id next

[<Struct>]
type Port = {
    title: string
    guid: Id
}

[<Struct>]
type Node = {
    title: string
    guid: Id
    pos: Pos
    inputs: Port List
    outputs: Port List
}

[<Struct>]
type Edge = {
    fromPort: Id
    toPort: Id
    isNodeEdge: bool
}

type Graph = {
    nodes: Map<Id,Node>
    edges: Edge list
}

let emptyGraph(): Graph = {
    nodes= Map.empty
    edges= []
}

let newPort (title: string) : Port =
    let guid = nextId() in
    {
        guid= guid
        title = guid.ToString()
    }
let newNode () : Node =
    let guid = nextId() in
    {
        guid = guid
        title = guid.ToString()
        pos = 0,0
        inputs = []
        outputs = []
    }
type GraphBuilder(g0:Graph) =
    let mutable g: Graph = g0
    new() = GraphBuilder(emptyGraph()) 
    member this.AddNodeEdge(fromNode:Id, toNode:Id) =
        g <- {g with edges = {fromPort=fromNode; toPort=toNode; isNodeEdge = true} :: g.edges}
    member this.AddNode(?title: string, ?pos: Pos, ?inputs: string List, ?outputs: string List, ?id:Id) =
       let guid = Option.defaultWith nextId id in
       let n = { guid = guid
                 title = defaultArg title (guid.ToString())
                 pos = defaultArg pos (0,0)
                 inputs = defaultArg inputs [] |> List.map newPort
                 outputs = defaultArg outputs [] |> List.map newPort }
       g <- { g with nodes = Map.add n.guid n g.nodes }
       n.guid
    member this.Build() = g