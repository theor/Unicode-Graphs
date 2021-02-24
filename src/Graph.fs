module App.Graph

open System
open App.Geometry

type Id = Id of uint
with
    static member Default = Id 0u
    member this.Value = match this with Id(i) -> i
// skip 1 as it's the reserved edge id
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
    id: Id
    fromNode: Id * uint
    toNode: Id * uint
    isNodeEdge: bool
    offset: int
}

type Graph = {
    nodes: Map<Id,Node>
    edges: Map<Id, Edge>
}

[<RequireQualifiedAccess>]
type Direction = Input | Output

let emptyGraph(): Graph = {
    nodes= Map.empty
    edges= Map.empty
}

let newPort (title: string) : Port =
    let guid = nextId() in
    {
        guid= guid
        title = title
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
    member this.AddNodeEdge(fromNode:Id, toNode:Id, ?id:Id) =
       let guid = Option.defaultWith nextId id in
       g <- {g with edges = Map.add guid {id=guid; fromNode=(fromNode,UInt32.MaxValue); toNode=(toNode,UInt32.MaxValue); isNodeEdge = true; offset=0} g.edges}
    member this.AddEdge(fromNode:Id, fromIndex:uint, toNode:Id, toIndex:uint, ?id:Id) =
       let guid = Option.defaultWith nextId id in
       g <- {g with edges = Map.add guid {id=guid; fromNode=(fromNode,fromIndex); toNode=(toNode,toIndex); isNodeEdge = true; offset=0} g.edges}
    member this.AddNode(?title: string, ?pos: Pos, ?inputs: string List, ?outputs: string List, ?id:Id) =
       let guid = Option.defaultWith nextId id in
       let n = { guid = guid
                 title = defaultArg title (guid.ToString())
                 pos = defaultArg pos (0,0)
                 inputs = defaultArg inputs [] |> List.map newPort
                 outputs = defaultArg outputs [] |> List.map newPort }
       g <- { g with nodes = Map.add n.guid n g.nodes }
       n.guid
    member this.UpdateNode(node:Node): GraphBuilder =
        g <- {g with nodes = Map.change node.guid (fun _ -> Some node) g.nodes}
        this
    member this.UpdateEdge(edge:Edge): GraphBuilder =
        g <- {g with edges = Map.change edge.id (fun _ -> Some edge) g.edges}
        this
    member this.Build() = g