module App.Graph

open System
open App.Geometry

[<Struct>]
type Port = {
    title: string
    guid: Guid
}

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