module App.Graph

open System
open App.Geometry

[<Struct;StructuralEquality;StructuralComparison>]
type Id =
    | Id of uint
    static member Default = Id 0u

    member this.Value =
        match this with
        | Id (i) -> i

    override this.ToString() = sprintf "#%u" this.Value

[<Struct>]
type Port = { title: string; id: Id }

[<Struct>]
type Node =
    { title: string
      id: Id
      pos: Pos
      inputs: Port List
      outputs: Port List }

[<Struct>]
type Edge =
    { id: Id
      fromNode: Id * uint8
      toNode: Id * uint8
      offset: int8 }

type Graph =
    { nodes: Map<Id, Node>
      edges: Map<Id, Edge> }

[<RequireQualifiedAccess>]
type Direction =
    | Input
    | Output

let emptyGraph (): Graph = { nodes = Map.empty; edges = Map.empty }

let getMaxId (g: Graph): uint =
    let maxOrDefault defaultValue s =
        if Seq.isEmpty s then defaultValue else s |> Seq.max

    let maxMap (m: Map<Id, _>) =
        m
        |> Map.toSeq
        |> Seq.map (fun (i, _) -> i.Value)
        |> maxOrDefault 0u

    let maxEdge = maxMap g.edges

    let maxNode =
        g.nodes
        |> Map.toSeq
        |> Seq.collect (fun (i, n) ->
            [ yield i.Value
              yield!
                  (Seq.concat [ n.inputs; n.outputs ]
                   |> Seq.map (fun p -> p.id.Value)) ])
        |> maxOrDefault 0u

    max maxEdge maxNode

type GraphBuilder(g0: Graph) =
    let mutable g: Graph = g0
    let mutable next: uint = getMaxId (g0)

    member this.nextId() =
        next <- next + 1u
        Id next

    new() = GraphBuilder(emptyGraph ())

    member this.newPort(title: string): Port =
        let id = this.nextId () in
        { id = id; title = title }

    member this.newNode(): Node =
        let id = this.nextId () in

        { id = id
          title = id.ToString()
          pos = 0, 0
          inputs = []
          outputs = [] }

    member this.AddEdge(fromNode: Id, fromIndex: uint8, toNode: Id, toIndex: uint8, ?id: Id) =
        let id = Option.defaultWith this.nextId id in

        g <-
            { g with
                  edges =
                      Map.add
                          id
                          { id = id
                            fromNode = (fromNode, fromIndex)
                            toNode = (toNode, toIndex)
                            offset = 0y }
                          g.edges }

        this

    member this.RemoveNode(id: Id) =
        g <- { g with nodes = Map.remove id g.nodes }
        this

    member this.RemoveEdge(id: Id) =
        g <- { g with edges = Map.remove id g.edges }
        this

    member this.DuplicateNode(node: Node, ?id: Id) =
        let id = Option.defaultWith this.nextId id in
        let duplicatePort (p: Port) = { p with id = this.nextId () }

        let duplicate =
            { node with
                  id = id
                  inputs = node.inputs |> List.map duplicatePort
                  outputs = node.outputs |> List.map duplicatePort }

        g <-
            { g with
                  nodes = Map.add id duplicate g.nodes }

        this

    member this.AddNode(?title: string, ?pos: Pos, ?inputs: string List, ?outputs: string List, ?id: Id) =
        let id = Option.defaultWith this.nextId id in

        let n =
            { id = id
              title = defaultArg title (id.ToString())
              pos = defaultArg pos (0, 0)
              inputs = defaultArg inputs [] |> List.map this.newPort
              outputs = defaultArg outputs [] |> List.map this.newPort }

        g <-
            { g with
                  nodes = Map.add n.id n g.nodes }

        n.id

    member this.UpdateNode(node: Node): GraphBuilder =
        g <-
            { g with
                  nodes = Map.change node.id (fun _ -> Some node) g.nodes }

        this

    member this.UpdateEdge(edge: Edge): GraphBuilder =
        g <-
            { g with
                  edges = Map.change edge.id (fun _ -> Some edge) g.edges }

        this

    member this.Build() = g
