module App.GraphLayout

open System
open Types
open Graph
open Geometry

let hasTitle n = not <| String.IsNullOrWhiteSpace n.title

let layout minWidth minHeight (model: Model) =
    let mutable nodeSizes = Map.empty<Id, Rect>

    let measureNode id n =
        let titleHeight = if hasTitle n then 1 else 0

        let portWidth =
            seq {
                for i in 0 .. Math.Max(n.inputs.Length, n.outputs.Length) - 1 do
                    match List.tryItem i n.inputs, List.tryItem i n.outputs with
                    | Some (a), Some (b) ->
                        a.title.Length
                        + b.title.Length
                        + 3 (* 2*port char + space between them*)
                        + 2
                    | Some (x), None
                    | None, Some (x) -> x.title.Length + 1 + 1 + 2
                    | _ -> failwith "Impossible"
            }
            //                |> Seq.map (fun x -> JS.console.log(x); x)
            |> (fun l -> if Seq.isEmpty l then Seq.replicate 1 0 else l)
            |> Seq.max

        let nw, nh =
            n.title.Length
            + 2
            + 2,
            2
            + titleHeight
            + Math.Max(n.inputs.Length, n.outputs.Length)

        let nw = Math.Max(nw, portWidth)
        let x, y = n.pos
        nodeSizes <- Map.add id (Rect.Create(x, y, nw, nh)) nodeSizes

    model.graph.nodes |> Map.iter measureNode

    let maxW =
        (nodeSizes
         |> Map.fold (fun max _ n -> Math.Max(max, (n.X + n.W))) 0)

    let maxH =
        (nodeSizes
         |> Map.fold (fun max _ n -> Math.Max(max, (n.Y + n.H))) 0)

    let w =
        Option.defaultValue maxW model.options.CanvasWidth

    let h =
        Option.defaultValue maxH model.options.CanvasHeight

    let makePortEntry (node: Node) (index: int) (p: Port) (dir: Direction) =
        let pos =
            let r = Map.find node.id nodeSizes

            let y =
                r.Y
                + (if hasTitle node then 1 else 0)
                + int index
                + 1

            let x =
                if dir = Direction.Input then r.X else (r.X + r.W)

            x, y

        { port = p
          ownerNode = node.id
          index = uint8 index
          position = pos
          direction = dir }

    let portData =
        model.graph.nodes
        |> Map.toSeq
        |> Seq.map (fun (_, n) ->
            (n,
             Seq.concat [ Seq.indexed n.inputs
                          |> Seq.map (fun p -> p, Direction.Input)
                          Seq.indexed n.outputs
                          |> Seq.map (fun p -> p, Direction.Output) ]))
        |> Seq.collect (fun (n, ports) ->
            ports
            |> Seq.map (fun ((i, p), dir) -> p.id, makePortEntry n i p dir))
        |> Map.ofSeq

    //    JS.console.log(nodeSizes |> Map.toSeq |> Seq.map (fun (_,x) -> x) |> Seq.toArray, nodeSizes |> Map.toArray |> Array.map (fun (a,b) -> b.Center))
    { model with
          options =
              { model.options with
                    ActualCanvasWidth = max w minWidth
                    ActualCanvasHeight = max h minHeight }
          nodeSizes = nodeSizes
          ports = portData }
