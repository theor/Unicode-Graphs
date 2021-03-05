module App.BinarySerialization

open Fable.Core

open Types
open Graph


type SerializationModel = {
    options: RenderOptions
    nodes: Node array
    edges: Edge array
}
    with
        static member toModel(m:SerializationModel):Model =
            let g: Graph = {
                nodes= m.nodes |> Seq.map (fun n -> (n.guid,n)) |> Map.ofSeq
                edges= m.edges |> Seq.map (fun n -> (n.id,n)) |> Map.ofSeq
            }
            { newModel g with options=m.options }
        static member fromModel(m:Model):SerializationModel = {
            options = m.options
            nodes = m.graph.nodes |> Map.toSeq |> Seq.map snd |> Seq.toArray
            edges = m.graph.edges |> Map.toSeq |> Seq.map snd |> Seq.toArray
        }

//let w = state {
//            do! putS 1
//            let! x = getS
//            return x
//        }
//let ww = runS w 0

let write f size : S<State,unit> =
    let wrap view offset =
        if view <> null then
            f view offset

    state {
        let! s = getS
        do wrap s.view s.offset
        do! putS {s with offset = s.offset + size}
    }
let writeInt (i:int): S<State,unit> = write (fun v o -> v.setInt32(o, i)) 4
let writeUInt8 (i:uint8): S<State,unit> = write (fun v o -> v.setUint8(o, i)) 1
let writeBool (i:bool): S<State,unit> = write (fun v o -> v.setInt8(o, if i then 1y else 0y)) 1
let writeString (x:string): S<State,unit> =
    state {
        do! writeInt x.Length
    }
//    write (fun v o -> v.setInt8(o, if i then 1y else 0y); 1) i
let read f : S<State, 'a> =
    state {
        let! s = getS
        let size = 1
        do! putS {s with offset = s.offset + size}
        return (f s.view) s.offset
    }
let readUInt8(): S<State, uint8> = read (fun v -> v.getUint8)
let readBool(): S<State, bool> = read (fun v -> (fun o -> v.getUint8(o) = 1uy))
//    state {
//        let! s = getS
//        let size = 1
//        do! putS {s with offset = s.offset + size}
//        return s.view.getUint8(s.offset)
//    }

let writeState buffer w =
    let s:State = {offset=0; view=if buffer = null then null else JS.Constructors.DataView.Create(buffer)}
    let x, s = runS w s
    x
let test (m:SerializationModel) =

    let w = state {
        do! writeBool m.options.NodeBorders
        do! writeBool m.options.ShowPorts
        do! writeBool m.options.ShowIds
        let! s = getS
        return s.offset
    }
    
    let len = writeState null w
    JS.console.log("BUFFER LEN", len)

    let b = JS.Constructors.ArrayBuffer.Create(len)
    let r = writeState b w
    JS.console.log("BUFFER",b, r)
    let x = state {
              let! i = readBool()  
              let! b = readBool()
              let! j = readBool()  
              return i,b,j
            } |> writeState b
    printf "READ %O" x