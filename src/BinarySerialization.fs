module App.BinarySerialization

open System.Text
open Fable.Core

open Types
open Graph

type SerializationState = {
    view: JS.DataView
    offset: int
}

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

let w = state {
            do! putState 1
            let! x = getState
            return x > 0
        }
let ww = eval w 0
let ww2 = exec w 0

let write f size : State<unit,SerializationState> =
    let wrap view offset =
        if view <> null then
            f view offset

    state {
        let! s = getState
        do wrap s.view s.offset
        do! putState {s with offset = s.offset + size}
    }
let writeInt (i:int): State<unit,SerializationState> = write (fun v o -> v.setInt32(o, i)) 4
let writeUInt8 (i:uint8): State<unit,SerializationState> = write (fun v o -> v.setUint8(o, i)) 1
let writeBool (i:bool): State<unit,SerializationState> = write (fun v o -> v.setInt8(o, if i then 1y else 0y)) 1
let writeString (x:string): State<unit,SerializationState> =
    state {
        let b = Encoding.UTF8.GetBytes x
        do! writeUInt8 (uint8 b.Length)
        let l = b.Length-1
        for i in 0..l do
            do! writeUInt8 b.[i]
    }
//    write (fun v o -> v.setInt8(o, if i then 1y else 0y); 1) i
let read f : State<'a,SerializationState> =
    state {
        let! s = getState
        let size = 1
        do! putState {s with offset = s.offset + size}
        return (f s.view) s.offset
    }
let readUInt8: State<uint8,SerializationState> = read (fun v -> v.getUint8)
let readBool: State<bool,SerializationState> = read (fun v -> (fun o -> v.getUint8(o) = 1uy))
let readString: State<string,SerializationState> =
    state {
        let! len = readUInt8
        do JS.console.log("READ LEN", len)
        let mutable arr = Array.zeroCreate (int len)
//        while true do
        for i in 0..(int len)-1 do
            let! c = readUInt8 
            do JS.console.log("    READ BYTE", c)
            arr.[i] <- c
        do JS.console.log("    ARR", arr)
        return Encoding.UTF8.GetString(arr)
    }
//    read (fun v -> (fun o -> v.getUint8(o) = 1uy))
//    state {
//        let! s = getState
//        let size = 1
//        do! putState {s with offset = s.offset + size}
//        return s.view.getUint8(s.offset)
//    }
let writeState buffer w =
    let s:SerializationState = {offset=0; view=if buffer = null then null else JS.Constructors.DataView.Create(buffer)}
    let x, s = w s
    x
let test (m:SerializationModel) =

    let w = state {
        do! writeBool m.options.NodeBorders
        do! writeBool m.options.ShowPorts
        do! writeBool m.options.ShowIds
        do! writeString "asd"
        let! s = getState
        return s.offset
    }
    
    let len = writeState null w
    JS.console.log("BUFFER LEN", len)

    let b = JS.Constructors.ArrayBuffer.Create(len)
    let r = writeState b w
    JS.console.log("BUFFER",b, r)
    let x = state {
              let! i = readBool  
              let! b = readBool
              let! j = readBool
              let! s = readString
              return i,b,j,s
            } |> writeState b
    printf "READ %O" x