module App.BinarySerialization

open System
open System.Text
open Fable.Core

open Types
open Graph

type IBase64Arraybuffer =
    abstract member encode: JS.ArrayBuffer-> string
    abstract member decode: string -> JS.ArrayBuffer
type ILz4js =
    abstract member compress: JS.TypedArray-> JS.TypedArray
    abstract member decompress: JS.TypedArray-> JS.TypedArray
let base64Arraybuffer: IBase64Arraybuffer = JsInterop.importAll "base64-arraybuffer";
let lz4js: ILz4js = JsInterop.importAll "lz4js";

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

//let w = state {
//            do! putState 1
//            let! x = getState
//            return x > 0
//        }
//let ww = eval w 0
//let ww2 = exec w 0

let write f size : State<unit,SerializationState> =
    let wrap view offset =
        if view <> null then
            f view offset

    state {
        let! s = getState
        do wrap s.view s.offset
        do! putState {s with offset = s.offset + size}
    }
let read f size : State<'a,SerializationState> =
    state {
        let! s = getState
        do! putState {s with offset = s.offset + size}
        return (f s.view) s.offset
    }
let writeInt (i:int): State<unit,SerializationState> = write (fun v o -> v.setInt32(o, i)) 4
let readInt: State<int,SerializationState> = read (fun v -> v.getInt32) 4

let writeUInt (i:uint): State<unit,SerializationState> = write (fun v o -> v.setUint32(o, i)) 4
let readUInt: State<uint,SerializationState> = read (fun v -> v.getUint32) 4

let writeUInt8 (i:uint8): State<unit,SerializationState> = write (fun v o -> v.setUint8(o, i)) 1
let readUInt8: State<uint8,SerializationState> = read (fun v -> v.getUint8) 1

let writeInt8 (i:int8): State<unit,SerializationState> = write (fun v o -> v.setInt8(o, i)) 1
let readInt8: State<int8,SerializationState> = read (fun v -> v.getInt8) 1

let writeBool (i:bool): State<unit,SerializationState> = write (fun v o -> v.setInt8(o, if i then 1y else 0y)) 1
let readBool: State<bool,SerializationState> = read (fun v -> (fun o -> v.getUint8(o) = 1uy)) 1

let writeSeq (x:seq<_>) f =
//    JS.console.log  ("write seq", x)
    state {
        let l = Seq.length x
        do! writeUInt8 (uint8 l)
        for i in x do
            do! f i
    }
let readSeq f =
    state {
        let! len = readUInt8
//        do JS.console.log("READ LEN", len)
        let mutable arr = Array.zeroCreate (int len)
        for i in 0..(int len)-1 do
            let! elt = f
            arr.[i] <- elt
        return Seq.ofArray arr
    }

let writeString (x:string): State<unit,SerializationState> = writeSeq (Encoding.UTF8.GetBytes x) writeUInt8
let readString: State<string,SerializationState> = Seq.toArray >> Encoding.UTF8.GetString <!> readSeq readUInt8

let writeId (x:Id): State<unit,SerializationState> = writeUInt x.Value
let readId: State<Id,SerializationState> = Id.Id <!> readUInt

let writePort (x:Port): State<unit,SerializationState> =
    state {
        do! writeId x.guid
        do! writeString x.title
    }
let readPort: State<Port, SerializationState> =
    state {
        let! id = readId
        let! title = readString
        return {
            guid = id
            title = title
        }
    }

let writeNode (x:Node): State<unit,SerializationState> =
    state {
        do! writeId x.guid
        do! writeString x.title
        let px,py = x.pos
        do! writeInt px
        do! writeInt py
        do! writeSeq x.inputs writePort
        do! writeSeq x.outputs writePort
    }
let readNode: State<Node, SerializationState> =
    state {
        let! id = readId
        let! title = readString
        let! px = readInt
        let! py = readInt
        let! inputs = readSeq readPort
        let! outputs = readSeq readPort
        return {
            guid = id
            title = title //id.ToString()// title
            pos = px,py
            inputs = inputs |> Seq.toList
            outputs = outputs |> Seq.toList
        }
    }

let writeEdge (x:Edge): State<unit,SerializationState> =
    state {
        do! writeId x.id
        do! writeInt8 x.offset
        do! writeBool x.isNodeEdge
        let fid, fidx = x.fromNode
        let tid, tidx = x.toNode
        do! writeId fid
        do! writeUInt8 fidx
        do! writeId tid
        do! writeUInt8 tidx
    }
let readEdge: State<Edge, SerializationState> =
    state {
        let! id = readId
        let! offset = readInt8
        let! isNodeEdge =readBool
        let! fid = readId
        let! fidx = readUInt8
        let! tid = readId
        let! tidx = readUInt8
        return {
            id = id
            offset = offset
            isNodeEdge= isNodeEdge
            fromNode = fid,fidx
            toNode = tid,tidx
        }
    }


let writeState buffer w =
    let s:SerializationState = {offset=0; view=if buffer = null then null else JS.Constructors.DataView.Create(buffer)}
    let x, s = w s
    x

let serialize (m:SerializationModel) =
    state {
        do! writeBool m.options.NodeBorders
        do! writeBool m.options.ShowPorts
        do! writeBool m.options.ShowIds
        do! writeSeq m.nodes writeNode
        do! writeSeq m.edges writeEdge
        let! s = getState
        return s.offset
    }
let deserialize =
    state {
      let! nodeBorders = readBool
      let! showPorts = readBool
      let! showIds = readBool
      let! nodes = readSeq readNode
      let! edges = readSeq readEdge
      let m:SerializationModel = {
          options = { RenderOptions.Default with
                        NodeBorders = nodeBorders
                        ShowPorts = showPorts
                        ShowIds = showIds }
          nodes = nodes |> Seq.toArray
          edges = edges |> Seq.toArray
      }
      return m
    }

[<Emit("btoa($0)")>]
let toBase64String x : string = failwith "JS"

[<Emit("atob($0)")>]
let fromBase64String (x : string):'a = failwith "JS"

let toBase64 (m:Model) =
    let sm = SerializationModel.fromModel m
    let len = serialize sm |> writeState null
    let b = JS.Constructors.ArrayBuffer.Create(len)
    let r =  serialize sm |> writeState b
    let b2 = lz4js.compress <| JS.Constructors.Uint8Array.Create(b, 0, b.byteLength)
    if len <> r then failwithf "non matched lengths: %i %i" len r
//    let res = base64Arraybuffer.encode(b)
    let res2 = base64Arraybuffer.encode(b2.buffer)
//    let u8 = JS.Constructors.Uint8Array.Create(b)
//    let res = toBase64String <| u8.join("")
//    JS.console.log("SER", res, res.Length, res2, res2.Length, b, b2)
    res2
let fromBase64 (s:string) =
    let decoded = base64Arraybuffer.decode s
    let decompressed = lz4js.decompress <| JS.Constructors.Uint8Array.Create(decoded, 0, decoded.byteLength)
//    let mutable decoded = JS.Constructors.Uint8Array.Create(a.Length)
//    JS.console.log("DESER", s, decoded)
//    for i in 0..a.Length-1 do
//        decoded.[i] <- Byte.Parse <| string a.[i]
//    JS.console.log decoded

    deserialize |> writeState decompressed.buffer |> SerializationModel.toModel
