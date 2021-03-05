module App.Serialization

open Fable.Core
open Thoth.Json
open Types
open Graph

[<Emit("btoa($0)")>]
let toBase64String (s:string) : string = failwith "JS"

[<Emit("atob($0)")>]
let fromBase64String (s:string) : string = failwith "JS"

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

let encodeId (id:Id) = Encode.uint32 id.Value
let decodeId (path:string) (value:obj) = Decode.uint32 path value |> Result.map Id.Id
let extras = Extra.empty
             |> Extra.withCustom encodeId decodeId
let encoder = Thoth.Json.Encode.Auto.generateEncoderCached<SerializationModel>(skipNullField=true, extra=extras)
let decoder = Thoth.Json.Decode.Auto.generateDecoderCached<SerializationModel>(extra=extras)

type State = {
    view: JS.DataView
    offset: int
}
type S<'State,'Value> =
    S of ('State -> 'Value * 'State)
let returnM x =
    let run state = x,state
    S run
let runS (S f) state = f state
let bindM f xS =
    let run state =
        let x,newState = runS xS state
        runS (f x) newState
    S run
type StateBuilder()=
    member this.Return(x) =
        JS.console.log("return", this, x)
        returnM x
    member this.ReturnFrom(xs) =
        JS.console.log("return from", this, xs)
        xs
    member this.Bind(xM,f) =
        JS.console.log("bind", this, xM, f)
        bindM f xM
let state = StateBuilder()

let getS =
    let run state =
        // return the current state in the first element of the tuple
        state, state
    S run
// val getS : S<State>

let putS newState =
    let run _ =
        // return nothing in the first element of the tuple
        // return the newState in the second element of the tuple
        (), newState
    S run

//let w = state {
//            do! putS 1
//            let! x = getS
//            return x
//        }
//let ww = runS w 0

let write f (x:'a): S<State,unit> =
    state {
        let! s = getS
        let newOffset = f s.view s.offset
        do! putS {s with offset = s.offset + newOffset}

    }
let writeInt2 (i:int): S<State,unit> = write (fun v o -> v.setInt32(o, i); 4) i
let writeBool (i:bool): S<State,unit> = write (fun v o -> v.setInt8(o, if i then 1y else 0y); 1) i
let writeInt (i:int): S<State,unit> =
    state {
        let! s = getS
        do s.view.setInt32(s.offset, i)
        do JS.console.log("write", i, s.offset)
        do! putS {s with offset = s.offset + 4}
    }

let w: S<State, unit> = state {
    do! writeInt2 42
    do! writeBool true
    do! writeInt 44
    return ()
}
let runState buffer w =
    let s:State = {offset=0; view=JS.Constructors.DataView.Create(buffer)}
    let _, s = runS w s
    s
let b = JS.Constructors.ArrayBuffer.Create(10)
runState b w
JS.console.log(b)
let toJson (m:Model): string =
    SerializationModel.fromModel m
    |> encoder
    |> Thoth.Json.Encode.toString 0

let fromJson(s:string): Result<Model,string> =
    Decode.fromString decoder s
    |> Result.map SerializationModel.toModel