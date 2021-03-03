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

let toJson (m:Model): string =
    SerializationModel.fromModel m
    |> encoder
    |> Thoth.Json.Encode.toString 0

let fromJson(s:string): Result<Model,string> =
    Decode.fromString decoder s
    |> Result.map SerializationModel.toModel