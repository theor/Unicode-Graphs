module App.Serialization

open Fable.Core
open Thoth.Json
open Types
open Graph
open BinarySerialization

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