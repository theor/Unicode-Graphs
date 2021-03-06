module App.Serialization

open Thoth.Json
open Types
open Graph
open BinarySerialization

let encodeId (id: Id) = Encode.uint32 id.Value

let decodeId (path: string) (value: obj) =
    Decode.uint32 path value |> Result.map Id.Id

let extras =
    Extra.empty |> Extra.withCustom encodeId decodeId

let encoder =
    Encode.Auto.generateEncoderCached<SerializationModel> (skipNullField = true, extra = extras)

let decoder =
    Decode.Auto.generateDecoderCached<SerializationModel> (extra = extras)

let toJson (m: Model): string =
    SerializationModel.fromModel m
    |> encoder
    |> Encode.toString 0

let fromJson (s: string): Result<Model, string> =
    Decode.fromString decoder s
    |> Result.map SerializationModel.toModel
