module App.Serialization

open Fable.Core
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

let toJson (m:Model): string =
    Thoth.Json.Encode.Auto.toString(0, SerializationModel.fromModel m)

let fromJson(s:string): Result<Model,string> =
    Thoth.Json.Decode.Auto.fromString(s)
    |> Result.map SerializationModel.toModel