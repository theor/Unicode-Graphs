module App.View
open App.Types
open Elmish
open Fable.Core
open Fable.Core.JS
open Fable.Core.JsInterop
open Fable.Import
open Fable.React
open Fable.React.Props


[<StringEnum; RequireQualifiedAccess>]
type PermissionStatusState = Granted | Denied | Prompt
type PermissionStatus =
  abstract state: PermissionStatusState with get

[<StringEnum; RequireQualifiedAccess>]
type Permission =
  | [<CompiledName("clipboard-write")>] ClipboardWrite
  | [<CompiledName("clipboard-read")>] ClipboardRead

[<Emit("""
navigator.permissions.query({name: $0})
""")>]
let requestPermission(perm:Permission): Promise<PermissionStatus> = jsNative


let readClipboard (): Async<Msg> =
  async {
    let! p = Async.AwaitPromise <| requestPermission(Permission.ClipboardRead)
    let! text = Async.AwaitPromise <| Browser.Navigator.navigator.clipboard.Value.readText ()
    return Msg.LoadJson(text, Format.Json)
  }

let view (model:Model) dispatch =
  let doCopy () =

    let r = Browser.Dom.document.createRange()
//    r.selectNode text
    Browser.Dom.window.getSelection().removeAllRanges();
    Browser.Dom.window.getSelection().addRange(r)
    Browser.Dom.document.execCommand("Copy") |> ignore
    ()
  let copyClipboard (content:string) =
    async {
      let! p = Async.AwaitPromise <| requestPermission(Permission.ClipboardWrite)
      do! Async.AwaitPromise <| Browser.Navigator.navigator.clipboard.Value.writeText content
    } |> Async.StartImmediate
  let copyJsonToClipboard() =
    let json = Serialization.toJson model
    let b64 = Serialization.toBase64String json
    JS.console.log("Base64", b64)
    copyClipboard(Serialization.toJson model)
  let graphText() =
    let gr =
      Browser.Dom.document.querySelector("#graph-output").textContent
      |> Seq.chunkBySize model.options.ActualCanvasWidth
      |> Seq.map System.String
      |> String.concat "\n"
    sprintf "%s\n%s" Browser.Dom.window.location.href gr

  div [] [
      section [ Class "section" ] [
        div [ Class "container" ]
          (seq {
            yield p [Class "buttons"] [
              button [Class "button"; OnClick (fun _ -> dispatch (AddNode(Graph.GraphBuilder(model.graph).nextId(), "New")))] [str "New Node"]
              button [ClassName "button is-primary"; OnClick (fun _ -> copyClipboard(graphText()))] [str "Copy Graph"]
              button [ClassName "button is-primary"; OnClick (fun _ -> copyJsonToClipboard())] [str "Copy Json to clipboard"]
              button [ClassName "button is-primary"; OnClick (fun _ -> dispatch Msg.ReadClipboard)] [str "Load Json from clipboard"]
            ]
            yield div [Class "columns"] [
              div [Class "column"]  (seq {
                yield! GraphRender.render dispatch model
//                yield! GraphRender.render dispatch (GraphRender.layout {model with options = {model.options with NodeBorders = not model.options.NodeBorders} })
              })
              div [Class "column"] [
                App.EditorView.view model dispatch
              ]

            ]

          })
      ]

  ]
//  div [ ClassName "root" ]
//      [ div [HTMLAttr.Id "graph-output"; ClassName "graph-output"
//             OnMouseMove (onMouseMove dispatch model MouseState.Move)
//             OnMouseDown (onMouseMove dispatch model MouseState.Down)
//             OnMouseUp (onMouseMove dispatch model MouseState.Up) ]
//             (render model)
//        button [ OnClick (fun _ -> doCopy ()) ] [ str "Copy" ]]
//        button [ OnClick (fun _ -> dispatch Increment) ] [ str "+" ]
//        div [] [ str (string model) ]
//        button [ OnClick (fun _ -> dispatch Decrement) ] [ str "-" ] ]
