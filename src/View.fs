module App.View
open App.Types
open Elmish
open Fable.Core
open Fable.Core.JS
open Fable.Core.JsInterop
open Fable.Import
open Fable.React
open Fable.React.Props
open Fulma
open Fulma
open Fulma


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
let menuItem label onClick =
    Button.a [ Button.OnClick (fun _ -> onClick()) ] [str label]

let copyClipboard (content:string) =
  async {
    let! _ = Async.AwaitPromise <| requestPermission(Permission.ClipboardWrite)
    do! Async.AwaitPromise <| Browser.Navigator.navigator.clipboard.Value.writeText content
  } |> Async.StartImmediate
let copyJsonToClipboard model =
  let json = Serialization.toJson model
  let b64 = Serialization.toBase64String json
  JS.console.log("Base64", b64)
  copyClipboard(Serialization.toJson model)
let graphText model =
  let gr =
    Browser.Dom.document.querySelector("#graph-output").textContent
    |> Seq.chunkBySize model.options.ActualCanvasWidth
    |> Seq.map System.String
    |> String.concat "\n"
  sprintf "%s\n%s" Browser.Dom.window.location.href gr

let navbarView model dispatch =
   Navbar.navbar [Navbar.CustomClass "is-primary"] [
    Navbar.Brand.div [] [
      Navbar.Item.a [] [
        Icon.icon [] [
          i [Class "fa fa-project-diagram"] []
        ]
        span [] [str "Unicode Graphs"]
      ]
      Navbar.Item.div [] [
        Button.list [] [
          Button.button [
            Button.OnClick (fun _ -> dispatch (AddNode(Graph.GraphBuilder(model.graph).nextId(), "New")))
          ] [
            Icon.icon [] [
              i [Class "fa fa-plus"] []
            ]
            span [] [str "New Node"]
          ]
        ]
      ]
      Navbar.burger [ Navbar.Burger.Option.OnClick (fun _ -> dispatch ToggleBurger)] [
        span [] []
        span [] []
        span [] []
      ]
    ]
    Navbar.menu [ Navbar.Menu.IsActive model.isBurgerOpen ] [
      Navbar.Start.div [] []
      Navbar.End.div [] [
        Navbar.Item.div [] [
          Button.list [] [
            menuItem  "Copy Graph" (fun _ -> copyClipboard(graphText model))
            menuItem  "Copy Json to clipboard" (fun _ -> copyJsonToClipboard model)
            menuItem  "Load Json from clipboard" (fun _ -> dispatch Msg.ReadClipboard)
          ]
        ]
      ]
    ]
  ]

let view (model:Model) dispatch =
  div [] [
      navbarView model dispatch
      Section.section [] [
        Container.container [] [
          Columns.columns [] [
              Column.column [] (GraphRender.render dispatch model |> Seq.toList)
              Column.column [] [
                App.EditorView.view model dispatch
              ]
            ]
          ]
      ]
  ]
