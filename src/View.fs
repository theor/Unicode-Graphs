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
open Fulma
open Fulma.Extensions.Wikiki


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

[<RequireQualifiedAccess>]
type GraphTextMode = Raw | Comment | Markdown
let graphText (mode:GraphTextMode) model =
  let lines =
    Browser.Dom.document.querySelector("#graph-output").textContent
    |> Seq.chunkBySize model.options.ActualCanvasWidth
    |> Seq.map System.String
  let url = Browser.Dom.window.location.href
  match mode with
  | GraphTextMode.Raw -> lines |> String.concat "\n" |> sprintf "%s\n%s" url
  | GraphTextMode.Comment -> lines |> String.concat "\n// " |> sprintf "// %s\n// %s" url
  | GraphTextMode.Markdown -> lines |> String.concat "\n" |> sprintf "```\n%s\n%s\n```" url

let menuItemColor label icon color tooltip onClick =
    Button.a [ Button.Option.Color color
               Button.OnClick (fun _ -> onClick())
               Button.Props [ Tooltip.dataTooltip tooltip ]
               Button.CustomClass (sprintf "%s %s" Tooltip.ClassName Tooltip.IsTooltipBottom)
    ] [
      yield Icon.icon [] [
          i [Class <| icon] []
      ]
      if not <| System.String.IsNullOrEmpty label then yield span [] [str label]
    ]

let menuItem label icon tooltip onClick = menuItemColor label icon Color.IsLink tooltip onClick

let navbarView model dispatch =
   Navbar.navbar [] [
    Navbar.Brand.div [] [
      Navbar.Item.a [] [
        Icon.icon [] [
          i [Class "fa fa-project-diagram"] []
        ]
        span [] [str "Unicode Graphs"]
      ]
      Navbar.Item.div [] [
        Button.list [] [
          menuItemColor "New Node" "fa fa-plus" Color.IsPrimary "Add a new node" (fun _ -> dispatch (AddNode(Graph.GraphBuilder(model.graph).nextId(), "New")))
          menuItem "" "fa fa-copy" "Copy graph as text" (fun _ -> copyClipboard(graphText GraphTextMode.Raw model))
          menuItem "" "fab fa-slack" "Copy graph as markdown" (fun _ -> copyClipboard(graphText GraphTextMode.Markdown model))
          menuItem "" "fa fa-code" "Copy graph as code comment" (fun _ -> copyClipboard(graphText GraphTextMode.Comment model))
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
            menuItem "" "fa fa-download" "Copy Json to clipboard" (fun _ -> copyJsonToClipboard model)
            menuItem "" "fa fa-upload" "Load Json from clipboard" (fun _ -> dispatch Msg.ReadClipboard)
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
              Column.column [] [
                yield! (GraphRender.render dispatch model |> Seq.toList)
              ]
              Column.column [] [
                App.EditorView.view model dispatch
                Fulma.Message.message [] [
                  Message.header [] [str "How to use"]
                  Message.body [] [
                    Fulma.Content.content [] [
                      Fulma.Text.div [] [
                        str "Nodes can be moved on the canvas, which will grow if needed to accomodate all nodes. Use the "
                        code [] [str "New Node"]
                        str " button to create nodes and the "
                        code [] [str"Del"]
                        str " key to delete a node or an edge"
                      ]
                      Fulma.Text.div [] [str "Selecting a node allows to change its title and add or remove ports. An empty title will hide the node's title line." ]
                      Fulma.Text.div [] [
                        code [] [str "Ctrl+D"]
                        str " to duplicate a node"
                      ]
                    ]
                    Fulma.Content.content [] [
                      Fulma.Text.div [] [str "To share a graph, you can just copy the url." ]
                    ]
                    Fulma.Content.content [] [
                      Fulma.Text.div [] [
                        str "The three \"Copy...\" buttons will copy the graph to the clipboard: \"as text\" is the raw graph as drawn here, \"as markdown\" surrounds it with triple backticks for slack, \"as code comment\" will add a "
                        code [] [str "// "]
                        str " prefix to each line"
                      ]
                    ]
                  ]
                ]
              ]
            ]
          ]
      ]
  ]
