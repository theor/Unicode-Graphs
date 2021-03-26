module App.View

open App.Types
open Browser.Types
open Fable.Core
open Fable.Import
open Fable.React
open Fable.React.Props
open Fable.SimpleHttp
open Fulma
open Fulma.Extensions.Wikiki
open Thoth.Json


[<StringEnum; RequireQualifiedAccess>]
type PermissionStatusState =
    | Granted
    | Denied
    | Prompt

type PermissionStatus = {
    state: PermissionStatusState
}

[<StringEnum; RequireQualifiedAccess>]
type Permission =
    | [<CompiledName("clipboard-write")>] ClipboardWrite
    | [<CompiledName("clipboard-read")>] ClipboardRead

[<Emit("""
navigator.permissions.query({name: $0})
"""     )>]
let queryPermission (_perm: Permission): JS.Promise<PermissionStatus> = jsNative

// firefox doesn't support the permission, but the copy to clipboard will actually work
let queryPermissionWrap (perm: Permission) = queryPermission perm |> Promise.either U2.Case1 (fun _ -> U2.Case1 {state=PermissionStatusState.Denied})

let readClipboard (): Async<Msg> =
    async {
        let! _p =
            Async.AwaitPromise
            <| queryPermission (Permission.ClipboardRead)

        let! text =
            Async.AwaitPromise
            <| Browser.Navigator.navigator.clipboard.Value.readText ()

        return Msg.LoadJson(text, Format.Json)
    }

let copyClipboard (content: string) =
    async {
        let! _p =
            Async.AwaitPromise
            <| queryPermissionWrap (Permission.ClipboardWrite)

        do! Async.AwaitPromise
            <| Browser.Navigator.navigator.clipboard.Value.writeText content
    }
    |> Async.StartImmediate

let copyJsonToClipboard model =
    let json = Serialization.toJson model
    let b64 = BinarySerialization.toBase64String json
    JS.console.log ("Base64", b64)
    copyClipboard (Serialization.toJson model)

[<RequireQualifiedAccess>]
type GraphTextMode =
    | Raw
    | Comment
    | Markdown

let graphText (mode: GraphTextMode) model =
    let lines =
        Browser
            .Dom
            .document
            .querySelector("#graph-output")
            .textContent
        |> Seq.chunkBySize model.options.ActualCanvasWidth
        |> Seq.map System.String

    let url = Browser.Dom.window.location.href

    match mode with
    | GraphTextMode.Raw ->
        lines
        |> String.concat "\n"
        |> sprintf "%s\n%s" url
    | GraphTextMode.Comment ->
        lines
        |> String.concat "\n// "
        |> sprintf "// %s\n// %s" url
    | GraphTextMode.Markdown ->
        lines
        |> String.concat "\n"
        |> sprintf "```\n%s\n%s\n```" url

let menuItemColor label icon color tooltip disabled onClick =
    Button.a [ Button.Option.Color color
               Button.Disabled disabled
               Button.OnClick(fun _ -> onClick ())
               Button.Props [ Tooltip.dataTooltip tooltip ]
               Button.CustomClass(sprintf "%s %s" Tooltip.ClassName Tooltip.IsTooltipBottom) ] [
        yield Icon.icon [] [ i [ Class <| icon ] [] ]
        if not <| System.String.IsNullOrEmpty label then
            yield
                Fulma.Text.span [
                                //                  Modifiers [ Modifier.IsHidden(Screen.Desktop, true) ]
                                ] [
                    str label
                ]
    ]

let getShortUrl () =
    let token = "1b8205de688cac49c75d4d60e24bf3155b7043f3"
    async {
        let requestData = {|
                            long_url= sprintf "https://unicode-graphs.netlify.app/%s" Browser.Dom.window.location.hash
                            domain="bit.ly"
                          |} |> JS.JSON.stringify 
        let url= "https://api-ssl.bitly.com/v4/shorten"
        let! response =
            Http.request url
            |> Http.method HttpMethod.POST
            |> Http.content (BodyContent.Text requestData)
            |> Http.header (Headers.contentType "application/json")
            |> Http.header (Headers.authorization (sprintf "Bearer %s" token))
            |> Http.send
        do printfn "Content: %s" response.responseText
        if response.statusCode <> 200 && response.statusCode <> 201
        then return Result.Error <| sprintf "%i: %s" response.statusCode response.responseText
        else return response.responseText |> Decode.fromString (Decode.field "link" Decode.string)
//        do printfn "Status: %d" response.statusCode
    }

let menuItem label icon tooltip onClick =
    menuItemColor label icon Color.IsLink tooltip onClick

let navbarView model dispatch =
    Navbar.navbar [] [
        Navbar.Brand.div [] [
            Navbar.Item.a [] [
                Icon.icon [] [
                    i [ Class "fa fa-project-diagram" ] []
                ]
                Fulma.Text.span [ Modifiers [ Modifier.IsHidden (Screen.Touch, true) ] ] [ str "Unicode Graphs" ]
            ]
            Navbar.Item.div [] [
                Button.list [] [
                    menuItemColor "New Node" "fa fa-plus" Color.IsPrimary "Add a new node" false (fun _ ->
                        dispatch (AddNode(Graph.GraphBuilder(model.graph).nextId(), "New")))
                    menuItemColor "" "fa fa-undo" Color.IsLight "Undo" false (fun _ ->
                                Browser.Dom.history.back())
                    menuItemColor "" "fa fa-redo" Color.IsLight "Redo" false (fun _ ->
                                Browser.Dom.history.forward())
                ]
            ]

            Navbar.burger [ Navbar.Burger.Option.OnClick(fun _ -> dispatch ToggleBurger) ] [
                span [] []
                span [] []
                span [] []
            ]
        ]
        Navbar.menu [ Navbar.Menu.IsActive model.isBurgerOpen ] [
            Navbar.Start.div [] [
                Navbar.Item.div [] [
                    Button.list [] [
                        menuItemColor "Get Short URL" "fa fa-upload" Color.IsLight "Get a shortened bitly url" (model.GraphState = GraphState.PendingLayout) (fun _ ->
                            dispatch GetShortUrl)
                    ]
                ]
                        
                Navbar.Item.div [] [
                    Button.list [] [
                        menuItem "Text" "fa fa-copy" "Copy graph as text" false (fun _ ->
                            copyClipboard (graphText GraphTextMode.Raw model))
                        menuItem "Md" "fab fa-slack" "Copy graph as markdown" false (fun _ ->
                            copyClipboard (graphText GraphTextMode.Markdown model))
                        menuItem "Comment" "fa fa-code" "Copy graph as code comment" false (fun _ ->
                            copyClipboard (graphText GraphTextMode.Comment model))
                    ]
                ]
            ]
            Navbar.End.div [] [
                Navbar.Item.div [] [
                    Button.list [] [
                        menuItemColor "Copy Json" "fa fa-download" Color.IsLight "Copy Json to clipboard" false (fun _ ->
                            copyJsonToClipboard model)
                        menuItemColor "Load Json" "fa fa-upload" Color.IsLight "Load Json from clipboard" false (fun _ ->
                            dispatch Msg.ReadClipboard)
                    ]
                ]
            ]
        ]
    ]

let view (model: Model) dispatch =
    div [] [
        navbarView model dispatch
        Section.section [] [
            Container.container [] [
                Columns.columns [] [
                    Column.column [] [
                        yield! (GraphRender.render dispatch model |> Seq.toList)
                    ]
                    Column.column [] [
                        EditorView.view model dispatch
                        Message.message [] [
                            Message.header [] [ str "How to use" ]
                            Message.body [] [
                                Fulma.Content.content [] [
                                    Fulma.Text.div [] [
                                        str
                                            "Nodes can be moved on the canvas, which will grow if needed to accomodate all nodes. Use the "
                                        code [] [ str "New Node" ]
                                        str " button to create nodes and the "
                                        code [] [ str "Del" ]
                                        str " or "
                                        code [] [ str "Backspace" ]
                                        str " key to delete a node or an edge"
                                    ]
                                    Fulma.Text.div [] [
                                        str
                                            "Selecting a node allows to change its title and add or remove ports. An empty title will hide the node's title line."
                                    ]
                                    Fulma.Text.div [] [
                                        code [] [ str "Ctrl+D" ]
                                        str " to duplicate a node"
                                    ]
                                ]
                                Fulma.Content.content [] [
                                    Fulma.Text.div [] [
                                        str "To share a graph, you can just copy the url."
                                    ]
                                ]
                                Fulma.Content.content [] [
                                    Fulma.Text.div [] [
                                        str "The browser's history is used for undo/redo, which is also mapped to "
                                        code [] [ str "Ctrl+Z" ]
                                        str " and "
                                        code [] [ str "Ctrl+Y" ]
                                    ]
                                ]
                                Fulma.Content.content [] [
                                    Fulma.Text.div [] [
                                        str
                                            "The three \"Copy...\" buttons will copy the graph to the clipboard: \"as text\" is the raw graph as drawn here, \"as markdown\" surrounds it with triple backticks for slack, \"as code comment\" will add a "
                                        code [] [ str "// " ]
                                        str " prefix to each line"
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
        Footer.footer [] [
            Content.content [ Content.Option.Modifiers [ Modifier.IModifier.TextAlignment
                                                             (Screen.All, TextAlignment.Centered) ] ] [
                p [] [
                    strong [] [ str "Unicode Graphs" ]
                    str " by "
                    a [ Href "https://github.com/theor" ] [
                        str "theor"
                    ]
                ]
                p [] [
                    Icon.icon [] [
                        i [ Class <| "fab fa-github-square" ] []
                    ]
                    a [ Href "https://github.com/theor/Unicode-Graphs" ] [
                        str "Source Code"
                    ]
                ]
            ]
        ]
    ]
