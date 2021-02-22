module App.EditorView
open Browser.Types
open Fable.React
open Fable.React.Props
open App.Types

let view (model:Model) dispatch =
    let dispatchChange (f:Event -> RenderOptions -> RenderOptions) (e:Event) =
        dispatch <| ChangeOptions (f e model.options)
    let dispatchNodeChange (f:Event -> Graph.Node) (e:Event) =
        dispatch <| ChangeNode (f e)
    let controlCheckbox (name:string) (value:bool) (setValue: bool -> RenderOptions) =
        div [Class "field"] [
            div [Class "control"] [
                label [Class "checkbox"] [
                    input [Type "checkbox"; Checked value; OnChange (dispatchChange (fun e o -> setValue (e.Checked)))]
                    str name
                ]
            ]
        ]
    let sectionLabel (name:string) controls =
        div [Class "field"] [
            label [Class "label"] [str name]
            yield! controls
        ]
    let control (name:string) controlInput =
        div [Class "field"] [
            label [Class "label"] [str name]
            div [Class "control"] [
                controlInput
            ]
        ]


    let selectedNodeView (n:Graph.Node) =
        [
            yield h2 [Class "title"] [str "Current Node"]
            yield control "Title" (input [Class "input"; Type "text"; Value n.title; OnChange (dispatchNodeChange (fun e -> {n with title = e.Value}))])

            yield sectionLabel "Inputs" [
                div [Class "control"] [ (input [Class "input"; Type "text"; Value n.title; OnChange (dispatchNodeChange (fun e -> {n with title = e.Value}))]) ]
                div [Class "control"] [ (input [Class "input"; Type "text"; Value n.title; OnChange (dispatchNodeChange (fun e -> {n with title = e.Value}))])
            ] ]
        ]

    div [] (seq {
        yield h2 [Class "title"] [str "Options"]
        yield controlCheckbox "Node Borders" model.options.NodeBorders (fun b -> {model.options with NodeBorders = b})// (input [Class "input"; Type "number"; Value model.options.Margin; OnChange (dispatchChange (fun e o -> {o with Margin = int e.Value}))])
        yield controlCheckbox "Show Ports" model.options.ShowPorts (fun b -> {model.options with ShowPorts = b})// (input [Class "input"; Type "number"; Value model.options.Margin; OnChange (dispatchChange (fun e o -> {o with Margin = int e.Value}))])
        yield control "Margin" (input [Class "input"; Type "number"; Value model.options.Margin; OnChange (dispatchChange (fun e o -> {o with Margin = int e.Value}))])
        if model.selectedNode.IsSome then yield! selectedNodeView model.selectedNode.Value
    })
