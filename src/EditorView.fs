module App.EditorView
open App.Graph
open Browser.Types
open Fable.React
open Fable.React.Props
open App.Types

let view (model:Model) dispatch =
    let dispatchChange (f:Event -> RenderOptions -> RenderOptions) (e:Event) =
        dispatch <| ChangeOptions (f e model.options)
    let dispatchNodeChange (f:Event -> Graph.Node) (e:Event) =
        dispatch <| ChangeNode (f e)
    let dispatchEdgeChange (f:Event -> Graph.Edge) (e:Event) =
        dispatch <| ChangeEdge (f e)
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
    let sectionLabelHorizontal (name:string) controls =
        div [Class "field is-horizontal"] [
                div [Class "field-label is-normal"] [
                    label [Class "label"] [str name]
                ]
                div [Class "field-body"] [
                    div [Class "field has-addons  has-addons-right"] [
                        yield! controls
                    ]

                ]
            ]
    let control (name:string) controlInput =
        div [Class "field"] [
            label [Class "label"] [str name]
            div [Class "control"] [
                controlInput
            ]
        ]


    let replace l idx x = List.mapi (fun i elt -> if i <> idx then elt else x) l
    let selectedPortView (n:Graph.Port) = [ str <| sprintf "%A" n ]
    let selectedEdgeView (n:Graph.Edge) = [
//        str <| sprintf "EDGE %u" n.id.Value
        yield h2 [Class "title"] [str "Current Edge"]
        yield control "Title" (input [Class "input"; Type "number"; Value n.offset; OnChange (dispatchEdgeChange (fun e -> {n with offset = e.Value |> int32}))])

    ]
    let selectedNodeView (n:Graph.Node) =
        let portView (dir:Direction) idx (p:Port) =
            a [Class "panel-block"] [
                    span [Class "panel-icon"] [ i [classList [
                        "fas",true; "fa-arrow-right",dir = Direction.Input; "fa-arrow-left", dir = Direction.Output
                    ]; HTMLAttr.Custom ("aria-hidden", true)] [] ]
//                    str"test"
                    input [Class "input"; Type "text"; Value p.title; OnChange (dispatchNodeChange (fun e ->
                                if dir = Direction.Input
                                then {n with inputs = replace n.inputs idx {p with title = e.Value} }
                                else {n with outputs = replace n.outputs idx {p with title = e.Value} }
                                ))]
                    button [ Class "button is-danger"; OnClick (dispatchNodeChange (fun _ ->
                                if dir = Direction.Input
                                then {n with inputs = n.inputs |> List.indexed |> List.choose (fun (cidx,p) -> if cidx = idx then None else Some p) }
                                else {n with outputs = n.outputs |> List.indexed |> List.choose (fun (cidx,p) -> if cidx = idx then None else Some p) }
                                )) ] [
                                span [Class "icon"] [ i [Class "fas fa-minus-circle"] [] ]
                            ]
            ]
//            div [Class "field is-horizontal"] [
//                div [Class "field-label is-normal"] [
//                    label [Class "label"] [str (string idx)]
//                ]
//                div [Class "field-body"] [
//                    div [Class "field is-narrow has-addons"] [
//                        div [Class "control"] [
//                            input [Class "input"; Type "text"; Value p.title; OnChange (dispatchNodeChange (fun e ->
//                                if dir = Direction.Input
//                                then {n with inputs = replace n.inputs idx {p with title = e.Value} }
//                                else {n with outputs = replace n.outputs idx {p with title = e.Value} }
//                                ))]
//                        ]
//                        div [Class "control"] [
//                            button [ Class "button is-danger"; OnClick (dispatchNodeChange (fun _ ->
//                                if dir = Direction.Input
//                                then {n with inputs = n.inputs |> List.indexed |> List.choose (fun (cidx,p) -> if cidx = idx then None else Some p) }
//                                else {n with outputs = n.outputs |> List.indexed |> List.choose (fun (cidx,p) -> if cidx = idx then None else Some p) }
//                                )) ] [
//                                span [Class "icon"] [ i [Class "fas fa-minus-circle"] [] ]
//                            ]
//                        ]
//                    ]
//
//                ]
//            ]
        let createPortButton dir =
            let onClick = dispatchNodeChange (fun _ ->
                if dir = Direction.Input
                then { n with inputs = n.inputs @ [ newPort "new" ] }
                else { n with outputs = n.outputs @ [ newPort "new" ] }
            )
            button [ Class "button"; OnClick onClick] [ span [Class "icon"] [ i [Class "fas fa-plus"] [] ]; span [] [str <| sprintf "New %A" dir ] ]
        [
            yield h2 [Class "title"] [str "Current Node"]
            yield control "Title" (input [Class "input"; Type "text"; Value n.title; OnChange (dispatchNodeChange (fun e -> {n with title = e.Value}))])

            yield article [Class "panel is-info"] [
                yield p [Class "panel-heading"] [
                    str <| sprintf "%i Inputs" (n.inputs.Length)
                ]
                yield div [Class "panel-block"][createPortButton Direction.Input]
                yield! n.inputs |> List.mapi (portView Direction.Input)
            ]
            yield article [Class "panel is-info"] [
                yield p [Class "panel-heading"] [
                    str <| sprintf "%i Outputs" (n.outputs.Length)
                ]
                yield div [Class "panel-block"][createPortButton Direction.Output]
                yield! n.outputs |> List.mapi (portView Direction.Output)
            ]
//            yield sectionLabelHorizontal "Inputs" [ createPortButton Direction.Input ]
//            yield! n.inputs |> List.mapi (portView Direction.Input)
//            yield sectionLabelHorizontal "Outputs" [ createPortButton Direction.Output ]
//            yield! n.outputs |> List.mapi (portView Direction.Output)
        ]

    div [] (seq {
        yield h2 [Class "title"] [str "Options"]
        yield controlCheckbox "Node Borders" model.options.NodeBorders (fun b -> {model.options with NodeBorders = b})// (input [Class "input"; Type "number"; Value model.options.Margin; OnChange (dispatchChange (fun e o -> {o with Margin = int e.Value}))])
        yield controlCheckbox "Show Ports" model.options.ShowPorts (fun b -> {model.options with ShowPorts = b})// (input [Class "input"; Type "number"; Value model.options.Margin; OnChange (dispatchChange (fun e o -> {o with Margin = int e.Value}))])
        yield control "Margin" (input [Class "input"; Type "number"; Value model.options.Margin; OnChange (dispatchChange (fun e o -> {o with Margin = int e.Value}))])
        if model.selectedNode.IsSome then yield! selectedNodeView model.selectedNode.Value
        if model.selectedPort.IsSome then yield! selectedPortView model.selectedPort.Value
    })
