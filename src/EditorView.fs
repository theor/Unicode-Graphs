module App.EditorView

open App.Graph
open Browser.Types
open Fable.Core
open Fable.React
open Fable.React.Props
open App.Types

let view (model: Model) dispatch =
    let dispatchChange (f: Event -> RenderOptions -> RenderOptions) (e: Event) =
        dispatch <| ChangeOptions(f e model.options)

    let dispatchNodeChange (f: Event -> Graph.Node) (e: Event) = dispatch <| ChangeNode(f e)
    let dispatchEdgeChange (f: Event -> Edge) (e: Event) = dispatch <| ChangeEdge(f e)

    let controlCheckbox (name: string) (value: bool) (setValue: bool -> RenderOptions) =
        //        printfn "%s %b" name value
        div [ Class "field" ] [
            div [ Class "control" ] [
                label [ Class "checkbox" ] [
                    input [ Type "checkbox"
                            Checked value
                            OnChange(dispatchChange (fun e _ -> setValue (e.Checked))) ]
                    str name
                ]
            ]
        ]

    let control (name: string) controlInput =
        div [ Class "field" ] [
            label [ Class "label" ] [ str name ]
            div [ Class "control" ] [ controlInput ]
        ]


    let replace l idx x =
        List.mapi (fun i elt -> if i <> idx then elt else x) l

    let selectedPortView (n: Port) =
        [ str <| sprintf "Port %A: %s" n.guid n.title ]

    let selectedEdgeView (n: Edge) =
        [ yield
            control
                "Edge Offset"
                (input [ Class "input"
                         Type "number"
                         Value n.offset
                         OnChange(dispatchEdgeChange (fun e -> { n with offset = e.Value |> int8 })) ]) ]

    let selectedNodeView (n: Graph.Node) =
        let portView (dir: Direction) idx (p: Port) =
            a [ Class "panel-block" ] [
                span [ Class "panel-icon" ] [
                    i [ classList [ "fas", true
                                    "fa-arrow-right", dir = Direction.Input
                                    "fa-arrow-left", dir = Direction.Output ]
                        HTMLAttr.Custom("aria-hidden", true) ] []
                ]
                //                    str"test"
                input [ Class "input"
                        Type "text"
                        Value p.title
                        OnChange
                            (dispatchNodeChange (fun e ->
                                if dir = Direction.Input then
                                    { n with
                                          inputs = replace n.inputs idx { p with title = e.Value } }
                                else
                                    { n with
                                          outputs = replace n.outputs idx { p with title = e.Value } })) ]
                button [ Class "button is-danger"
                         OnClick
                             (dispatchNodeChange (fun _ ->
                                 if dir = Direction.Input then
                                     { n with
                                           inputs =
                                               n.inputs
                                               |> List.indexed
                                               |> List.choose (fun (cidx, p) -> if cidx = idx then None else Some p) }
                                 else
                                     { n with
                                           outputs =
                                               n.outputs
                                               |> List.indexed
                                               |> List.choose (fun (cidx, p) -> if cidx = idx then None else Some p) })) ] [
                    span [ Class "icon" ] [
                        i [ Class "fas fa-minus-circle" ] []
                    ]
                ]
            ]

        let createPortButton dir =
            let onClick =
                dispatchNodeChange (fun _ ->
                    if dir = Direction.Input then
                        { n with
                              inputs =
                                  n.inputs
                                  @ [ GraphBuilder(model.graph).newPort "new" ] }
                    else
                        { n with
                              outputs =
                                  n.outputs
                                  @ [ GraphBuilder(model.graph).newPort "new" ] })

            button [ Class "button"; OnClick onClick ] [
                span [ Class "icon" ] [
                    i [ Class "fas fa-plus" ] []
                ]
                span [] [ str <| sprintf "New %A" dir ]
            ]

        [
          //            yield h2 [Class "title"] [str "Current Node"]
          yield
              control
                  "Node Title"
                  (input [ Class "input"
                           Type "text"
                           Value n.title
                           OnChange(dispatchNodeChange (fun e -> { n with title = e.Value })) ])

          yield
              article [ Class "panel is-info" ] [
                  yield
                      p [ Class "panel-heading" ] [
                          str <| sprintf "%i Inputs" (n.inputs.Length)
                      ]
                  yield
                      div [ Class "panel-block" ] [
                          createPortButton Direction.Input
                      ]
                  yield! n.inputs |> List.mapi (portView Direction.Input)
              ]
          yield
              article [ Class "panel is-info" ] [
                  yield
                      p [ Class "panel-heading" ] [
                          str <| sprintf "%i Outputs" (n.outputs.Length)
                      ]
                  yield
                      div [ Class "panel-block" ] [
                          createPortButton Direction.Output
                      ]
                  yield! n.outputs |> List.mapi (portView Direction.Output)
              ] ]

    let onKeyDown (e:KeyboardEvent) =
//        JS.console.log e
        // prevent delete/backspace to bubble up and delete the selected node
        match e.key with
        | "Delete" | "Backspace" -> e.stopPropagation()
        | _ -> ()
    Fulma.Content.content
        [ Fulma.Content.Option.Props [ OnKeyDown onKeyDown ]]
        ([ if model.selectedNode.IsSome
           then yield! selectedNodeView model.selectedNode.Value
           if model.selectedPort.IsSome
           then yield! selectedPortView model.selectedPort.Value
           if model.selectedEdge.IsSome
           then yield! selectedEdgeView model.selectedEdge.Value
//           yield
//               controlCheckbox "Show Node Borders" model.options.NodeBorders (fun b ->
//                   { model.options with NodeBorders = b })
//           yield controlCheckbox "Show Ports" model.options.ShowPorts (fun b -> { model.options with ShowPorts = b })
           yield controlCheckbox "Show Debug Ids" model.options.ShowIds (fun b -> { model.options with ShowIds = b })
        ])
