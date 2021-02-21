module App.View
open App.Types
open Fable.React
open Fable.React.Props

let view (model:Model) dispatch =
  let doCopy () =
      let text = Browser.Dom.document.querySelector("#hidden-output")
      let r = Browser.Dom.document.createRange()
      r.selectNode text
      Browser.Dom.window.getSelection().removeAllRanges();
      Browser.Dom.window.getSelection().addRange(r)
      Browser.Dom.document.execCommand("Copy") |> ignore
      ()
  div [] [
      section [ Class "section" ] [
        div [ Class "container" ]
          (seq {
            yield p [Class "buttons"] [
              button [Class "button"; OnClick (fun _ -> dispatch (AddNode(Graph.nextId(), "New")))] [str "New Node"]
            ]
            yield div [Class "columns"] [
              div [Class "column"]  (seq {
                yield! GraphRender.render dispatch model
                yield! GraphRender.render dispatch (GraphRender.layout {model with options = {model.options with NodeBorders = not model.options.NodeBorders} })
              })
              div [Class "column"] [
                App.EditorView.view model dispatch
              ]

            ]

          })
        div [ Class "container" ] [
          button [ClassName "button is-primary"
                  OnClick (fun _ -> doCopy())] [str "Copy"]
        ]
      ]

      section [ Class "section" ] [
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
