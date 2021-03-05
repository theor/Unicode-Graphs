[<AutoOpen>]
module App.StateMonad

open Fable.Core

type State = {
    view: JS.DataView
    offset: int
}
type S<'State,'Value> =
    S of ('State -> 'Value * 'State)
let returnM x =
    let run state = x,state
    S run
let runS (S f) state = f state
let bindM f xS =
    let run state =
        let x,newState = runS xS state
        runS (f x) newState
    S run
type StateBuilder()=
    member this.Return(x) =
//        JS.console.log("return", this, x)
        returnM x
    member this.ReturnFrom(xs) =
//        JS.console.log("return from", this, xs)
        xs
    member this.Bind(xM,f) =
//        JS.console.log("bind", this, xM, f)
        bindM f xM
    member this.Zero() = S (fun x -> (),x)
let state = StateBuilder()

let getS =
    let run state =
        // return the current state in the first element of the tuple
        state, state
    S run
// val getS : S<State>

let putS newState =
    let run _ =
        // return nothing in the first element of the tuple
        // return the newState in the second element of the tuple
        (), newState
    S run