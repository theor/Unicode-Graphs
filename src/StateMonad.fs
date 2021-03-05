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
    member this.Zero() = S (fun x -> (), x)
    member this.Return(x) = returnM x
    member this.ReturnFrom(xs) = xs
    member this.Bind(xM,f) = bindM f xM
    member this.Combine(a:S<'S,'a>,b:S<'S,'b>) =
        S (fun s ->
                let value,state = runS a s in runS b state)
//        match a,b with
//        | S sa, S sb -> 
//            S (fun s -> let value,state = sa(s) in sb(state))
//    member this.Run(f) = f() 
    member this.Delay(f:unit -> _) = f()
    member this.While (f, x) =
        if f () then this.Combine (x, this.While (f, x))
        else this.Zero ()
//    member this.While(guard, body) =
//        if not (guard())
//        then this.Zero()
//        else this.Bind( body(), fun () ->
//            this.While(guard, body))
        
    member this.TryFinally(body, compensation) =
        try this.ReturnFrom(body())
        finally compensation()
        
    member this.Using(disposable:#System.IDisposable, body) =
        let body' = fun () -> body disposable
        this.TryFinally(body', fun () ->
            match disposable with
                | null -> ()
                | disp -> disp.Dispose())
      
    member this.For(sequence:seq<_>, f) =
        sequence
        |> Seq.map f
        |> Seq.reduce(fun x1 x2 -> this.Combine (x1, x2)) 
//        |> (fun x -> x())
//        this.Using(sequence.GetEnumerator(),fun enum ->
//            this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current)))

//type StateBuilder() =
//    member this.Zero () = S(fun s -> (), s)
//    member this.Return x = S(fun s -> x, s)
//    member inline this.ReturnFrom (x: S<'s, 'a>) = x
//    member this.Bind (x, f) : S<'s, 'b> =
//        S(fun state ->
//            let (result: 'a), state = runS state x
//            runS state (f result))
//    member this.Combine (x1: S<'s, 'a>, x2: S<'s, 'b>) =
//        S(fun state ->
//            let result, state = runS state x1
//            runS state x2)
//    member this.Delay f : S<'s, 'a> = f ()
//    member this.For (seq, (f: 'a -> S<'s, 'b>)) =
//        seq
//        |> Seq.map f
//        |> Seq.reduceBack (fun x1 x2 -> this.Combine (x1, x2))
//    member this.While (f, x) =
//        if f () then this.Combine (x, this.While (f, x))
//        else this.Zero ()

        
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