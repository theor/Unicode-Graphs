module App.BinarySerialization

open Fable.Core



//let w = state {
//            do! putS 1
//            let! x = getS
//            return x
//        }
//let ww = runS w 0

let write f (x:'a): S<State,unit> =
    state {
        let! s = getS
        let newOffset = f s.view s.offset
        do! putS {s with offset = s.offset + newOffset}

    }
let writeInt2 (i:int): S<State,unit> = write (fun v o -> v.setInt32(o, i); 4) i
let writeBool (i:bool): S<State,unit> = write (fun v o -> v.setInt8(o, if i then 1y else 0y); 1) i
let writeInt (i:int): S<State,unit> =
    state {
        let! s = getS
        do s.view.setInt32(s.offset, i)
        do JS.console.log("write", i, s.offset)
        do! putS {s with offset = s.offset + 4}
    }
let runState buffer w =
    let s:State = {offset=0; view=JS.Constructors.DataView.Create(buffer)}
    let _, s = runS w s
    s
let test() =
    JS.console.log("BUFFER")
    let b = JS.Constructors.ArrayBuffer.Create(50)

    let w: S<State, unit> = state {
        do! writeInt2 42
        do! writeBool true
        do! writeInt 44
        return ()
    }
    let r = runState b w
    JS.console.log("BUFFER",b, r)