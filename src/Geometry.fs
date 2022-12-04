module App.Geometry

open Fable.Core.JS

type Pos = int * int

[<Struct>]
type Rect =
    { X: int
      Y: int
      W: int
      H: int }
    member this.Center =
        int
        <| Math.floor (float this.X + float this.W / 2.0),
        int
        <| Math.floor (float this.Y + float this.H / 2.0)

    static member Create(x, y, w, h): Rect = { X = x; Y = y; W = w; H = h }

[<Struct>]
type RectF =
    { X: float32
      Y: float32
      W: float32
      H: float32 }
    static member Create(x, y, w, h): RectF = { X = x; Y = y; W = w; H = h }
module Rect =
    let contains (x, y) (r: Rect) =
        x >= r.X
        && x < r.X + r.W
        && y >= r.Y
        && y < r.Y + r.H
