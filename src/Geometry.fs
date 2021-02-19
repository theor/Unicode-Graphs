module App.Geometry

type Pos = int*int

[<Struct>]
type Rect =
    { X:int
      Y:int
      W:int
      H:int }
    member this.Center = (this.X+this.W/2, this.Y+this.H/2)
    
    static member Create(x,y,w,h):Rect = {X=x;Y=y;W=w;H=h}
module Rect =
    let contains (x,y) (r:Rect) = x >= r.X && x < r.X+r.W && y >= r.Y && y < r.Y + r.H 