open Tsdl
open Objet

module type Tools = sig
  val loadPicture : Sdl.renderer -> int * int -> int * int -> Sdl.texture -> unit
  val loadPicturePosObj : Sdl.renderer -> Objet.objet -> unit
  val drawRect : int * int -> int * int -> Sdl.renderer -> unit
  val drawFillRect : int * int -> int * int -> Sdl.renderer -> unit 
  val chooseColor  : int -> int -> int -> int -> Sdl.renderer   -> unit
end

module Tools : Tools
