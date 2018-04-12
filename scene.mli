open Tsdl
open Objet
open Camera
open Sound

module type Scene = sig
  type scene
  val create : Objet.objet list -> float -> Objet.objet -> Camera.camera -> Sdl.renderer -> Sound.sound -> string -> scene
  val getEntitie : scene -> Objet.objet list
  val getTexture : scene -> Sdl.texture list
  val getSize : scene -> (int*int)
  val addEntitie : scene -> Objet.objet -> scene
  val kickDead : scene -> scene
  val loadPicture : Sdl.renderer -> (int*int) -> (int*int) -> Sdl.texture -> unit
  val refresh : scene -> scene -> unit
  val closeScene : scene -> unit
  val moveAll : scene -> scene
  val movePersonnage : scene -> (float*float) -> scene 
  val continue : scene -> bool
  val suicide : scene -> scene
  val shoot : Objet.objet -> scene -> (int*int) -> scene
  val getPers : scene -> Objet.objet
  val decreaseClock : scene -> scene
end
  
module Scene : Scene
