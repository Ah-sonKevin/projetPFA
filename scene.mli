open Tsdl
open Objet
open Camera


module type Scene = sig
  type scene
  val create : float ->  Objet.objet list -> Objet.objet -> Camera.camera -> Sdl.renderer -> scene
  val getEntitie : scene -> Objet.objet list
  val getTexture : scene -> Sdl.texture list
  val getSize : scene -> (int*int)
  val addEntitie : scene -> Objet.objet -> scene
  val kickDead : scene -> scene
  val generateScene : scene -> Objet.objet list -> scene
  val loadPicture : Sdl.renderer -> (int*int) -> (int*int) -> Sdl.texture -> unit
  val refresh : scene -> scene -> unit
  val closeScene : scene -> unit
  val moveAll : scene -> scene
  val movePersonnage : scene -> (float*float) -> scene
  val continue : scene -> bool
  val suicide : scene -> scene
  val shoot : scene -> (int*int) -> int -> scene
end
  
module Scene : Scene
