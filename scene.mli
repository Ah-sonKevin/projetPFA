open Tsdl
open Objet
open Camera


module type Scene = sig
  type scene
  val create : Objet.objet list -> float -> Objet.objet -> Camera.camera -> Sdl.renderer -> scene
  val getEntitie : scene -> Objet.objet list
  val getTexture : scene -> Sdl.texture list
  val addEntitie : scene -> Objet.objet -> scene
  val generateScene : scene -> Objet.objet list -> scene
  val loadPicture : Sdl.renderer -> (int*int) -> (int*int) -> Sdl.texture -> unit
  val refresh : scene -> scene -> unit
  val closeScene : scene -> unit
  val moveAll : scene -> scene
  val movePersonnage : scene -> (float*float) -> scene
  val suicide : scene -> scene 

end
  
module Scene : Scene
