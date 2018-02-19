open Tsdl
open Objet

module type Scene = sig
  type scene
    
  val create : Objet.objet list -> int -> Objet.objet -> Sdl.renderer -> scene
  val getEntitie : scene -> Objet.objet list
  val getTexture : scene -> Sdl.texture list
  val getGravitie : scene -> int
  val addEntitie : scene -> Objet.objet -> scene
  val generateScene : scene -> Objet.objet list -> scene
  val applyGravitie : scene -> scene
  val moveAll : scene -> scene
  val movePersonnage : scene -> (int*int) -> scene
  val loadPicture : Sdl.renderer -> (int*int) -> (int*int) -> Sdl.texture -> unit
  val refresh : scene -> scene -> unit
  val closeScene : scene -> unit
end
  
module Scene : Scene
