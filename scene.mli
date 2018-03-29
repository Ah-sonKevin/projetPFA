open Tsdl
open Objet

module type Scene = sig
  type scene
    
  val create : Objet.objet list -> float -> Objet.objet -> Sdl.renderer -> scene
  val getEntitie : scene -> Objet.objet list
  val getTexture : scene -> Sdl.texture list
  val getGravitie : scene -> float
  val addEntitie : scene -> Objet.objet -> scene
  val generateScene : scene -> Objet.objet list -> scene
  (* val applyGravitie : scene -> scene *)
  val moveAll : scene -> scene
  val movePersonnage : scene -> (float*float) -> scene
  val loadPicture : Sdl.renderer -> (int*int) -> (int*int) -> Sdl.texture -> unit
  val refresh : scene -> scene -> unit
  val hit_box : Objet.objet -> scene -> ((int*int) * (float * float)) option
  val closeScene : scene -> unit
    
    
end
  
module Scene : Scene
