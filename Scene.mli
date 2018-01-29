open Tsdl
open Objet

module type Scene = sig
  type entities = Objet.objet list 
  type textures = Sdl.texture list
  type scene
    
  val create : entities -> textures -> scene
end
  
