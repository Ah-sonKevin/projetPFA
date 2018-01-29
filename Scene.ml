open Tsdl
open Objet

module Scene : Scene =  struct
  type entities = Objet.objet list 
  type textures = Sdl.texture list
  type scene = {entities:entities ; textures:textures}
    
  
end
  
