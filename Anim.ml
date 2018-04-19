open Tsdl
open Tools

module type Anim = sig 
  type direction = Gauche|Droite|Milieu|Saut
  type anim = {gauche : Sdl.texture array ; milieu : Sdl.texture array ; droite : Sdl.texture array; saut : Sdl.texture array ;frame : int; dir : direction; animated : bool}
  val create :  string  array ->  string array -> string  array -> string array -> Sdl.renderer -> anim
  val getFrame : anim -> int
  val changeFrame :anim -> direction -> anim
  val getTexture : anim -> Sdl.texture
  val changeDir : anim -> direction -> anim
end

module Anim : Anim = struct 
  type direction = Gauche|Droite|Milieu|Saut
  type anim = {gauche : Sdl.texture array ; milieu : Sdl.texture array ; droite : Sdl.texture array; saut : Sdl.texture array; frame : int; dir : direction ; animated : bool}
  
  let create g m d s renderer = 
      let animBool = if (((Array.length g) = 0)&&((Array.length d) = 0)) then false else true in 
	  {
	  gauche = (Array.init (Array.length g) (fun i -> Tools.loadTexture g.(i)));
	  milieu = (Array.init (Array.length m) (fun i -> Tools.loadTexture m.(i)));
	  droite = (Array.init (Array.length d) (fun i -> Tools.loadTexture d.(i)));
	  saut   = (Array.init (Array.length s) (fun i -> Tools.loadTexture s.(i)));
	  frame = 0;
	  dir = Milieu;
	  animated = animBool
	  }

  let getFrame ani = ani.frame
  let changeDir ani newDir =  {ani with dir = newDir; frame = 0 }

  let changeFrame ani newDir =
    if (ani.dir != newDir &&  ani.animated = true) then 
      {ani with dir = newDir; frame = 0 } 
    else
      match ani.dir with 
      |Milieu -> {ani with frame = (((ani.frame)+1)mod (Array.length ani.milieu))}
      |Gauche -> {ani with frame = (((ani.frame)+1)mod (Array.length ani.gauche))}
      |Droite -> {ani with frame = (((ani.frame)+1)mod (Array.length ani.droite))}
      |Saut ->   {ani with frame = (((ani.frame)+1)mod (Array.length ani.saut))} 
                   
  let getTexture ani = 
    match ani.dir with 
    |Milieu -> ani.milieu.(ani.frame)
    |Gauche -> ani.gauche.(ani.frame)
    |Droite -> ani.droite.(ani.frame)
    |Saut   -> ani.saut.(ani.frame)
end
