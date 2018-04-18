open Tsdl 

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
    let loadTexture x = 
      match Sdl.load_bmp x with
      | Error (`Msg e) -> Sdl.log "Init load picture error: %s" e; exit 1
      | Ok surface_temp ->
	 match Sdl.create_texture_from_surface renderer surface_temp with
	 | Error (`Msg e) -> Sdl.log "Init surface to texture error: %s" e; exit 1
	 | Ok name -> Sdl.free_surface surface_temp;name
    in    
      let animBool = if (((Array.length g) = 0)&&((Array.length d) = 0)) then false else true in 
	  {
	  gauche = (Array.init (Array.length g) (fun i -> loadTexture g.(i)));
	  milieu = (Array.init (Array.length m) (fun i -> loadTexture m.(i)));
	  droite = (Array.init (Array.length d) (fun i -> loadTexture d.(i)));
	  saut   = (Array.init (Array.length s) (fun i -> loadTexture s.(i)));
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
