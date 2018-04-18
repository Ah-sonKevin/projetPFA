open Tsdl

module type Anim = sig 
  type direction = Gauche|Droite|Milieu|Saut
  type anim = {gauche : Sdl.texture array ; milieu : Sdl.texture array ; droite : Sdl.texture array; saut : Sdl.texture array ;frame : int; dir : direction; animated : bool}
  val create :  string  array ->  string array -> string  array -> string array -> Sdl.renderer -> anim
  val getFrame : anim -> int
  val changeFrame :anim -> direction -> anim
  val getTexture : anim -> Sdl.texture
  val changeDir : anim -> direction -> anim
  val getBaseText : anim -> anim
end

module Anim : Anim
 
