open Tsdl
open Scene


module type GameMap = sig
  val evenement :  Sdl.event ->  Sdl.window -> Sdl.renderer ->  bool -> bool
  val startMap : Sdl.window ->  Sdl.renderer -> Scene.scene ->  unit  
  val startMapMini : Sdl.window ->  Sdl.renderer -> Scene.scene ->  unit  
end
  
module GameMap : GameMap
