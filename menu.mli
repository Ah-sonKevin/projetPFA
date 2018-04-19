open Tsdl 
open Sound
module type Menu = sig 
  type choix
  type soundSetting
  val my_exit : Sdl.window * Sdl.renderer -> unit 
  val evenement : Sdl.window * Sdl.renderer -> choix ->  choix  option
  val startMenu : Sdl.window ->  Sdl.renderer  ->  unit 
end

module Menu : Menu
