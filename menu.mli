open Tsdl 
open Sound
module type Menu = sig 
  type choix
  type soundSetting
  val my_exit : Sdl.window * Sdl.renderer -> unit 
  val evenement : Sdl.window * Sdl.renderer -> choix -> Sound.sound ->  choix  option
  val startMenu : Sdl.window ->  Sdl.renderer -> Sound.sound ->  unit 
end

module Menu : Menu
