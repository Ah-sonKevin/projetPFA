open Tsdl 
open Sound
module type Menu = sig 
val my_exit : Sdl.window * Sdl.renderer -> unit 
val evenement : Sdl.window * Sdl.renderer -> bool -> Sound.sound ->  bool option
val startMenu : Sdl.window ->  Sdl.renderer -> Sound.sound -> unit 
end

module Menu : Menu
