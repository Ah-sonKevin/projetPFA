open Tsdl 
 
module type Menu = sig 
val my_exit : Sdl.window * Sdl.renderer -> unit 
val evenement : Sdl.window * Sdl.renderer -> bool -> bool option
val startMenu : Sdl.window ->  Sdl.renderer -> unit
end

module Menu : Menu
