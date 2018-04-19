open Tsdl
open Objet
open Scene
open Sound

  
(* Gestion Procedure de fermeture de la fenetre *)
val my_exit : (Sdl.window * Sdl.renderer) ->  Scene.scene -> Sound.sound -> unit

(* Gestion des entré clavier/souris  *)
val evenement : Sdl.event -> Sdl.window -> Sdl.renderer -> Scene.scene ->  Sound.sound ->   Scene.scene

val jeu : unit -> unit

val main : unit -> unit


  
