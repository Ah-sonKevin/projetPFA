open Tsdl
(* variable qui contient la liste des textures affichées *)
val background : Sdl.texture list ref

(* Gestion Procedure de fermeture de la fenetre *)
val my_exit : (Sdl.window * Sdl.renderer) -> unit

(* Gestion des entré clavier/souris  *)
val evenement : Sdl.event -> (Sdl.window * Sdl.renderer) -> unit
  
(* Gestion de chargement d'une image dans son intégralité *)
val load_picture_full : (Sdl.window * Sdl.renderer) -> (int * int) -> string -> unit

(* Gestion de chargement d'une fraction d'image *)
val load_picture_fragment : (Sdl.window * Sdl.renderer) -> (int * int) -> (int * int * int * int) -> string -> unit

val affichage : unit -> unit

val main : unit -> unit
