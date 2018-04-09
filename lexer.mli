open Tsdl
open Objet

val lex : string -> Objet.objet option -> Sdl.renderer -> (Objet.objet list * float * Objet.objet* Objet.objet)
