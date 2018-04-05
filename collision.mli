open Tsdl
open Objet

module type Collision  = sig 
  val kickObject    : Objet.objet -> Objet.objet list -> Objet.objet list
  val kickObjectWithProj    : Objet.objet -> Objet.objet list -> Objet.objet list -> Objet.objet list
  val kickObjectWithMovable    : Objet.objet -> Objet.objet list -> Objet.objet list -> Objet.objet list
  val hit_boxObjet : Objet.objet -> Objet.objet list -> (int * int)-> (int*int)  -> ((int * int) * (float * float)) option
  val hit_boxPerso : Objet.objet -> Objet.objet list -> (int * int)-> (int*int)  -> (Objet.objet * (int * int) * (float * float)) option
  val hit_boxProj : Objet.objet -> Objet.objet list -> float-> (int*int) -> (Objet.objet) option
end

module Collision : Collision
