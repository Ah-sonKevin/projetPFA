open Objet

module type Collision = sig 
  val kickObjet    : Objet.objet -> Objet.objet list -> Objet.objet list
  val hit_boxObjet : Objet.objet -> Objet.objet list -> (int * int ) -> ((int * int) * (float * float)) option
end
                          
module Collision : Collision
