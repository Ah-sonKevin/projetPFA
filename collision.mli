open Tsdl
open Objet

module type Collision  = sig
  val checkCollision : Objet.objet -> Objet.objet -> bool
  val directionCollision : Objet.objet -> Objet.objet -> int
  val replace : Objet.objet -> int -> Objet.objet -> Objet.objet
  val collision_perso : Objet.objet -> Objet.objet -> Objet.objet
  val collision_ennemi : Objet.objet -> Objet.objet -> Objet.objet
  val collision_projectile : Objet.objet -> Objet.objet
  val collision : Objet.objet -> Objet.objet -> Objet.objet
end

module Collision : Collision
