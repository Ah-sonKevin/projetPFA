open Tsdl
open Anim

module type Objet = sig
  type genre_objet = Personnage|Ennemi|Plateforme of int * int |Wall of int * int|Door of string |Background|Projectile
  type objet
  val create : genre_objet -> int*int -> float*float -> float*float -> int ->   Anim.anim -> Sdl.renderer -> objet
  val move : objet -> (int*int) -> objet
  val changePV : objet -> int -> objet
  val setSpeed : objet -> (float*float) -> objet
  val resetSpeed : objet -> objet
  val reposition : objet -> int*int -> float*float-> objet
  val getGenre : objet -> genre_objet
  val getPos : objet -> int*int
  val getOldPos : objet -> int*int
  val allowJump : objet -> objet
  val forbidJump : objet -> objet
  val canJump : objet -> bool
  val getSpeed : objet -> float*float
  val getPV : objet -> int
  val getSize : objet -> int*int
  val getTexture : objet -> Sdl.texture
  val isMovable : objet -> bool
  val changeFrame : objet -> Anim.direction -> objet
  val getBaseSize : objet -> int*int
  val print : objet -> unit
  val getMaxSpeed : objet -> float * float
  val getAnim : objet -> Anim.anim
  val kill : objet -> objet
  val canShoot : objet -> bool
  val canBeDmg : objet -> bool 
  val triggerShoot : objet -> objet
  val triggerInv : objet -> objet
  val decreaseClock : objet -> objet
end

module Objet : Objet
