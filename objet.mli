open Tsdl
open Anim

module type Objet = sig
  type genre_objet = Personnage|Ennemi|Plateforme|Wall|Door|Background|Projectile
  type objet
  val create : genre_objet -> int*int -> float*float -> float*float -> int ->(string array * string array * string array * string array ) -> Sdl.renderer -> objet
  val create_immobile : genre_objet -> int*int -> string array -> Sdl.renderer -> objet
  val move : objet ->(int*int)->  objet
  val changePV : objet -> int -> objet
  val setSpeed : objet -> (float*float) -> objet
  val resetSpeed : objet -> objet
  val getGenre : objet -> genre_objet
  val getPos : objet -> int*int
  val allowJump : objet -> objet
  val forbidJump : objet -> objet
  val canJump : objet -> bool
  val getSpeed : objet -> float*float
  val dmgObjet : objet -> objet
  val getPV : objet -> int
  val getSize : objet -> int*int
  val getTexture : objet -> Sdl.texture
  val isMovable : objet -> bool

 (* val getFrame : objet -> int
  *)
  val changeFrame : objet -> Anim.direction -> objet 
  val setSize : objet -> int*int -> objet
  val getBaseSize : objet -> int*int
end

module Objet : Objet
