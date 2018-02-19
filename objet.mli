open Tsdl

module type Objet = sig
  type genre_objet = Personnage|Ennemi|Plateforme|Wall|Door|Background
  type objet
  val create : genre_objet -> int*int -> int*int -> int*int -> int ->string ->int*int -> Sdl.renderer -> objet
  val move : objet  -> objet
  val calcul_pos : objet -> (int*int)
  val changePV : objet -> int -> objet
  val setSpeed : objet -> (int*int) -> objet
  val getNextPosition : objet -> (int*int)
  val getHitBox : objet -> (int*int*int*int)
  val getGenre : objet -> genre_objet
  val getPos : objet -> int*int
  val getPV : objet -> int
  val getSize : objet -> int*int
  val getPath : objet -> string
  val getTexture : objet -> Sdl.texture        
      
end

module Objet : Objet
