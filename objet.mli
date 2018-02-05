  
module type Objet = sig
  type genre_objet = Personnage|Ennemi|Plateforme|Wall|Door
  type objet
  val create : genre_objet -> int*int -> int*int -> int ->string ->int*int -> objet
    val move : objet ->(int*int) -> objet
    val calcul_pos : objet -> (int*int)
    val changePV : objet ->int -> objet
    val getPos : objet -> int*int
    val getPV : objet -> int
    val getSize : objet -> int*int
    val getTexture : objet -> string
      
end

module Objet : Objet
