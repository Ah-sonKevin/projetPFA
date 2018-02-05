open Tsdl


    
module type Objet = sig
  type genre_objet = Personnage|Ennemi|Plateforme|Wall|Door
  type objet
  val create : genre_objet -> int*int -> int*int ->int-> string ->int*int -> objet
  val move : objet ->(int*int) -> objet
  val calcul_pos : objet -> (int*int)
  val changePV : objet ->int -> objet
  val getPos : objet -> int*int
  val getPV : objet -> int
  val getSize : objet  -> int*int
  val getTexture : objet  -> string
end

module Objet : Objet = struct
  type genre_objet = Personnage|Ennemi|Plateforme|Wall|Door
  type objet = {genre : genre_objet;   position : int*int;vitesse : int * int ; pv : int ;size : int*int ;  texture : string }
  let create genre_o pos vit hp  text s  =   {genre=genre_o;vitesse =vit; position = pos; pv = hp ; texture=text; size =s}
    
  let move obj (a,b)  =
    {obj with position = (a,b)}

  let calcul_pos obj =    
    let (x,y)=obj.position in
    let (vx,vy) = obj.vitesse in
    (x+vx,y+vy)
          
  let move obj (x,y) = {obj with position =(x,y)}			       
      
  let changePV obj a =    {obj with pv = obj.pv+a }

  let getPos  obj = obj.position
  let getPV  obj = obj.pv
  let getSize obj = obj.size
  let getTexture obj = obj.texture 
end
