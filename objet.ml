open Tsdl
open Anim

module type Objet = sig
  type genre_objet = Personnage|Ennemi|Plateforme|Wall|Door|Background|Projectile
  type objet
  val create : genre_objet -> int*int -> float*float -> float*float -> int ->(string array *string array *string array * string array) -> Sdl.renderer -> objet
  val create_immobile : genre_objet -> int*int -> string array -> Sdl.renderer -> objet
  val move : objet -> (int*int) -> objet
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
  val changeFrame : objet -> Anim.direction -> objet
  val setSize : objet -> int * int -> objet
  val getBaseSize : objet -> int*int

end

module Objet : Objet = struct
  type genre_objet = Personnage|Ennemi|Plateforme|Wall|Door|Background|Projectile
  type objet = {genre : genre_objet; position : int*int; can_jump : bool; vitesse : float * float ;
                maxSpeed : float*float; pv : int ;size : int*int; baseSize : int*int ; texture : Anim.anim }
  let create genre_o pos vit maxvit hp  (textG, textM, textD, textS) renderer  =
    let sizeT t=
      match Sdl.query_texture t with 
      |Error (`Msg e) -> Sdl.log "Init load picture error: %s" e; exit 1
      |Ok (_,_,y) -> y
    in
    let textu =  Anim.create textG textM textD textS renderer in 
    {genre = genre_o;
     position = pos;
     can_jump = true;
     vitesse = vit;
     maxSpeed = maxvit;
     pv = hp;
     texture = textu; 
     size =sizeT (Anim.getTexture textu) ;
     baseSize = sizeT (Anim.getTexture textu) ;
      }
  let create_immobile genre_o pos textM render =
    let sizeT t =
      match Sdl.query_texture t with 
      |Error (`Msg e) -> Sdl.log "Init load picture error: %s" e; exit 1
      |Ok (_,_,y) -> y
    in
    let textu = Anim.create [||] textM [||] [||] render in 
    {
      genre = genre_o;
      position = pos;
      can_jump = true;
      vitesse = (0.0,0.0);
      maxSpeed = (0.0,0.0);
      pv = 10000;
      texture = textu ;
      size =sizeT (Anim.getTexture textu) ;
      baseSize = sizeT (Anim.getTexture textu) ;	
    }

  let getBaseSize obj = obj.baseSize
          
  let setSpeed obj (x,y) =
    let (x1,y1) = obj.vitesse in
    let (msx,msy) = obj.maxSpeed in
    let signe = x *. x1 in
    let interx = if (signe >= 0.0) then x+.x1 else 0.0 in
    let intery = if (y < 0.0) && (obj.can_jump = true) then y else y+.y1 in
    let finalx = if x+.x1 >= msx  then msx else if interx <= -.msx then -.msx else interx in
    let finaly = if y+.y1 >= msy  then msy else if intery <= -.msy then -.msy else intery in
    {obj with vitesse = (finalx,finaly)}

  let resetSpeed obj = {obj with vitesse =(0.0,0.0)}
    
  let changePV obj a = {obj with pv = obj.pv+a }
    
  let move obj (x,y)  =
    {obj with position = (x,y)}

  let setSize obj (x,y) = {obj with size = (x,y)} 
	
  let getGenre obj = obj.genre      
  let getPos obj = obj.position
  let allowJump obj = {obj with can_jump = true}
  let forbidJump obj = {obj with can_jump = false}
  let canJump obj = obj.can_jump
  let getSpeed obj = obj.vitesse
  let dmgObjet obj = {obj with pv = obj.pv - 20}    
  let getPV  obj = obj.pv    
  let getTexture obj =  Anim.getTexture obj.texture
  let isMovable obj = if (obj.genre = Personnage) || (obj.genre = Ennemi) || (obj.genre = Projectile) then true else false
  let changeFrame obj dir = {obj with texture = Anim.changeFrame obj.texture dir}
  let getSize2 obj = obj.size 

  let getSize obj = 
    let x = Anim.getTexture obj.texture in
   match Sdl.query_texture x with 
    |Error (`Msg e) -> Sdl.log "Init load picture error: %s" e; exit 1
    |Ok (_,_,(x,y)) -> (x,y)
      

    
end
