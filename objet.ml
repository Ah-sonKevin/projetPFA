open Tsdl
open Anim

module type Objet = sig
  type genre_objet = Personnage|Ennemi|Plateforme|Wall|Door of string |Background|Projectile
  type objet
  val create : genre_objet -> int*int -> float*float -> float*float -> int -> Anim.anim -> Sdl.renderer -> objet
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
  val dmgGesture : objet -> objet
  val getSpeed : objet -> float*float
  val dmgObjet : objet -> objet
  val getPV : objet -> int
  val getSize : objet -> int*int
  val getTexture : objet -> Sdl.texture
  val isMovable : objet -> bool
  val changeFrame : objet -> Anim.direction -> objet
  val setSize : objet -> int * int -> objet
  val getBaseSize : objet -> int*int
  val print : objet -> unit
  val getMaxSpeed : objet -> float * float
  val getAnim : objet -> Anim.anim
end
 
module Objet : Objet = struct
  type genre_objet = Personnage|Ennemi|Plateforme|Wall|Door of string|Background|Projectile
  type objet = {genre : genre_objet; position : int*int; can_jump : bool; vitesse : float * float ;
                maxSpeed : float*float; pv : int;canBeDmg : bool*int  ;size : int*int; baseSize : int*int ; texture : Anim.anim }
  let create genre_o pos vit maxvit hp textu renderer  =
    let sizeT t=
      match Sdl.query_texture t with 
      |Error (`Msg e) -> Sdl.log "Init load picture error: %s" e; exit 1
      |Ok (_,_,y) -> y
    in
    {genre = genre_o;
     position = pos;
     can_jump = true;
     vitesse = vit;
     maxSpeed = maxvit;
     pv = hp;
     canBeDmg = if ((genre_o = Personnage) || (genre_o = Ennemi)) then (true,0) else (false,(-1));
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
      canBeDmg = if ((genre_o = Personnage) || (genre_o = Ennemi)) then (true,0) else (false,(-1));
      texture = textu ;
      size =sizeT (Anim.getTexture textu) ;
      baseSize = sizeT (Anim.getTexture textu) ;	
    }

      let print obj = 
        let (x,y) = obj.position in 
        let (xs,ys) = obj.vitesse in
        let (xsm,ysm) = obj.maxSpeed in
        let pv = obj.pv in
        Printf.printf "Pos : %d %d \n Speed : %f %f \n MaxSpeed : %f %f \n PV : %d \n\n" x y xs ys xsm ysm pv 


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
    
  let changePV obj a =
    let (b,x) = obj.canBeDmg in
    if b
    then
      begin
	match obj.genre with
	|Personnage -> {obj with pv = obj.pv+a ; canBeDmg = (false,5)}
	|Ennemi     -> {obj with pv = obj.pv+a }
	|_          -> obj
      end
    else obj
    
  let move obj (x,y)  =
    {obj with position = (x,y)}

  let setSize obj (x,y) = {obj with size = (x,y)} 
	
  let getGenre obj = obj.genre      
  let getPos obj = obj.position
  let allowJump obj = {obj with can_jump = true}
  let forbidJump obj = {obj with can_jump = false}
  let canJump obj = obj.can_jump
  let dmgGesture obj =
    let (b,x) = obj.canBeDmg in
    match x with
    |y when (y=0) -> {obj with canBeDmg = (true,0)}
    |y when (y>0) -> {obj with canBeDmg = (false,(x-1))}
    |y when (y<0) -> {obj with canBeDmg = (false,(-1))}
    |_            -> obj
  let getSpeed obj = obj.vitesse
  let getMaxSpeed obj = obj.maxSpeed
  let dmgObjet obj = {obj with pv = obj.pv - 20}    
  let getPV  obj = obj.pv
  let getAnim obj = obj.texture 
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
