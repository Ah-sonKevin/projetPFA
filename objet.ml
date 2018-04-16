open Tsdl
open Anim

module type Objet = sig
  type genre_objet = Personnage|Ennemi|Plateforme|Wall|Door of string |Background|Projectile
  type objet
  val create : genre_objet -> int*int -> float*float -> float*float -> int -> Anim.anim -> Sdl.renderer -> objet
  val move : objet -> (int*int) -> objet
  val changePV : objet -> int -> objet
  val setSpeed : objet -> (float*float) -> objet
  val resetSpeed : objet -> objet
  val reposition : objet -> int*int -> float*float -> objet
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
 
module Objet : Objet = struct
  type genre_objet = Personnage|Ennemi|Plateforme|Wall|Door of string|Background|Projectile
  type objet = {genre : genre_objet; position : int*int; old_pos : int*int; can_jump : bool; vitesse : float * float ; maxSpeed : float*float; pv : int;baseSize : int*int ; texture : Anim.anim; clockInv : int; clockShoot : int}
    
  let create genre_o pos vit maxvit hp textu renderer  =
    let sizeT t=
      match Sdl.query_texture t with 
      |Error (`Msg e) -> Sdl.log "Init load picture error: %s" e; exit 1
      |Ok (_,_,y) -> y
    in
    {genre = genre_o;
     position = pos;
     old_pos = pos;
     can_jump = true;
     vitesse = vit;
     maxSpeed = maxvit;
     pv = hp;
     clockInv = 0;
     clockShoot = 0;
     texture = textu; 
     baseSize = sizeT (Anim.getTexture textu); (*taille de base du personnage, utilisé lors des collision *)
    }
      
  let canShoot p = p.clockShoot = 0
  let canBeDmg p = p.clockInv = 0
  let triggerShoot p = {p with clockShoot = 5}
  let triggerInv p = {p with clockInv = 40}
  let decreaseClock p =
    {p with clockShoot = if p.clockShoot > 0 then p.clockShoot -1 else 0;
      clockInv = if p.clockInv > 0 then p.clockInv -1 else 0}

  (* fonction de debuggage*)
  let print obj = 
    let (x,y) = obj.position in 
    let (xs,ys) = obj.vitesse in
    let (xsm,ysm) = obj.maxSpeed in
    let pv = obj.pv in
    Printf.printf "Pos : %d %d \n Speed : %f %f \n MaxSpeed : %f %f \n PV : %d \n\n" x y xs ys xsm ysm pv 
      
  let kill obj = {obj with pv = 0}                   
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

  let reposition obj (x,y) (xs,ys) = {obj with position = (x,y); vitesse = (xs,ys)}

  let isDmgType obj =
    (((obj.genre) = Personnage) || ((obj.genre) = Ennemi) || ((obj.genre) = Projectile))
    
    
  let changePV obj a =
    if ((isDmgType obj) && (canBeDmg obj)) then
      begin
	match obj.genre with
	|Personnage -> let temp = triggerInv obj in
		       {temp with pv = obj.pv+a}
	|Ennemi     -> {obj with pv = obj.pv+a}
	|_          -> obj
      end
    else obj
    
  let move obj (x,y)  = {obj with position = (x,y); old_pos = obj.position}
	
  let getGenre obj = obj.genre      
  let getPos obj = obj.position
  let getOldPos obj = obj.old_pos
  let allowJump obj = {obj with can_jump = true}
  let forbidJump obj = {obj with can_jump = false}
  let canJump obj = obj.can_jump      
  let getSpeed obj = obj.vitesse
  let getMaxSpeed obj = obj.maxSpeed
  let getPV  obj = obj.pv
  let getAnim obj = obj.texture 
  let getTexture obj =  Anim.getTexture obj.texture
  let isMovable obj = if (obj.genre = Personnage) || (obj.genre = Ennemi) || (obj.genre = Projectile) then true else false
  let changeFrame obj dir = {obj with texture = Anim.changeFrame obj.texture dir}

  let getSize obj = 
    let x = Anim.getTexture obj.texture in
   match Sdl.query_texture x with 
    |Error (`Msg e) -> Sdl.log "Init load picture error: %s" e; exit 1
    |Ok (_,_,(x,y)) -> (x,y)

      
end
