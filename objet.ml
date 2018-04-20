open Tsdl
open Anim

module type Objet = sig
    
  type kind = Normal | Fly | Shooter | Both
  type typeUp = HP|Inv
  type genre_objet = Personnage|Ennemi of kind|Plateforme of int * int |Wall of int * int|Door of string |Background|Projectile |PowerUp of typeUp
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
  val getMaxSpeed : objet -> float * float
  val getAnim : objet -> Anim.anim
  val kill : objet -> objet
  val canShoot : objet -> bool
  val canBeDmg : objet -> bool 
  val triggerShoot : objet -> int -> objet
  val triggerInv : objet -> objet
  val triggerInvPU : objet -> objet
  val decreaseClock : objet -> objet
  val getPvMax : objet -> int
  val clignote : objet -> bool
end
 
module Objet : Objet = struct
  type kind = Normal | Fly | Shooter | Both
  type typeUp = HP|Inv
  type genre_objet = Personnage|Ennemi of kind|Plateforme of int * int |Wall of int * int|Door of string |Background|Projectile |PowerUp of typeUp
  type objet = {genre : genre_objet; position : int*int; old_pos : int*int; can_jump : bool; vitesse : float * float ; maxSpeed : float*float; pv : int; pvMax : int;
	baseSize : int*int ; texture : Anim.anim; clockInv : int; clockShoot : int}
    
  let create genre_o pos vit maxvit hp textu renderer  =
    let sizeT t=
      match Sdl.query_texture t with 
      |Error (`Msg e) -> Sdl.log "Init load picture error: %s" e; exit 1
      |Ok (_,_,y) -> y
    in
    {genre = genre_o; position = pos; old_pos = pos;can_jump = true;
     vitesse = vit; maxSpeed = maxvit; pv = hp; pvMax = hp; clockInv = 0;
     clockShoot = 0; texture = textu; baseSize =
	match genre_o with
	|Plateforme (x,y) | Wall (x,y) -> (x,y)
	|_->sizeT (Anim.getTexture textu); (*taille de base de l'objet, utilisé lors des collision *)
    }
	
  let getGenre obj = obj.genre 
  let getPos obj = obj.position
  let getOldPos obj = obj.old_pos
  let getPV obj = obj.pv
  let getPvMax obj = obj.pvMax
  let getAnim obj = obj.texture
  let allowJump obj = {obj with can_jump = true}
  let forbidJump obj = {obj with can_jump = false}
  let canJump obj = obj.can_jump 
  let getSpeed obj = obj.vitesse
  let getMaxSpeed obj = obj.maxSpeed
  let getBaseSize obj = obj.baseSize 
  let getTexture obj = Anim.getTexture obj.texture
  let getSize obj =
    match obj.genre  with
    |Plateforme (w,h) | Wall (w,h) -> (w,h)
    |_                            ->
       begin
	 let x = Anim.getTexture obj.texture in
	 match Sdl.query_texture x with 
	 |Error (`Msg e) -> Sdl.log "Init load picture error: %s" e; exit 1
	 |Ok (_,_,(x,y)) -> (x,y)
       end
    
  let canShoot p = p.clockShoot = 0
  let canBeDmg p = p.clockInv = 0
  let clignote p = (p.clockInv mod 2) = 0 
  let triggerShoot p n = {p with clockShoot = n}
  let triggerInv p = {p with clockInv = 40}
  let triggerInvPU p = {p with clockInv = 300}
  let decreaseClock p =
    {p with clockShoot = if p.clockShoot > 0 then p.clockShoot -1 else 0;
      clockInv = if p.clockInv > 0 then p.clockInv -1 else 0}

  let move obj (x,y)  = {obj with position = (x,y); old_pos = obj.position}
  let changeFrame obj dir = {obj with texture = Anim.changeFrame obj.texture dir}
  let kill obj = {obj with pv = 0}
  let resetSpeed obj = {obj with vitesse =(0.0,0.0)}
  let reposition obj (x,y) (xs,ys) = {obj with position = (x,y); vitesse = (xs,ys)}


  let isDmgType obj =
    match obj.genre with
    |Personnage |Ennemi _ |Projectile -> true
    |_                                -> false     

  let isMovable obj =
    match obj.genre with
    |Personnage |Ennemi _ |Projectile |PowerUp _ -> true
    |_                                           -> false

       
  let changePV obj a =
    if ((isDmgType obj) && (canBeDmg obj)) then
      begin
	match obj.genre with
	|Personnage -> let temp = triggerInv obj in
		       {temp with pv = obj.pv+a}
	|Ennemi _   -> {obj with pv = obj.pv+a}
	|_          -> obj
      end
    else obj 
                          
  let setSpeed obj (x,y) =
    let (x1,y1) = obj.vitesse in
    let (msx,msy) = obj.maxSpeed in
    let signe = x *. x1 in
    let interx = if (signe >= 0.0) then x+.x1 else 0.0 in
    let intery = if (y < 0.0) && (obj.can_jump = true) then y else y+.y1 in
    let finalx = if x+.x1 >= msx  then msx else if interx <= -.msx then -.msx else interx in
    let finaly = if y+.y1 >= msy  then msy else if intery <= -.msy then -.msy else intery in
    {obj with vitesse = (finalx,finaly)}       
      
end
