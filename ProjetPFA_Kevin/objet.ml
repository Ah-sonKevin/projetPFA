open Tsdl
open Anim

module type Objet = sig
  type genre_objet = Personnage|Ennemi|Plateforme|Wall|Door|Background|Projectile
  type objet
  val create : genre_objet -> int*int -> float*float -> float*float -> int ->(string array * string array * string array * string array ) ->int*int -> Sdl.renderer -> objet
  val move : objet ->(int*int)->  objet
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
  val getPV : objet -> int
  val getSize : objet -> int*int
  val getTexture : objet -> Sdl.texture
  val isMovable : objet -> bool
 (* val getFrame : objet -> int
  *)
  val changeFrame : objet -> Anim.direction -> objet 

end

module Objet : Objet = struct
  type genre_objet = Personnage|Ennemi|Plateforme|Wall|Door|Background|Projectile
  type objet = {genre : genre_objet; position : int*int; can_jump : bool; vitesse : float * float ;
                maxSpeed : float*float; pv : int ;canBeDmg : bool*int ;size : int*int ; texture : Anim.anim }
  let create genre_o pos vit maxvit hp  (textG, textM, textD, textS) s renderer  =
    {genre = genre_o;
     position = pos;
     can_jump = true;
     vitesse = vit;
     maxSpeed = maxvit;
     pv = hp;
     canBeDmg = if ((genre_o = Personnage) || (genre_o = Ennemi)) then (true,0) else (false,(-1));
     size = s;
     texture = Anim.create textG textM textD textS renderer
    }
          
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
  let getPV  obj = obj.pv    
  let getSize obj = obj.size    
  let getTexture obj =  Anim.getTexture obj.texture
  let isMovable obj = if (obj.genre = Personnage) || (obj.genre = Ennemi) || (obj.genre = Projectile) then true else false
  let changeFrame obj dir = {obj with texture = Anim.changeFrame obj.texture dir}
    
end
