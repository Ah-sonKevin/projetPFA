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

module Objet : Objet = struct
  type genre_objet = Personnage|Ennemi|Plateforme|Wall|Door|Background
  type objet = {genre : genre_objet;   position : int*int; vitesse : int * int ; maxSpeed : int*int; pv : int ;size : int*int ;  path : string ; texture : Sdl.texture}
  let create genre_o pos vit maxvit hp  text s renderer  =
    let x = 
       match Sdl.load_bmp text with
       | Error (`Msg e) -> Sdl.log "Init load picture error: %s" e; exit 1
       | Ok surface_temp ->
	  match Sdl.create_texture_from_surface renderer surface_temp with
	  | Error (`Msg e) -> Sdl.log "Init surface to texture error: %s" e; exit 1
	  | Ok name -> Sdl.free_surface surface_temp;
	     name
    in
    {genre = genre_o;
     position = pos;
     vitesse = vit;
     maxSpeed = maxvit;
     pv = hp;
     path = text;
     size = s;
     texture = x
    }

  let calcul_pos obj =    
    let (x,y)=obj.position in
    let (vx,vy) = obj.vitesse in
    (x+vx,y+vy)
          
  let setSpeed obj (x,y) =
    let (x1,y1) = obj.vitesse in
    let (msx,msy) = obj.maxSpeed in
    let finalx = if x+x1 >= msx  then msx else if x+x1 <= -msx then -msx else x+x1 in
    let finaly = if y+y1 >= msy  then msy else if y+y1 <= -msy then -msy else y+y1 in
    {obj with vitesse =(finalx,finaly)}			       
      
  let changePV obj a = {obj with pv = obj.pv+a }
    
  let move obj  =
    let (xs,ys) = obj.vitesse in
    let (xp,yp) = obj.position in
    {obj with position = (xp+xs,yp+ys)}

  let getNextPosition obj =
    let (xs,ys) = obj.vitesse in
    let (xp,yp) = obj.position in
    (xp+xs,yp+ys)

  let getHitBox obj =
    let (x,y) = obj.position in
    let (w,h) = obj.size in
    (x,y,w,h)
	
  let getGenre obj = obj.genre
      
  let getPos  obj = obj.position
    
  let getPV  obj = obj.pv
    
  let getSize obj = obj.size
    
  let getPath obj = obj.path
    
  let getTexture obj = obj.texture
    
end
