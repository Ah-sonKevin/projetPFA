open Tsdl
open Objet

module type Collision  = sig
  val checkCollision : Objet.objet -> Objet.objet -> bool
  val directionCollision : Objet.objet -> Objet.objet -> int
  val replace : Objet.objet -> int -> Objet.objet -> Objet.objet
  val collision_perso : Objet.objet -> Objet.objet -> Objet.objet
  val collision_ennemi : Objet.objet -> Objet.objet -> Objet.objet
  val collision_projectile : Objet.objet -> Objet.objet
  val collision : Objet.objet -> Objet.objet -> Objet.objet
end

module Collision : Collision = struct

  let checkCollision obj1 obj2 =
    let (x1,y1) = Objet.getPos obj1 in
    let (w1,h1) = Objet.getBaseSize obj1 in
    let rect1 = Sdl.Rect.create x1 y1 w1 h1 in
    let (x2,y2) = Objet.getPos obj2 in
    let (w2,h2) = Objet.getBaseSize obj2 in
    let rect2 = Sdl.Rect.create x2 y2 w2 h2 in
    Sdl.has_intersection rect1 rect2
    
  let directionCollision obj1 obj2 =
    (* donnée de l'objet 1 *)
    let (o_x1,o_y1) = Objet.getOldPos obj1 in
    let (w1,h1) = Objet.getSize obj1 in
    (* donnée de l'objet 2 *)
    let (o_x2,o_y2) = Objet.getOldPos obj2 in
    let (w2,h2) = Objet.getSize obj2 in
    (*
      La detection de collision étant faite avant cette étape, il ne nous reste plus qu'à vérifier l'axe de collision 
      Gestion des 8 cas de collisions possibles (directions) :
      -------------------
      -     -     -     -
      -  5  -  2  -  6  -
      -     -     -     -
      -------------------
      -     -     -     -
      -  1  -     -  3  -
      -     -     -     -
      -------------------
      -     -     -     -
      -  8  -  4  -  7  -
      -     -     -     -
      -------------------
      les différentes faces du rectangle rencontré : 
      LEFT: 1 /UP: 2 /RIGHT: 3 /DOWN: 4 / Diag UP-LEFT : 5 / Diag UP-RIGHT : 6 / Diag DOWN-RIGHT : 7 / Diag DOWN-LEFT : 8 
    *)
    (* on check si l'obj 1 etait au dessus de l'obj 2 *)
    if(o_y1+h1) <= o_y2
    then
      (* si oui, alors on check si il était à gauche de l'obj 2 --> Diag UP-LEFT *)
      if (o_x1+w1) < o_x2 then 5 else
	(* sinon, alors on check si il était à droite, si oui --> Diag UP-RIGHT, si non --> UP *)
	if o_x1 > (o_x2+w2) then 6 else 2
    else
      (*si l'objet n'est pas au dessus on check si il est en dessous *)
      if o_y1 >= (o_y2+h2) then
	(* si oui, alors on check si il était à gauche de l'obj 2 --> Diag DOWN-LEFT *)
	if (o_x1+w1) < o_x2 then 8 else
	  (* sinon, alors on check si il était à droite, si oui --> Diag DOWN-RIGHT, si non --> DOWN *)
	  if o_x1 > (o_x2+w2) then 7 else 4
      else
	(* si l'objet n'était ni au dessus, ni au dessous, alors il est sur les cotés : soit --> LEFT / soit --> RIGHT *)
	if (o_x1+w1) <= o_x2 then 1 else 3
	  
  let replace obj face obj_col =
    let (x,y) = Objet.getPos obj in
    let (w,h) = Objet.getBaseSize obj in
    let (xs,ys) = Objet.getSpeed obj in
    let (xCol,yCol) = Objet.getPos obj_col in
    let (wCol,hCol) = Objet.getBaseSize obj_col in
    match ((Objet.getGenre obj_col)) with
    |Plateforme _  ->
       if (face = 2 || face = 5 || face = 6)
       then Objet.allowJump (Objet.reposition obj (x,yCol-h) (xs,0.0))
       else obj
    | _ ->
      match face with
      |1 -> Objet.allowJump (Objet.reposition obj (xCol-w,y) (0.0,ys))
      |2 -> Objet.allowJump (Objet.reposition obj (x,yCol-h) (xs,0.0))
      |3 -> Objet.allowJump (Objet.reposition obj (xCol+wCol,y) (0.0,ys))
      |4 -> Objet.reposition obj (x,yCol+hCol) (xs,0.0)
      |5 -> Objet.allowJump (Objet.reposition obj (xCol-w,yCol-h) (0.0,0.0))
      |6 -> Objet.allowJump (Objet.reposition obj (xCol+wCol,yCol-h) (0.0,0.0))
      |7 -> Objet.reposition obj (xCol+wCol,yCol+hCol) (0.0,0.0)
      |8 -> Objet.reposition obj (xCol-w,yCol+hCol) (0.0,0.0)
      |_ -> failwith "ya que 8 cas normalement"
	 
  let collision_perso p obj =
    match (Objet.getGenre obj) with
    |Ennemi _   ->
       Sound.play_sound Degat son;
       if (Objet.canBeDmg p) then
	 let (xs,ys) = Objet.getSpeed p in
	 let (xse,yse) = Objet.getSpeed obj in
	 let temp = replace p (directionCollision p obj) obj in
	 if abs_float(xs) < 0.8 then
	   Objet.changePV (Objet.setSpeed (Objet.resetSpeed temp) ((xse*.3.0),(-.6.0))) (-40)
	 else
	   Objet.changePV (Objet.setSpeed (Objet.resetSpeed temp) ((-.xs+.(xse*.0.5)),(-.ys+.(yse*.0.5)-.6.0))) (-40)
       else p
    |Projectile -> Objet.changePV p (-20)
    |Door t     -> p
    |PowerUp HP ->      
	 let x = if (Objet.getPV p) +50 <= (Objet.getPvMax p ) then 50 else (Objet.getPvMax p) - (Objet.getPV p)
		   in Objet.changePV p x
    |PowerUp Inv -> Objet.triggerInv p 
    |_          -> replace p (directionCollision p obj) obj
       
       
  let collision_ennemi e obj =
    match Objet.getGenre obj with
    |Personnage -> e
    |Ennemi _   -> e
    |Projectile -> Objet.changePV e (-20)
    |_          ->
       let(xs,ys) = Objet.getSpeed e in
       let temp = replace e (directionCollision e obj) obj in
       let(xst,yst) = Objet.getSpeed temp in
       if(xst = 0.0) then Objet.setSpeed (Objet.resetSpeed temp) (-.xs,ys) else Objet.setSpeed (Objet.resetSpeed temp) (xs,0.0)
		     
		     
  let collision_projectile proj =
    Objet.kill proj

  let collision_powerUp obj1 obj2 son =
    Sound.play_sound PowerUp son;
    if (Objet.getGenre obj2) = Personnage then
      Objet.kill obj1
    else
      obj1
      
  let collision obj1 obj2 son =
    if checkCollision obj1 obj2 then
      match Objet.getGenre obj1 with
      |Personnage -> collision_perso obj1 obj2 son    
      |Ennemi _   -> collision_ennemi obj1 obj2 
      |Projectile -> collision_projectile obj1
      |PowerUp _  -> collision_powerUp obj1 obj2 son
      |_          -> obj1
    else obj1

end
