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
    let (x1,y1) = Objet.getPos obj1 in
    let (o_x1,o_y1) = Objet.getOldPos obj1 in
    let (w1,h1) = Objet.getSize obj1 in
    (* donnée de l'objet 2 *)
    let (x2,y2) = Objet.getPos obj2 in
    let (o_x2,o_y2) = Objet.getOldPos obj2 in
    let (w2,h2) = Objet.getSize obj2 in
    (* calcul du déplacement de obj1 *)
    let (xd,yd) = (x1-o_x1,y1-o_y1) in
    (*
      Gestion des 8 cas de collisions possibles (directions) :
      -------------------
      -     -     -     -
      -  5  -  2  -  6  -
      -     -     -     -
      -------------------
      -     -     -     -
      -  1  -  0  -  3  -
      -     -     -     -
      -------------------
      -     -     -     -
      -  8  -  4  -  7  -
      -     -     -     -
      -------------------
      les différentes faces du rectangle fixe : LEFT: 1 /UP: 2 /RIGHT: 3 /DOWN: 4 
    *)
    (* notre objet était t'il à droite où à gauche de l'objet avec lequel il entre en collision *)
    if xd>0
    then
      begin
	(* si il est à gauche, est-il dans les cas 5/1/8 ou alors dans les cas 2/4 *)
	if (o_x1 + w1) <= o_x2
	then
	  begin
	    (*dans le cadres des cas 5/1/8, il faut maintenant savoir précisément les différencier *)
	    if (o_y1 + h1) < o_y2
	    then
	      begin
		(* cas 5 uniquement, necessité de savoir si face 1 ou 2 rencontrée en premier -> Calcul des temps relatif avant collision *)
		if ((abs_float (float_of_int (o_x2 - (o_x1 + w1)))/.(float_of_int xd)) > (abs_float (float_of_int (o_y2 - (o_y1 + h1)))/.(float_of_int yd)))
		(* face 1 rencontre en premiere *)
		then 1
		(* face 2 rencontre en premier *)
		else 2
	      end
	    else
	      begin
		(* reste à differencier cas 8 et cas 1 *)
		if o_y1 > (o_y2 + h2)
		then
		  begin
		    (* cas 8 uniquement, necessité de savoir si face 1 ou 4 rencontrée en premier -> Calcul des temps relatif avant collision *)
		    if ((abs_float (float_of_int (o_x2 - (o_x1 + w1)))/.(float_of_int xd)) > (abs_float (float_of_int (o_y1 - (o_y2 + h2)))/.(float_of_int yd)))
		    (* face 1 rencontre en premiere *)
		    then 1
		    (* face 4 rencontre en premier *)
		    else begin Printf.printf "tbaisé 8 \n\n" ;4 end
		  end
		(* cas 1 *)
		else 1
	      end
	  end
	(* reste les cas 2 et 4 à traiter pour ce faire il suffit de savoir si l'objet movible était au dessus ou en dessous *)
	else
	  begin
	    if (o_y1 + h1) <= o_y2
	    (* cas 2 *)
	    then 2
	    (* cas 4 *)
	    else begin Printf.printf "tbaisé d->4 \n\n" ;4 end
	  end
      end
    else
      begin
	(* si il est à droite, est-il dans les cas 6/3/7 ou alors dans les cas 2/4 *)
	if o_x1 >= (o_x2 + w2)
	then
	  begin
	    (*dans le cadres des cas 6/3/7, il faut maintenant savoir précisément les différencier *)
	    if (o_y1 + h1) < o_y2
	    then
	      begin
		(* cas 6 uniquement, necessité de savoir si face 3 ou 2 rencontrée en premier -> Calcul des temps relatif avant collision *)
		if ((abs_float (float_of_int (o_x1 - (o_x2 + w2)))/.(float_of_int xd)) > (abs_float (float_of_int (o_y2 - (o_y1 + h1)))/.(float_of_int yd)))
		(* face 3 rencontre en premiere *)
		then 3
		(* face 2 rencontre en premier *)
		else 2
	      end
	    else
	      begin
		(* reste à differencier cas 7 et cas 3 *)
		if o_y1 > (o_y2 + h2)
		then
		  begin
		    (* cas 7 uniquement, necessité de savoir si face 3 ou 4 rencontrée en premier -> Calcul des temps relatif avant collision *)
		    if ((abs_float (float_of_int (o_x1 - (o_x2 + w2)))/.(float_of_int xd)) > (abs_float (float_of_int (o_y1 - (o_y2 + h2)))/.(float_of_int yd)))
		    (* face 3 rencontre en premiere *)
		    then 3
		    (* face 4 rencontre en premier *)
		    else begin Printf.printf "tbaisé 7->4 \n\n" ;4 end
		  end
		(* cas 3 *)
		else 3
	      end
	  end
	(* reste les cas 2 et 4 à traiter pour ce faire il suffit de savoir si l'objet movible était au dessus ou en dessous *)
	else
	  begin
	    if (o_y1 + h1) <= o_y2
	    (* cas 2 *)
	    then 2
	    (* cas 4 *)
	    else begin Printf.printf "tbaisé g->4 \n\n" ;4 end
	  end
      end
    
    

  let replace obj face obj_col =
    let (x,y) = Objet.getPos obj in
    let (w,h) = Objet.getBaseSize obj in
    let (xs,ys) = Objet.getSpeed obj in
    let (xCol,yCol) = Objet.getPos obj_col in
    let (wCol,hCol) = Objet.getBaseSize obj_col in
    if ((Objet.getGenre obj_col) = Plateforme)
    then
	if (face = 2)
	then Objet.allowJump (Objet.reposition obj (x,yCol-h) (xs,0.0))
	else obj
    else
      match face with
      |1 -> Objet.allowJump (Objet.reposition obj (xCol-w,y) (0.0,ys))
      |2 -> Objet.allowJump (Objet.reposition obj (x,yCol-h) (xs,0.0))
      |3 -> Objet.allowJump (Objet.reposition obj (xCol+wCol,y) (0.0,ys))
      |4 -> Objet.reposition obj (x,yCol+hCol) (xs,0.0)
      |_ -> failwith "ya que 4 face à un rectangle"
	 
  let collision_perso p obj =
    Objet.print p;
    Objet.print obj;
    match (Objet.getGenre obj) with
    |Ennemi     ->
       let (xs,ys) = Objet.getSpeed p in
       let temp = replace p (directionCollision p obj) obj in
       if abs_float(xs) < 0.8
       then Objet.changePV (Objet.setSpeed (Objet.resetSpeed temp) (2.0,-.5.0)) (-20)
       else Objet.changePV (Objet.setSpeed (Objet.resetSpeed temp) (-.xs,-.ys)) (-20)
    |Projectile -> Objet.changePV p (-20)
    (*ici mettre la gestion colision porte *)
    |_          -> replace p (directionCollision p obj) obj
       
       
  let collision_ennemi e obj =
    match Objet.getGenre obj with
    |Personnage -> e
    |Ennemi     -> e
    |Projectile -> Objet.changePV e (-20)
    |_          ->
       let(xs,ys) = Objet.getSpeed e in
       let temp = replace e (directionCollision e obj) obj in
       let(xst,yst) = Objet.getSpeed temp in
       if(xst = 0.0) then Objet.setSpeed (Objet.resetSpeed temp) (-.xs,ys) else Objet.setSpeed (Objet.resetSpeed temp) (xs,0.0)
		     
		     
  let collision_projectile proj =
    Objet.kill proj
      
  let collision obj1 obj2 =
    if checkCollision obj1 obj2 then
      match Objet.getGenre obj1 with
      |Personnage -> collision_perso obj1 obj2 
      |Ennemi -> collision_ennemi obj1 obj2 
      |Projectile -> collision_projectile obj1
      |_ -> obj1
    else obj1

end
