open Tsdl
open Objet

module type Collision  = sig 
  val kickObject    : Objet.objet -> Objet.objet list -> Objet.objet list
  val kickObjectWithProj    : Objet.objet -> Objet.objet list -> Objet.objet list -> Objet.objet list
  val kickObjectWithMovable    : Objet.objet -> Objet.objet list -> Objet.objet list -> Objet.objet list
  val hit_boxObjet : Objet.objet -> Objet.objet list -> (int * int)-> (int*int)  -> ((int * int) * (float * float)) option
  val hit_boxPerso : Objet.objet -> Objet.objet list -> (int * int)-> (int*int)  -> (Objet.objet * (int * int) * (float * float)) option
  val hit_boxProj : Objet.objet -> Objet.objet list -> float-> (int*int) -> (Objet.objet) option
end

module Collision : Collision = struct 
    
  (* méthode pour obtenir la liste des objets de la scene sans l'objet traité (évite self collision) *)
  let kickObject obj list = 
    let rec kickObjetRec obj list reslist =
      match list with
      |[] -> reslist
      |x::s -> if x = obj then kickObjetRec obj s reslist else kickObjetRec obj s (x::reslist)
    in
    kickObjetRec obj list []

     (* méthode pour obtenir la liste des objets de la scene sans l'objet traité (évite self collision) et sans les projectiles (normalement déjà traité)*)
  let rec kickObjectWithProj obj list reslist=
    match list with
    |[] -> reslist
    |x::s when x = obj -> kickObjectWithProj obj s reslist
    |x::s when ((Objet.getGenre x)=(Projectile)) -> kickObjectWithProj obj s reslist
    |x::s -> kickObjectWithProj obj s (x::reslist) 
      
  (* méthode pour obtenir la liste des objets de la scene sans l'objet traité (évite self collision) et sans les objets mobiles (normalement déjà traité)*)
  let rec kickObjectWithMovable obj list reslist=
    match list with
    |[] -> reslist
    |x::s when x = obj -> kickObjectWithMovable obj s reslist
    |x::s when (Objet.isMovable x) -> kickObjectWithMovable obj s reslist
    |x::s -> kickObjectWithMovable obj s (x::reslist)
       
  let hit_boxObjet obj list nextP (sizeX,sizeY)  =
    (* éléments nécessaire pour les calculs sur l'objet traité *)
    let (xm,ym) = Objet.getPos obj in
    let (wm2,hm2) = Objet.getSize obj in
    let (wm,hm) = Objet.getBaseSize obj in 
    let (xs,ys) = Objet.getSpeed obj in
    (* calcul de la position future *)
    let (xf,yf) = nextP in 
    (* création d'un rectangle correspondant à l'objet traité dans le "future" *)
    let rectObjet = Sdl.Rect.create xf yf wm2 hm2 in
    (* méthode de calcul des hit box *)
    let rec subHit list =
      (* calcul des collisions avec les bords de la scene, on regarde si l'objet est "sorti" de la scene*)
      if (((xf-wm2) < 0) || ((yf-hm2) < 0) || (xf>sizeX) || (yf>sizeY))
      then
	begin
	  Some ((0,0),(1000.0,0.0))
	end
      else
	begin
	  match list with
	  |[] -> None
	  |x::s when ((Objet.getGenre x)=(Plateforme)) ->
             (* éléments nécessaire pour les calculs sur le deuxieme objet traité *)
             let (xt,yt) = Objet.getPos x in
             let (wt,ht) = Objet.getSize x in
             let rectTemp = Sdl.Rect.create  xt yt wt ht in
             (* gestion des collisions pour les plateformes *)
             if ((Sdl.has_intersection rectObjet rectTemp)&&((ym+hm) <= yt))  then
               begin
		 (*calcul du déplacement de notre objet*)
		 let (xd,yd) = (xf-xm , yf-ym) in
		 (* Gestion des 3 cas de collisions possibles pour une plateforme (directions) :
                    -------------------
                    -     -     -     -
                    -  5  -  2  -  6  -
                    -     -     -     -
                    -------------------
                    -     -     -     -
                    -  1  -  0  -  3  -
                    -     -     -     -
                    -------------------
		    les différentes faces du rectangle fixe : LEFT: 1 /UP: 2 /RIGHT: 3 /DOWN: 4 
		 *)
		 (* cas 5 et 6, necessité de savoir si face 2 ou 1/3 rencontrée en premier -> Calcul des temps relatif avant collision *)
		 if (((abs_float (float_of_int (xt - (xm + wm)))/.float_of_int xd)) < (abs_float (float_of_int (yt - (ym + hm)))/.(float_of_int yd)))
		 (* face 2 rencontre en premier *)
		 then Some (((xf,(yt-hm))),(xs,0.0))
		 (* soit cas 2 soit rien *)
		 else
		   begin
                     if(((xm + wm) > xt) && (xm < (xt + wt)))
                     then Some ((xf,(yt-hm)),(xs,0.0))
                     else subHit s
		   end
               end
             else subHit s
	  |x::s when (Objet.isMovable x) -> None
	  |x::s -> 
             (* éléments nécessaire pour les calculs sur le deuxieme objet traité *)
             let (xt,yt) = Objet.getPos x in
             let (wt,ht) = Objet.getSize x in
             let rectTemp = Sdl.Rect.create  xt yt wt ht in
             (* gestion des collisions pour les objets autres que les plateformes/ennemis *)
             if Sdl.has_intersection rectObjet rectTemp
             then
               begin
		 (*calcul du déplacement de notre objet*)
		 let (xd,yd) = (xf-xm , yf-ym) in
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
		 (* notre objet en mouvement était t'il à droite où à gauche de l'objet avec lequel il entre en collision *)
		 if xd>0
		 then
		   begin
                     (* si il est à gauche, est-il dans les cas 5/1/8 ou alors dans les cas 2/4 *)
                     if (xm + wm) <= xt
                     then
                       begin
			 (*dans le cadres des cas 5/1/8, il faut maintenant savoir précisément les différencier *)
			 if (ym + hm) < yt
			 then
			   begin
                             (* cas 5 uniquement, necessité de savoir si face 1 ou 2 rencontrée en premier -> Calcul des temps relatif avant collision *)
                             if ((abs_float (float_of_int (xt - (xm + wm)))/.(float_of_int xd)) > (abs_float (float_of_int (yt - (ym + hm)))/.(float_of_int yd)))
                             (* face 1 rencontre en premiere *)
                             then Some ((((xt-wm),yf)),(0.0,ys))
                             (* face 2 rencontre en premier *)
                             else Some (((xf,(yt-hm))),(xs,0.0))
			   end
			 else
			   begin
                             (* reste à differencier cas 8 et cas 1 *)
                             if ym > (yt + ht)
                             then
                               begin
				 (* cas 8 uniquement, necessité de savoir si face 1 ou 4 rencontrée en premier -> Calcul des temps relatif avant collision *)
				 if ((abs_float (float_of_int (xt - (xm + wm)))/.(float_of_int xd)) > (abs_float (float_of_int (ym - (yt + ht)))/.(float_of_int yd)))
				 (* face 1 rencontre en premiere *)
				 then Some ((((xt-wm),yf)),(0.0,ys))
				 (* face 4 rencontre en premier *)
				 else Some (((xf,(yt+ht))),(xs,0.0))
                               end
                             (* cas 1 *)
                             else Some ((((xt-wm),yf)),(0.0,ys))
			   end
                       end
                     (* reste les cas 2 et 4 à traiter pour ce faire il suffit de savoir si l'objet movible était au dessus ou en dessous *)
                     else
                       begin
			 if (ym + hm) <= yt
			 (* cas 2 *)
			 then Some (((xf,(yt-hm))),(xs,0.0))
			 (* cas 4 *)
			 else Some (((xf,(yt+ht))),(xs,0.0))
                       end
		   end
		 else
		   begin
                     (* si il est à droite, est-il dans les cas 6/3/7 ou alors dans les cas 2/4 *)
                     if xm >= (xt + wt)
                     then
                       begin
			 (*dans le cadres des cas 6/3/7, il faut maintenant savoir précisément les différencier *)
			 if (ym + hm) < yt
			 then
			   begin
                             (* cas 6 uniquement, necessité de savoir si face 3 ou 2 rencontrée en premier -> Calcul des temps relatif avant collision *)
                             if ((abs_float (float_of_int (xm - (xt + wt)))/.(float_of_int xd)) > (abs_float (float_of_int (yt - (ym + hm)))/.(float_of_int yd)))
                             (* face 3 rencontre en premiere *)
                             then Some ((((xt+wt),yf)),(0.0,ys))
                             (* face 2 rencontre en premier *)
                             else Some (((xf,(yt-hm))),(xs,0.0))
			   end
			 else
			   begin
                             (* reste à differencier cas 7 et cas 3 *)
                             if ym > (yt + ht)
                             then
                               begin
				 (* cas 7 uniquement, necessité de savoir si face 3 ou 4 rencontrée en premier -> Calcul des temps relatif avant collision *)
				 if ((abs_float (float_of_int (xm - (xt + wt)))/.(float_of_int xd)) > (abs_float (float_of_int (ym - (yt + ht)))/.(float_of_int yd)))
				 (* face 3 rencontre en premiere *)
				 then Some ((((xt+wt),yf)),(0.0,ys))
				 (* face 4 rencoantre en premier *)
				 else Some (((xf,(yt+ht))),(xs,0.0))
                               end
                             (* cas 3 *)
                             else Some ((((xt+wt),yf)),(0.0,ys))
			   end
                       end
                     (* reste les cas 2 et 4 à traiter pour ce faire il suffit de savoir si l'objet movible était au dessus ou en dessous *)
                     else
                       begin
			 if (ym + hm) <= yt
			 (* cas 2 *)
			 then Some (((xf,(yt-hm))),(xs,0.0))
			 (* cas 4 *)
			 else Some (((xf,(yt+ht))),(xs,0.0))
                       end
		   end
               end
             else subHit s
	end
    in
    subHit (kickObjectWithMovable obj list [])
      
  let hit_boxPerso obj list nextP (sizeX,sizeY)  =
    print_string "Debut Collision " ;
    (* éléments nécessaire pour les calculs sur l'objet traité *)
    let (xm,ym) = Objet.getPos obj in
    let (wm,hm) = Objet.getSize obj in
    let (xs,ys) = Objet.getSpeed obj in
    (* calcul de la position future *)
    let (xf,yf) = nextP in
    (* creation d'un rectangle correspondant à l'objet traité maintenant *)
    let rectObjetPre = Sdl.Rect.create xm ym wm hm in
    (* création d'un rectangle correspondant à l'objet traité dans le "future" *)
    let rectObjet = Sdl.Rect.create xf yf wm hm in
    (* méthode de calcul des hit box *)
    let rec subHit list =
      (* calcul des collisions avec les bords de la scene, on regarde si l'objet est "sorti" de la scene*)
      if (((xf-wm) < 0) || ((yf-hm) < 0) || (xf>sizeX) || (yf>sizeY))
      then
	begin
	  Some (obj,(0,0),(1000.0,0.0))
	end
      else
	begin
	  match list with
	  |[] -> None
	  |x::s when ((Objet.getGenre x)=(Ennemi))->
	     (* éléments nécessaire pour les calculs sur le deuxieme objet traité *)
	     let (xt,yt) = Objet.getPos x in
	     let (wt,ht) = Objet.getSize x in
	     (* let (xst,yst) = Objet.getSpeed x in *)
	     let rectTemp = Sdl.Rect.create  xt yt wt ht in
	        Printf.printf "\nCoordonné %d %d \n" xm (xt+wt);
	     (* gestion des collisions *)
	     (* gestion des collisions pour les objets "Ennemi" *)
             if (Sdl.has_intersection rectObjet rectTemp || Sdl.has_intersection rectObjetPre rectTemp)
             then
               begin
		 Printf.printf "debut gestion ennemi \n";
		 Printf.printf "coord ennemi : %d %d %d %d \n" xt yt wt ht;
		 (*calcul du déplacement de notre objet*)
		 let (xd,yd) = (xf-xm , yf-ym) in
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
		 (* notre objet en mouvement était t'il à droite où à gauche de l'objet avec lequel il entre en collision *)
		 if xd>0 then
		   begin
                     (* si il est à gauche, est-il dans les cas 5/1/8 ou alors dans les cas 2/4 *)
                     if (xm + wm) <= xt (*************)
                     then
                       begin
			 (*dans le cadres des cas 5/1/8, il faut maintenant savoir précisément les différencier *)
			 if (ym + hm) < yt
			 then
			   begin
			     print_string "cas 5";
                             (* cas 5 uniquement, necessité de savoir si face 1 ou 2 rencontrée en premier -> Calcul des temps relatif avant collision *)
                             if ((abs_float (float_of_int (xt - (xm + wm)))/.(float_of_int xd)) > (abs_float (float_of_int (yt - (ym + hm)))/.(float_of_int yd)))
                             (* face 1 rencontre en premiere *)
                             then Some (x,(((xt-wm),yf)),((0.0-.5.0),(ys-.10.0)))
                             (* face 2 rencontre en premier *)
                             else Some (x,((xf,(yt-hm))),((xs-.5.0),(0.0-.10.0)))
			   end
			 else
			   begin
                             (* reste à differencier cas 8 et cas 1 *)
                             if ym > (yt + ht)
                             then
                               begin
				 print_string "cas 8 ";
				 (* cas 8 uniquement, necessité de savoir si face 1 ou 4 rencontrée en premier -> Calcul des temps relatif avant collision *)
				 if ((abs_float (float_of_int (xt - (xm + wm)))/.(float_of_int xd)) > (abs_float (float_of_int (ym - (yt + ht)))/.(float_of_int yd)))
				 (* face 1 rencontre en premiere *)
				 then Some (x,(((xt-wm),yf)),((0.0-.5.0),(ys-.10.0)))
				 (* face 4 rencontre en premier *)
				 else Some (x,((xf,(yt+ht))),((xs-.5.0),(0.0+.10.0)))
                               end
                             (* cas 1 *)				 
                             else begin print_string "cas 1" ; Some (x,(((xt-wm),yf)),((0.0-.5.0),(ys-.10.0)))end 
			   end
                       end
                     (* reste les cas 2 et 4 à traiter pour ce faire il suffit de savoir si l'objet movible était au dessus ou en dessous *)
                     else
                       begin
			 (* normalement le bon test à faire est : (ym + hm) <= yt mais suite à un bug incompris, on a bidouillé*)
			 if ym <= (yt+ht)
			 (* cas 2 *)
			 then  begin print_string "cas 2";Some (x,((xf,(yt-hm))),((xs-.5.0),(0.0-.10.0)))end
			 (* cas 4 *)
<<<<<<< HEAD
			 else begin print_string "cas 4 " ; Some (x,((xf,(yt+ht))),((xs-.5.0),(0.0+.10.0)))end
=======
			 else begin Printf.printf "cas g -->  4 %d %d %d %d \n" xt (xm+wm) yt (ym+hm); Some (x,((xf,(yt+ht))),((xs-.5.0),(0.0+.10.0)))end
>>>>>>> dcf27abdccff8e160ce7c6154bc15513a9fe8b05
                       end
		   end
		 else
		   begin
		     Printf.printf "\nCoordonné %d %d \n" xm (xt+wt);
                     (* si il est à droite, est-il dans les cas 6/3/7 ou alors dans les cas 2/4 *)
                     if xm >= (xt + wt)  then begin print_string "droite "; None end
                      (* begin
			 (*dans le cadres des cas 6/3/7, il faut maintenant savoir précisément les différencier *)
			 if (ym + hm) < yt
			 then
			   begin
                             (* cas 6 uniquement, necessité de savoir si face 3 ou 2 rencontrée en premier -> Calcul des temps relatif avant collision *)
			     print_string "cas 6 ";
                             if ((abs_float (float_of_int (xm - (xt + wt)))/.(float_of_int xd)) > (abs_float (float_of_int (yt - (ym + hm)))/.(float_of_int yd)))
                             (* face 3 rencontre en premiere *)
                             then Some (x,(((xt+wt),yf)),((0.0+.5.0),(ys-.10.0)))
                             (* face 2 rencontre en premier *)
                             else Some (x,((xf,(yt-hm))),((xs+.5.0),(0.0-.10.0)))
			   end
			 else
			   begin
                             (* reste à differencier cas 7 et cas 3 *)
                             if ym >= (yt + ht)
                             then
                               begin
				 (* cas 7 uniquement, necessité de savoir si face 3 ou 4 rencontrée en premier -> Calcul des temps relatif avant collision *)
				 print_string "cas 7 ";
				 if ((abs_float (float_of_int (xm - (xt + wt)))/.(float_of_int xd)) > (abs_float (float_of_int (ym - (yt + ht)))/.(float_of_int yd)))
				 (* face 3 rencontre en premiere *)
				 then Some (x,(((xt+wt),yf)),((0.0+.5.0),(ys-.10.0)))
				 (* face 4 rencoantre en premier *)
				 else  begin Printf.printf "cas 7 --> 4 \n" ;Some (x,((xf,(yt+ht))),((xs+.5.0),(0.0+.10.0)))end
                               end
<<<<<<< HEAD
                             (* cas 3 *)				 
                             else begin print_string "cas 3 "; Printf.printf "   cas 7 --> 3" ; Some (x,(((xt+wt),yf)),((0.0+.5.0),(ys-.10.0))) end
=======
                             (* cas 3 *)
                             else begin Printf.printf "cas 7 --> 3 \n" ; Some (x,(((xt+wt),yf)),((0.0+.5.0),(ys-.10.0))) end
>>>>>>> dcf27abdccff8e160ce7c6154bc15513a9fe8b05
			   end
                       end
                     (* reste les cas 2 et 4 à traiter pour ce faire il suffit de savoir si l'objet movible était au dessus ou en dessous *)
		      *)
                     else
                       begin
			 (* normalement le bon test à faire est : (ym + hm) <= yt mais suite à un bug incompris, on a bidouillé*)
			 if ym <= (yt+ht)
			 (* cas 2 *)
			 then begin print_string "cas 2 ";Some (x,((xf,(yt-hm))),((xs+.5.0),(0.0-.10.0)))end 
			 (* cas 4 *)
			 else begin Printf.printf "cas d -->  4 %d %d %d %d \n" xm (xt+wt) yt (ym+hm) ;Some (x,((xf,(yt+ht))),((xs+.5.0),(0.0+.10.0)))end
                       end
		   end
               end
             else subHit s
	  |x::s when ((Objet.getGenre x)=(Plateforme)) ->
         (* éléments nécessaire pour les calculs sur le deuxieme objet traité *)
             let (xt,yt) = Objet.getPos x in
             let (wt,ht) = Objet.getSize x in
             let rectTemp = Sdl.Rect.create  xt yt wt ht in
             (* gestion des collisions pour les plateformes *)
             if ((Sdl.has_intersection rectObjet rectTemp)&&((ym+hm) <= yt))  then
               begin
		 (*calcul du déplacement de notre objet*)
		 let (xd,yd) = (xf-xm , yf-ym) in
		 (* Gestion des 3 cas de collisions possibles pour une plateforme (directions) :
                    -------------------
                    -     -     -     -
                    -  5  -  2  -  6  -
                    -     -     -     -
                    -------------------
                    -     -     -     -
                    -  1  -  0  -  3  -
                    -     -     -     -
                    -------------------
		    les différentes faces du rectangle fixe : LEFT: 1 /UP: 2 /RIGHT: 3 /DOWN: 4 
		 *)
		 (* cas 5 et 6, necessité de savoir si face 2 ou 1/3 rencontrée en premier -> Calcul des temps relatif avant collision *)
		 if (((abs_float (float_of_int (xt - (xm + wm)))/.float_of_int xd)) < (abs_float (float_of_int (yt - (ym + hm)))/.(float_of_int yd)))
		 (* face 2 rencontre en premier *)
		 then Some (x,((xf,(yt-hm))),(xs,0.0))
		 (* soit cas 2 soit rien *)
		 else
		   begin
                     if(((xm + wm) > xt) && (xm < (xt + wt)))
                     then Some (x,(xf,(yt-hm)),(xs,0.0))
                     else subHit s
		   end
               end
             else subHit s
	  |x::s -> 
             (* éléments nécessaire pour les calculs sur le deuxieme objet traité *)
             let (xt,yt) = Objet.getPos x in
             let (wt,ht) = Objet.getSize x in
             let rectTemp = Sdl.Rect.create  xt yt wt ht in
             (* gestion des collisions pour les objets autres que les plateformes/ennemis *)
             if Sdl.has_intersection rectObjet rectTemp
             then
               begin
		 (*calcul du déplacement de notre objet*)
		 let (xd,yd) = (xf-xm , yf-ym) in
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
		 (* notre objet en mouvement était t'il à droite où à gauche de l'objet avec lequel il entre en collision *)
		 if xd>0
		 then
		   begin
                     (* si il est à gauche, est-il dans les cas 5/1/8 ou alors dans les cas 2/4 *)
                     if (xm + wm) <= xt
                     then
                       begin
			 (*dans le cadres des cas 5/1/8, il faut maintenant savoir précisément les différencier *)
			 if (ym + hm) < yt
			 then
			   begin
                             (* cas 5 uniquement, necessité de savoir si face 1 ou 2 rencontrée en premier -> Calcul des temps relatif avant collision *)
                             if ((abs_float (float_of_int (xt - (xm + wm)))/.(float_of_int xd)) > (abs_float (float_of_int (yt - (ym + hm)))/.(float_of_int yd)))
                             (* face 1 rencontre en premiere *)
                             then Some (x,(((xt-wm),yf)),(0.0,ys))
                             (* face 2 rencontre en premier *)
                             else Some (x,((xf,(yt-hm))),(xs,0.0))
			   end
			 else
			   begin
                             (* reste à differencier cas 8 et cas 1 *)
                             if ym > (yt + ht)
                             then
                               begin
				 (* cas 8 uniquement, necessité de savoir si face 1 ou 4 rencontrée en premier -> Calcul des temps relatif avant collision *)
				 if ((abs_float (float_of_int (xt - (xm + wm)))/.(float_of_int xd)) > (abs_float (float_of_int (ym - (yt + ht)))/.(float_of_int yd)))
				 (* face 1 rencontre en premiere *)
				 then Some (x,(((xt-wm),yf)),(0.0,ys))
				 (* face 4 rencontre en premier *)
				 else Some (x,((xf,(yt+ht))),(xs,0.0))
                               end
                             (* cas 1 *)
                             else Some (x,(((xt-wm),yf)),(0.0,ys))
			   end
                       end
                     (* reste les cas 2 et 4 à traiter pour ce faire il suffit de savoir si l'objet movible était au dessus ou en dessous *)
                     else
                       begin
			 if (ym + hm) <= yt
			 (* cas 2 *)
			 then Some (x,((xf,(yt-hm))),(xs,0.0))
			 (* cas 4 *)
			 else Some (x,((xf,(yt+ht))),(xs,0.0))
                       end
		   end
		 else
		   begin
                     (* si il est à droite, est-il dans les cas 6/3/7 ou alors dans les cas 2/4 *)
                     if xm >= (xt + wt)
                     then
                       begin
			 (*dans le cadres des cas 6/3/7, il faut maintenant savoir précisément les différencier *)
			 if (ym + hm) < yt
			 then
			   begin
                             (* cas 6 uniquement, necessité de savoir si face 3 ou 2 rencontrée en premier -> Calcul des temps relatif avant collision *)
                             if ((abs_float (float_of_int (xm - (xt + wt)))/.(float_of_int xd)) > (abs_float (float_of_int (yt - (ym + hm)))/.(float_of_int yd)))
                             (* face 3 rencontre en premiere *)
                             then Some (x,(((xt+wt),yf)),(0.0,ys))
                             (* face 2 rencontre en premier *)
                             else Some (x,((xf,(yt-hm))),(xs,0.0))
			   end
			 else
			   begin
                             (* reste à differencier cas 7 et cas 3 *)
                             if ym > (yt + ht)
                             then
                               begin
				 (* cas 7 uniquement, necessité de savoir si face 3 ou 4 rencontrée en premier -> Calcul des temps relatif avant collision *)
				 if ((abs_float (float_of_int (xm - (xt + wt)))/.(float_of_int xd)) > (abs_float (float_of_int (ym - (yt + ht)))/.(float_of_int yd)))
				 (* face 3 rencontre en premiere *)
				 then Some (x,(((xt+wt),yf)),(0.0,ys))
				 (* face 4 rencoantre en premier *)
				 else Some (x,((xf,(yt+ht))),(xs,0.0))
                               end
                             (* cas 3 *)
                             else Some (x,(((xt+wt),yf)),(0.0,ys))
			   end
                       end
                     (* reste les cas 2 et 4 à traiter pour ce faire il suffit de savoir si l'objet movible était au dessus ou en dessous *)
                     else
                       begin
			 if (ym + hm) <= yt
			 (* cas 2 *)
			 then Some (x,((xf,(yt-hm))),(xs,0.0))
			 (* cas 4 *)
			 else Some (x,((xf,(yt+ht))),(xs,0.0))
                       end
		   end
               end
             else subHit s
	end
    in
    (* on veut que les ennemis soit les premiers traités *)
    let rec sort list_iter list_enne list_others =
      match list_iter with
      |[]   -> List.append list_enne list_others
      |x::s when ((Objet.getGenre x)=(Ennemi)) -> sort s (x::list_enne) list_others
      |x::s -> sort s list_enne (x::list_others)
    in
    subHit (sort (kickObjectWithProj obj list []) [] [])
	  
	  
    
  let hit_boxProj obj list grav (sizeX,sizeY)=
    (* le calcul des collisions des projectiles se fera avant tout les autres, donc il faut calculer toutes les collisions possibles*)
    (* éléments nécessaire pour les calculs sur l'objet traité *)
    let (xm,ym) = Objet.getPos obj in
    let (wm,hm) = Objet.getSize obj in
    let (xs,ys) = Objet.getSpeed obj in
    (* calcul de la position future *)
    let (xf,yf) = ((int_of_float xs) + xm , (int_of_float ys) + ym ) in
    (* création d'un rectangle correspondant à l'objet traité dans le "futur" *)
    let rectObjet = Sdl.Rect.create xf yf wm hm in
    (* méthode de calcul des hit box *)
    let rec proj_subHit list =
      (* calcul des collisions avec les bords de la scene *)
      if (((xf-wm) < 0) || ((yf-hm) < 0) || (xf>sizeX) || (yf>sizeY))
      then Some(obj)
      else
	begin
	  match list with
	  |[] -> None
	  |x::s ->
	     (* éléments nécessaire pour les calculs sur le deuxieme objet traité *)
	     let (xt,yt) = Objet.getPos x in
	     let (wt,ht) = Objet.getSize x in
	     let (xst,yst) = Objet.getSpeed x in
	     let rectTemp = Sdl.Rect.create  xt yt wt ht in
	     (* deux cas, le deuxieme objet traité est lui aussi un projectile, ou non *)
	     if((Objet.getGenre x)=(Projectile))
	     then
	       begin
		 (* si ils vont dans la meme direction, et à la meme vitesse, ils ne sont pas censé se percuter *)
		 if(Objet.getSpeed obj = Objet.getSpeed x)
		 then proj_subHit s
		 else
		   begin
		     (* gestion des collisions,on ne s'interesse pas à où remettre les projectiles, car si il y a collision les projectiles disparaissent *)
		     if Sdl.has_intersection rectObjet rectTemp
		     then Some(x)
		     else proj_subHit s
		   end
	       end
	     else
	       (* si le deuxieme objet n'est pas un projectile *)
	       begin
		 (* on calcule aussi le deuxieme objet  dans le futur*)
		 (* calcul de la position future du deuxieme objet *)
		 let (xft,yft) = (((int_of_float xst) + xt ),((int_of_float(ceil(grav/.2.))) + (int_of_float yst) + yt)) in
		 (* création d'un rectangle correspondant deuxieme objet traité dans le "futur" *)
		 let rectTempFut = Sdl.Rect.create xft yft wt ht in
		 (* gestion des collisions, on ne s'interesse pas à où remettre le projectile, car si il y a collision le projectile disparait *)
		 if (((Sdl.has_intersection rectObjet rectTemp) && (((xst *. xs) < 0.0) || ((yst *. ys) < 0.0)))||(Sdl.has_intersection rectObjet rectTempFut))
		 then Some(x)
		 else proj_subHit s
	       end
	end
    in
    proj_subHit (kickObject obj list)
      
end
