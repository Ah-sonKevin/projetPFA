  (* gestion des hit-box (sauf projectiles)*)
  let hit_box obj scene =
    (* éléments nécessaire pour les calculs sur l'objet traité *)
    let (xm,ym) = Objet.getPos obj in
    let (wm,hm) = Objet.getSize obj in
    let (xs,ys) = Objet.getSpeed obj in
    (* calcul de la position future *)
    let (xf,yf) = (((int_of_float xs) + xm ),((int_of_float(ceil((scene.gravitie )/.2.))) + (int_of_float ys) + ym)) in
    (* création d'un rectangle correspondant à l'objet traité dans le "future" *)
    let rectObjet = Sdl.Rect.create xf yf wm hm in

    (* méthode de calcul des hit box *)
    let rec subHit list =
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
                -     -  0  -     -
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
      |x::s -> 
         (* éléments nécessaire pour les calculs sur le deuxieme objet traité *)
         let (xt,yt) = Objet.getPos x in
         let (wt,ht) = Objet.getSize x in
         let rectTemp = Sdl.Rect.create  xt yt wt ht in
         (* gestion des collisions pour les objets autres que les plateformes *)
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
                             (* cas 8 uniquement, necessité de savoir si face 1 ou 2 rencontrée en premier -> Calcul des temps relatif avant collision *)
                             if ((abs_float (float_of_int (xt - (xm + wm)))/.(float_of_int xd)) > (abs_float (float_of_int (ym - (yt + ht)))/.(float_of_int yd)))
                             (* face 1 rencontre en premiere *)
                             then Some ((((xt-wm),yf)),(0.0,ys))
                             (* face 2 rencontre en premier *)
                             else Some (((xf,(yt-hm))),(xs,0.0))
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
                         (* cas 6 uniquement, necessité de savoir si face 1 ou 2 rencontrée en premier -> Calcul des temps relatif avant collision *)
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
                             (* cas 7 uniquement, necessité de savoir si face 1 ou 2 rencontrée en premier -> Calcul des temps relatif avant collision *)
                             if ((abs_float (float_of_int (xm - (xt + wt)))/.(float_of_int xd)) > (abs_float (float_of_int (ym - (yt + ht)))/.(float_of_int yd)))
                             (* face 3 rencontre en premiere *)
                             then Some ((((xt+wt),yf)),(0.0,ys))
                             (* face 4 rencontre en premier *)
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
    in
    subHit (kickObjet obj (scene.entities) [])
