open Tsdl
open Objet

module type Scene = sig
  type scene
    
  val create : Objet.objet list -> float -> Objet.objet -> Sdl.renderer -> scene
  val getEntitie : scene -> Objet.objet list
  val getTexture : scene -> Sdl.texture list
  val getGravitie : scene -> float
  val addEntitie : scene -> Objet.objet -> scene
  val generateScene : scene -> Objet.objet list -> scene
  (* val applyGravitie : scene -> scene *)
  val moveAll : scene -> scene
  val movePersonnage : scene -> (float*float) -> scene
  val loadPicture : Sdl.renderer -> (int*int) -> (int*int) -> Sdl.texture -> unit
  val refresh : scene -> scene -> unit
  val hit_box : Objet.objet -> scene -> ((int*int) * (float * float)) option
  val closeScene : scene -> unit
end

module Scene : Scene =  struct
  type scene = {entities:Objet.objet list ; gravitie:float ; background : Objet.objet ; renderer : Sdl.renderer}
    
  let create objs grav back render = {entities = objs ; gravitie = grav ; background = back ; renderer = render}
    
  let getEntitie scene = scene.entities



  (* gestion des hit-box (sauf projectiles)*)
  let hit_box obj scene =
    (* éléments nécessaire pour les calculs sur l'objet traité *)
    let (xm,ym) = Objet.getPos obj in
    let (wm,hm) = Objet.getSize obj in
    let (xs,ys) = Objet.getSpeed obj in
    (* calcul de la position future *)
    let (xf,yf) = (((int_of_float xs) + xm ),((int_of_float(ceil(scene.gravitie/.2.))) + (int_of_float ys) + ym)) in
    (* création d'un rectangle correspondant à l'objet traité dans le "future" *)
    let rectObjet = Sdl.Rect.create xf yf wm hm in
    (* méthode pour obtenir la liste des objets de la scene sans l'objet traité (évite self collision) *)
    let rec kickObject list reslist=
      match list with
      |[] -> reslist
      |x::s -> if x = obj then kickObject s reslist else kickObject s (x::reslist)
    in
    (* méthode de calcul des hit box *)
    let rec subHit list =
      match list with
      |[] -> None
      |x::s when ((Objet.getGenre x)=(Plateforme)) ->
         (* éléments nécessaire pour les calculs sur le deuxieme objet traité *)
         let (xt,yt) = Objet.getPos x in
         let (wt,ht) = Objet.getSize x in
         (*let genreTemp = Objet.getGenre x in*)
         let rectTemp = Sdl.Rect.create  xt yt wt ht in
         (* gestion des collisions pour les plateformes *)
         if ((Sdl.has_intersection rectObjet rectTemp)&&((ym+hm) <= yt)) 
         then
           begin
             (*calcul du déplacement de notre objet*)
             let (xd,yd) = (xf-xm , yf-ym) in
           (*
               Gestion des 3 cas de collisions possibles pour une plateforme (directions) :
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
         (*let genreTemp = Objet.getGenre x in*)
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
    subHit (kickObject scene.entities [])
    
  let applyGravitie scene =
    let grav = (scene.gravitie *. scene.gravitie) /. 2.0 in
    let rec setAll listObjet listRes =
      match listObjet with
      |[]-> listRes
      |x::s -> setAll s ((Objet.setSpeed x (0.0,grav))::listRes)
    in
    {scene with entities = (setAll scene.entities [] ) }

 (*let moveAll scene =
    let rec moveAll_sub listObjet listRes =
      match listObjet with
      |[]-> listRes
      |x::s -> moveAll_sub s ((Objet.move x)::listRes)
    in
    let temp = applyGravitie scene in
    {temp with entities = (moveAll_sub temp.entities [] ) }
 *)
  (* gestion des déplacements *)

  let movePers scene = 
    let rec movePers_sub listObjet listRes = 
      match listObjet with 
      |[] -> listRes 
      |x::s when (Objet.isMovable x) ->
        begin
          let (xs,ys) = Objet.getSpeed x in
          let xs_int = int_of_float xs in
          let ys_int = int_of_float ys in
          let (xp,yp) = Objet.getPos x in
          match (hit_box x scene) with         
          |None ->
            let objTemp = Objet.move x ((xs_int + xp) , (((int_of_float(ceil(scene.gravitie/.2.))) + ys_int + yp))) in
            movePers_sub s ((Objet.setSpeed objTemp (0.0,scene.gravitie))::listRes)
          |Some ((xNew,yNew),(xsNew,ysNew)) ->
            let objTemp = Objet.allowJump (Objet.move x (xNew,yNew)) in
            movePers_sub s  ((Objet.setSpeed (Objet.resetSpeed objTemp) ((0.0 +. xsNew),(scene.gravitie +. ysNew)))::listRes)
        end
      |x::s->
        movePers_sub s (x::listRes)
    in
    {scene with entities = (movePers_sub scene.entities [] ) }

  let rec getPers l = 
    match l with 
    |[]-> failwith " "
    |a::b -> if (Objet.getGenre a) = Personnage then a else getPers b 

  let moveCam scene = 
    let rec moveCam_sub listObjet listRes  = 
      match listObjet with 
      |[]-> listRes
      |x::s ->
        let (xs,ys) = Objet.getSpeed x in
        let xs_int = int_of_float xs in
        let ys_int = int_of_float ys in
        let (xp,yp) = Objet.getPos x in
        match (hit_box x scene) with
        |None ->
          let pers = getPers scene.entities in 
          let (vxPers,vyPers) = Objet.getSpeed pers in 
          let vxp = int_of_float vxPers in 
          let vyp = int_of_float vyPers in         
          if x != pers then 
            let objTemp = Objet.move x ((xp-vxp) , (yp)) in
            moveCam_sub s ((Objet.setSpeed objTemp (0.0,scene.gravitie))::listRes)
          else 
            let objTemp = Objet.move x ((xp-vxp) , (yp+  int_of_float(ceil(scene.gravitie/.2.)))) in
            moveCam_sub s ((Objet.setSpeed objTemp (0.0,scene.gravitie))::listRes)
        |Some ((xNew,yNew),(xsNew,ysNew)) ->
          let objTemp = Objet.allowJump (Objet.move x (xNew,yNew)) in
          moveCam_sub s  ((Objet.setSpeed (Objet.resetSpeed objTemp) ((0.0 +. xsNew),(scene.gravitie +. ysNew)))::listRes)
    in {scene with entities = (moveCam_sub scene.entities [] ) }


  let moveAll scene =
    let (xb,yb) = Objet.getPos scene.background in 
    let (wb,hb) = Objet.getSize scene.background in 
    let pers = getPers scene.entities in 
    let (vxPers,vyPers) = Objet.getSpeed pers in 
    let vxp = int_of_float vxPers in 
    let vyp = int_of_float vyPers in
    if (((xb + wb > 1000) && (vxPers>0.))|| ((xb<0)&&(vxPers<0.))) then  
      let temp = moveCam scene in
      {temp with background =Objet.move scene.background (xb-vxp,yb) }
    else
      movePers scene

  let movePersonnage scene (xs,ys) =
    let rec changePerso listObjet listRes =
      match listObjet with
      |[] -> listRes
      |x::s -> if (Objet.getGenre x) = Personnage then changePerso s ((Objet.setSpeed x (xs,ys))::listRes)
        else changePerso s (x::listRes)
    in
{scene with entities = (changePerso scene.entities [] ) }

 let loadPicture renderer (x1,y1) (w,h) texture =
    let frag_rect = Sdl.Rect.create 0 0 w h in
    let position_background = Sdl.Rect.create x1 y1 w h in
    match Sdl.render_copy ~dst:position_background ~src:frag_rect renderer texture with
    |Error (`Msg e) -> Sdl.log "Init texture on screen error: %s" e; exit 1
|Ok () -> ()

 let refresh sceneOld sceneNew =
    let rec refresh_sub list =
      match list with
      |[] -> ();
      |x::s ->
         if ((Objet.getPV x) < 1) then (refresh_sub s) else
           loadPicture sceneNew.renderer (Objet.getPos x) (Objet.getSize x) (Objet.getTexture x);
           refresh_sub s
    in
    match Sdl.render_clear sceneOld.renderer with
               |Error (`Msg e) -> Sdl.log "Init render error: %s" e; exit 1
               |Ok () -> loadPicture sceneNew.renderer (Objet.getPos sceneNew.background) 
                    (Objet.getSize sceneNew.background) (Objet.getTexture sceneNew.background);
                  refresh_sub sceneNew.entities;
                  Sdl.render_present sceneNew.renderer


  let getTexture scene =
    let rec sub list res =
      match list with
      |[] -> res
      |x::s -> sub s ((Objet.getTexture x)::res)
    in
    sub scene.entities [];;

  let getGravitie scene = scene.gravitie

  let addEntitie scene objet =
    {scene with entities =  (objet::scene.entities)}

  let generateScene scene objetList =
    let rec sub list res =
      match list with
      |[] -> res
      |x::s -> sub s (addEntitie res x)
    in
    sub objetList scene


  ;;

  let closeScene scene =
    let rec close_sub list =
      match list with
      |[] -> Sdl.destroy_texture (Objet.getTexture scene.background);
      |x::s ->
         begin
           Sdl.destroy_texture (Objet.getTexture x);
           close_sub s
         end
    in
    close_sub scene.entities

end
  
