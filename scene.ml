open Tsdl
open Objet
open Anim
open Collision
open Camera

module type Scene = sig
  type scene 
  val create : Objet.objet list -> float -> Objet.objet -> Camera.camera ->  Sdl.renderer -> scene
  val getEntitie : scene -> Objet.objet list
  val getTexture : scene -> Sdl.texture list
  val getSize : scene -> (int*int)
  val addEntitie : scene -> Objet.objet -> scene
  val kickDead : scene -> scene
  val generateScene : scene -> Objet.objet list -> scene
  val loadPicture : Sdl.renderer -> (int*int) -> (int*int) -> Sdl.texture -> unit
  val refresh : scene -> scene -> unit
  val closeScene : scene -> unit
  val moveAll : scene -> scene
  val movePersonnage : scene -> (float*float) -> scene
  val continue : scene -> bool
  val suicide : scene -> scene
  val shoot : scene -> (int*int) -> int -> scene
end

module Scene : Scene =  struct
  type scene = {entities:Objet.objet list ; gravitie:float ; background : Objet.objet ; cam : Camera.camera ; renderer : Sdl.renderer}

  exception NoPerso
  exception ErreurScene
              
  let create objs grav back camera render = {entities = objs ; gravitie = grav ; background = back ; cam = camera ;renderer = render}
                                              
  let getEntitie scene = scene.entities
                           
  let getSize scene = Objet.getSize scene.background 

  let addEntitie scene objet = {scene with entities =  (objet::scene.entities)}

  let getPers scene = 
    let rec getPers_rec l =
      match l with 
      |[]-> raise NoPerso
      |x::s when ((Objet.getGenre x) = Personnage) -> x
      |x::s -> getPers_rec s
    in 
    getPers_rec scene.entities

  let genererScene t pers scene  =
    let (l,g,b,p) = Lexer.lex t (Some pers ) scene.renderer  in
    {entities = l  ; gravitie =  g; background = b;
       cam =( Camera.create (Objet.getPos p) (Objet.getSize b) (Camera.getWindowSize scene.cam));
       renderer = scene.renderer}

  let kickDead scene = List.fold_left (fun acc x ->  (if (Objet.getPV x) < 1) then acc else (x::acc)) [] scene.entities

    
 
                
  let continue scene =(Objet.getPV (getPers scene))>0 
  let suicide scene =
    let rec sub l acc = 
      match l with
      |[] -> {scene with entities = acc }
      |x::s when (Objet.getGenre x) = Personnage -> sub s ((Objet.changePV x (-(Objet.getPV x)))::acc)
      |x::s->sub s (x::acc)
    in sub scene.entities []

  let nextPos obj scene = 
    let (xs,ys) = Objet.getSpeed obj in
    let xs_int = int_of_float xs in
    let (xp,yp) = Objet.getPos obj in
    ((xs_int + xp) , ((int_of_float(ceil(scene.gravitie/.2.) +. ys) + yp)))
 
  let changeAnim l = 
    let rec changeAnimRec l res = 
      match l with
      |[]->res 
      |x::s -> 
         let (xs,ys) = Objet.getSpeed x in 
         let dir =
           if Objet.canJump x then
	     let _ = Printf.printf "%f\n" xs in 
             if xs > 1.0 then Anim.Droite else if xs < -1.0 then  Anim.Gauche else Milieu
             else Anim.Saut
         in let size  = if Objet.getGenre x = Personnage then 
             if (dir = Anim.Saut) then (16,16) 
             else (23,40)
           else
             Objet.getSize x
            in changeAnimRec s ((Objet.setSize (Objet.changeFrame x dir) size)::res)
    in changeAnimRec l []
    
  (* gestion des déplacements *)
 let moveAll scene =

   let (sizeX,sizeY) = getSize scene in
   (* méthode pour obtenir la liste des objets de la liste en @param avec l'objet traité modifié (qui a perdu 20 PV) *)
   let rec damageObjet obj list reslist=
     match list with
     |[] -> reslist
     |x::s -> if x = obj then damageObjet obj s ((Objet.changePV x (-20))::reslist) else damageObjet obj s (x::reslist)
   in
   
   
   let rec moveAll_sub listObjet listRes cam =
     match listObjet with
     |[]->  {scene with entities = listRes ; cam = cam }
     (* traitement des déplacement / collision pour un projectile *)
     |x::s when ((Objet.getGenre x)=(Projectile)) ->
	begin
	  match (Collision.hit_boxProj x (List.append listObjet listRes) scene.gravitie (sizeX,sizeY)) with
	  |None ->
	     let (xs,ys) = Objet.getSpeed x in
	     let xs_int = int_of_float xs in
	     let ys_int = int_of_float ys in
	     let (xp,yp) = Objet.getPos x in
	     let objTemp = Objet.move x (xs_int + xp , ys_int + yp) in
	     moveAll_sub s (objTemp::listRes) cam;
	  (* quand le projectile percute les bords de la scene *)
	  |Some(obj) when obj = x ->
	     (* on retire le projectile du traitement *)
	     moveAll_sub s listRes cam
	  |Some(obj) ->
	     if ((Objet.getGenre obj)=(Projectile))
	     then
	       begin
		 (* on retire les deux projectiles du traitement, mais pour "obj" on ne sait pas si il va ou à déjà été traité *)
		 moveAll_sub (Collision.kickObject obj s) (Collision.kickObject obj listRes) cam
	       end
	     else
	       begin
		 (* on modifie le "obj" du traitement on sait qu'il n'a pas encore été traité*)
		 moveAll_sub (damageObjet obj s []) listRes cam
	       end
	end
     (* traitement des déplacements / collisions pour le personnage*)
     |x::s when ((Objet.getGenre x)=(Personnage)) ->
        begin
	  let temp = Camera.move (Objet.getPos x) cam in  
          match Collision.hit_boxPerso x (List.append listObjet listRes) (nextPos x scene) (sizeX,sizeY) with
          |None ->
	     let objTemp = Objet.move x (nextPos x scene) in
	     moveAll_sub s ((Objet.setSpeed objTemp (0.0,scene.gravitie))::listRes) temp
	  (*gestion du cas où le perso/un ennemi est "sortie" de la scene*)
	  |Some (obj,(xNew,yNew),(xsNew,ysNew)) when (obj = x) ->
	     (* on tue le "x" en cours de traitement*)
		 moveAll_sub s ((Objet.changePV x (-Objet.getPV x))::listRes) cam
          |Some (obj,(xNew,yNew),(xsNew,ysNew)) ->
	     match ((Objet.getGenre obj)) with
	     |Ennemi ->
		let objTemp = (Objet.changePV (Objet.allowJump (Objet.move x (xNew,yNew))) (-30)) in
		moveAll_sub s ((Objet.setSpeed (Objet.resetSpeed objTemp) ((0.0 +. xsNew),(scene.gravitie +. ysNew)))::listRes) temp
	     |Door t -> genererScene t x scene 	
	     |_ -> 
		let objTemp = Objet.allowJump (Objet.move x (xNew,yNew)) in
		moveAll_sub s ((Objet.setSpeed (Objet.resetSpeed objTemp) ((0.0 +. xsNew),(scene.gravitie +. ysNew)))::listRes) temp
	end
     (* traitement des déplacements / collisions pour les ennemis*)
     |x::s when ((Objet.getGenre x)=(Ennemi)) ->
        begin
          match Collision.hit_boxObjet x (List.append listObjet listRes) (nextPos x scene) (sizeX,sizeY) with
          |None ->
	     let objTemp = Objet.move x (nextPos x scene) in  
	     moveAll_sub s ((Objet.setSpeed objTemp (0.0,scene.gravitie))::listRes) cam
	  (*gestion du cas où le perso/un ennemi est "sortie" de la scene*)
	  |Some ((xNew,yNew),(xsNew,ysNew)) when (xsNew = 1000.0) ->
	     (* on tue le "x" en cours de traitement*)
		 moveAll_sub s ((Objet.changePV x (-Objet.getPV x))::listRes) cam
          |Some ((xNew,yNew),(xsNew,ysNew)) ->
	     let objTemp = Objet.allowJump (Objet.move x (xNew,yNew)) in
	     moveAll_sub s ((Objet.setSpeed (Objet.resetSpeed objTemp) ((0.0 +. xsNew),(scene.gravitie +. ysNew)))::listRes) cam
	end
     (* Ne rien faire pour les autres objets *)
     |x2::s -> moveAll_sub s (x2::listRes) cam
   in

   (* on met les projectiles de façon à ce qu'ils soient traiter avant toutes autres entitées, puis le personnage, puis le reste*)
    let rec sort list_iter list_proj list_perso list_enne list_others =
      match list_iter with
      |[]   -> List.append (List.append list_proj list_perso) (List.append list_others list_enne)
      |x::s when ((Objet.getGenre x)=(Projectile)) -> sort s (x::list_proj) list_perso list_enne list_others
      |x::s when ((Objet.getGenre x)=(Personnage)) -> sort s list_proj (x::list_perso) list_enne list_others
      |x::s when ((Objet.getGenre x)=(Ennemi))     -> sort s list_proj list_perso (x::list_enne) list_others
      |x::s -> sort s list_proj list_perso list_enne (x::list_others)
    in   
   let temp = (moveAll_sub (sort scene.entities [] [] [] []) [] scene.cam) in 
   {temp with entities = changeAnim temp.entities}

 let shoot scene (x,y) clock =
   let rec getPosPerso list =
     try
       match list with
       |[] -> raise NoPerso 
       |x::s when ((Objet.getGenre x)=(Personnage)) -> x
       |x::s -> getPosPerso s
     with NoPerso -> failwith "Pas de personnages dans la scene !"
   in
   if (clock != 0) then scene
   else
     begin
       let perso = (getPosPerso scene.entities) in
       let (xP,yP) = Objet.getPos perso in
       
       match (x,y) with
       (* coordonnée d'apparition des projectile à determiné *)
       |(0,(-1))   -> addEntitie scene (Objet.create Projectile (xP+11,yP-10) (0.0,(-.8.0))      (8.0,8.0) 10 (Anim.create [||] [|"Image/Samus_proj_10_10.bmp"|] [||] [||] scene.renderer) scene.renderer)
       |((-1),0)   -> addEntitie scene (Objet.create Projectile (xP-10,yP+13) ((-.8.0),0.0)      (8.0,8.0) 10 (Anim.create [||] [|"Image/Samus_proj_10_10.bmp"|] [||] [||] scene.renderer) scene.renderer)
       |(0,1)      -> addEntitie scene (Objet.create Projectile (xP+11,yP+40) (0.0,8.0)          (8.0,8.0) 10 (Anim.create [||] [|"Image/Samus_proj_10_10.bmp"|] [||] [||] scene.renderer) scene.renderer)
       |(1,0)      -> addEntitie scene (Objet.create Projectile (xP+25,yP+13) (8.0,0.0)          (8.0,8.0) 10 (Anim.create [||] [|"Image/Samus_proj_10_10.bmp"|] [||] [||] scene.renderer) scene.renderer)
       |(1,1)      -> addEntitie scene (Objet.create Projectile (xP+25,yP+40) (8.0,8.0)          (8.0,8.0) 10 (Anim.create [||] [|"Image/Samus_proj_10_10.bmp"|] [||] [||] scene.renderer) scene.renderer)
       |(1,(-1))   -> addEntitie scene (Objet.create Projectile (xP+25,yP-10) (8.0,(-.8.0))      (8.0,8.0) 10 (Anim.create [||] [|"Image/Samus_proj_10_10.bmp"|] [||] [||] scene.renderer) scene.renderer)
       |((-1),1)   -> addEntitie scene (Objet.create Projectile (xP-10,yP+40) ((-.8.0),8.0)      (8.0,8.0) 10 (Anim.create [||] [|"Image/Samus_proj_10_10.bmp"|] [||] [||] scene.renderer) scene.renderer)
       |((-1),(-1))-> addEntitie scene (Objet.create Projectile (xP-10,yP-10) ((-.8.0),(-.8.0))  (8.0,8.0) 10 (Anim.create [||] [|"Image/Samus_proj_10_10.bmp"|] [||] [||] scene.renderer) scene.renderer)
       |_          -> scene
     end
      
        
 (* gestion des déplacements *)
 let movePersonnage scene (xs,ys)=
   let rec changePerso listObjet listRes =
     match listObjet with
     |[] -> listRes
     |x::s when ((Objet.getGenre x) = Personnage) ->
	if ys < 0.0
	    then
	      begin
		if (Objet.canJump x = false)
		then changePerso s ((Objet.dmgGesture (Objet.setSpeed x (xs,0.0)))::listRes)
		else changePerso s ((Objet.dmgGesture (Objet.forbidJump (Objet.setSpeed x (xs,ys))))::listRes)
	      end
	    else changePerso s ((Objet.dmgGesture (Objet.setSpeed x (xs,ys)))::listRes)
     |x::s -> changePerso s (x::listRes)
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
	if ((Objet.getPV x) < 1) then 
          if ((Objet.getGenre x) = Personnage) then
            ()
          else
            (refresh_sub s) 
	else
          begin
            loadPicture sceneNew.renderer (Camera.convertPosObjet (Objet.getPos x) sceneNew.cam) (Objet.getSize x) (Objet.getTexture x);
            refresh_sub s
          end
   in
   match Sdl.render_clear sceneOld.renderer with
   |Error (`Msg e) -> Sdl.log "Init render error: %s" e; exit 1
   |Ok () ->
      begin
	loadPicture sceneNew.renderer (Camera.convertPosBackground sceneNew.cam)
          (Objet.getSize sceneNew.background) (Objet.getTexture sceneNew.background);
	refresh_sub sceneNew.entities;
	Sdl.render_present sceneNew.renderer
      end
	
 let getTexture scene =
   let rec sub list res =
     match list with
     |[] -> res
     |x::s -> sub s ((Objet.getTexture x)::res)
   in
   sub scene.entities []
     
 let getGravitie scene = scene.gravitie
     
 let generateScene scene objetList =
   let rec sub list res =
     match list with
     |[] -> res
     |x::s -> sub s (addEntitie res x)
   in
   sub objetList scene
     
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
