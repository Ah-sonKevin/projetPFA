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
  val movePersonnage : scene -> (float*float) -> scene * scene
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

  let genererScene t scene  =
    let (l,g,b,p) = Lexer.lex t scene.renderer  in
    { entities =l; gravitie =  g; background = b;
       cam =( Camera.create (Objet.getPos p) (Objet.getSize b) (Camera.getWindowSize scene.cam));
       renderer = scene.renderer}

  let kickDead scene =
    let rec sub_kick list listRes =
	match list with
	|[] -> {scene with entities = listRes}
	|x::s when (((Objet.getPV x) < 1) && (Objet.getGenre x != Personnage)) -> sub_kick s listRes
	|x::s -> sub_kick s (x::listRes)
    in
    sub_kick scene.entities []
    
  let getPers scene = 
    let rec getPers_rec l =
      match l with 
      |[]-> failwith "No Perso"
      |x::s-> 
        if ((Objet.getGenre x) = Personnage) then 
          x 
        else
          getPers_rec s
    in 
    getPers_rec scene.entities
                
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
     |x::s ->
	let (xs,ys) = Objet.getSpeed x in
	let xs_int = int_of_float xs in
	let ys_int = int_of_float ys in
	let (xp,yp) = Objet.getPos x in
	let objTemp = Objet.move x (xs_int + xp , ys_int + yp) in
	let tempCam = if (Objet.getGenre x) = Personnage then Camera.move (Objet.getPos x) cam else cam
	in
	moveAll_sub s (objTemp::listRes) cam
   in
   let temp = (moveAll_sub scene.entities [] scene.cam) in 
   ({temp with entities = changeAnim temp.entities},scene)

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
       |(0,(-1))   -> addEntitie scene (Objet.create Projectile (xP+11,yP-10) (0.0,(-.8.0))      (8.0,8.0) 10 ([||],[|"Image/Samus_proj_10_10.bmp"|],[||],[||]) scene.renderer)
       |((-1),0)   -> addEntitie scene (Objet.create Projectile (xP-10,yP+13) ((-.8.0),0.0)      (8.0,8.0) 10 ([||],[|"Image/Samus_proj_10_10.bmp"|],[||],[||]) scene.renderer)
       |(0,1)      -> addEntitie scene (Objet.create Projectile (xP+11,yP+40) (0.0,8.0)          (8.0,8.0) 10 ([||],[|"Image/Samus_proj_10_10.bmp"|],[||],[||]) scene.renderer)
       |(1,0)      -> addEntitie scene (Objet.create Projectile (xP+25,yP+13) (8.0,0.0)          (8.0,8.0) 10 ([||],[|"Image/Samus_proj_10_10.bmp"|],[||],[||]) scene.renderer)
       |(1,1)      -> addEntitie scene (Objet.create Projectile (xP+25,yP+40) (8.0,8.0)          (8.0,8.0) 10 ([||],[|"Image/Samus_proj_10_10.bmp"|],[||],[||]) scene.renderer)
       |(1,(-1))   -> addEntitie scene (Objet.create Projectile (xP+25,yP-10) (8.0,(-.8.0))      (8.0,8.0) 10 ([||],[|"Image/Samus_proj_10_10.bmp"|],[||],[||]) scene.renderer)
       |((-1),1)   -> addEntitie scene (Objet.create Projectile (xP-10,yP+40) ((-.8.0),8.0)      (8.0,8.0) 10 ([||],[|"Image/Samus_proj_10_10.bmp"|],[||],[||]) scene.renderer)
       |((-1),(-1))-> addEntitie scene (Objet.create Projectile (xP-10,yP-10) ((-.8.0),(-.8.0))  (8.0,8.0) 10 ([||],[|"Image/Samus_proj_10_10.bmp"|],[||],[||]) scene.renderer)
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
