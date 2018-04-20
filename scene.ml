open Tsdl
open Objet
open Anim
open Collision
open Camera
open Sound
open Random
open Tools

module type Scene = sig
  type scene 
  val create : Objet.objet list -> float -> Objet.objet -> Camera.camera ->  Sdl.renderer -> string -> scene
  val getEntitie : scene -> Objet.objet list
  val getTexture : scene -> Sdl.texture list
  val getSize : scene -> (int*int)
  val addEntitie : scene -> Objet.objet -> scene
  val removeEntitie : scene -> Objet.objet -> scene 
  val kickDead : scene -> scene
  val refreshLifebar : scene -> scene
  val refresh : scene -> scene -> unit
  val closeScene : scene -> unit
  val collision_All : scene -> scene
  val moveAll : scene -> scene
  val shootAll : scene -> scene
  val movePersonnage : scene -> (float*float) -> scene
  val continue : scene -> bool
  val suicide : scene -> scene
  val shoot : Objet.objet -> scene -> (int*int) ->  scene
  val getPers : scene -> Objet.objet
  val decreaseClock : scene -> scene
end

module Scene : Scene =  struct
  type scene = {entities:Objet.objet list ; gravitie:float ; background : Objet.objet ;
                cam : Camera.camera ; renderer : Sdl.renderer; lifebar : Objet.objet}

  exception NoPerso
  exception ErreurScene
              
  let create objs grav back camera render theme =
    Sound.play_mus theme;
    {entities = objs ; gravitie = grav ; background = back ; cam = camera ;renderer = render; 
     lifebar =
	let rec getPers_rec l =
	  match l with 
	  |[]-> raise NoPerso
	  |x::s when ((Objet.getGenre x) = Personnage) -> x
	  |x::s -> getPers_rec s
	in
	let pv = Objet.getPV (getPers_rec objs) in
	Objet.create (Wall ((pv),20)) (900-(pv),10) (0.0,0.0) (0.0,0.0) 1000 (Anim.create [||] [|"Image/LifeBar.bmp"|] [||] [||] render) render
    }
      
  let getEntitie scene = scene.entities                           
  let getSize scene = Objet.getSize scene.background
  let getPers scene = 
    let rec getPers_rec l =
      match l with 
      |[]-> raise NoPerso
      |x::s when ((Objet.getGenre x) = Personnage) -> x
      |x::s -> getPers_rec s
    in 
    getPers_rec scene.entities
    
  let addEntitie scene objet = {scene with entities =  (objet::scene.entities)}
  let removeEntitie scene objet = {scene with entities = List.fold_left (fun res obj -> if obj=objet then res else (obj::res)) [] scene.entities}

  let newPowerUp (x,y) renderer=   
    if (Random.int 10) <=8 then 
      (Objet.create (PowerUp HP) (x,y) (0.0,0.0) (0.0,1.0) 100 (Anim.create [||] [|"Image/powerUpHP.bmp"|] [||] [||] renderer) renderer)
    else
      (Objet.create (PowerUp Inv) (x,y) (0.0,0.0) (0.0,1.0) 100 (Anim.create [||] [|"Image/powerUpInv.bmp"|] [||] [||] renderer) renderer)
      
  (*On enleve les objet qui sont mort, on conserve le personnage meme mort, car la scene a besoin de lui pour determiner le game over*)
  let kickDead scene = 
    {scene with entities = List.fold_left (fun acc x ->
      if (((Objet.getPV x) < 1) && (Objet.getGenre x)!=Personnage) then
	match  (Objet.getGenre x ) with
	|Ennemi _ ->
	   if  ((Random.int 5)= 0) then (((newPowerUp (Objet.getPos x) scene.renderer))::acc) else acc
	| _ ->  acc
      else (x::acc)) [] scene.entities
    }
      
  let refreshLifebar scene =
    {scene with lifebar =
	let pv = Objet.getPV (getPers scene) in
	Objet.create (Wall ((pv),20)) (950-(pv),10) (0.0,0.0) (0.0,0.0) 1000 (Anim.create [||] [|"Image/LifeBar.bmp"|] [||] [||] scene.renderer) scene.renderer}
  let continue scene =(Objet.getPV (getPers scene))>0

  let suicide scene =
    print_string "bwark";
    {scene with entities = List.map (fun  x -> if (Objet.getGenre x) = Personnage then  Objet.kill x else x) scene.entities }
      
  let nextPos obj scene = 
    let (xs,ys) = Objet.getSpeed obj in
    let xs_int = int_of_float xs in
    let (xp,yp) = Objet.getPos obj in
    ((xs_int + xp) , ((int_of_float(ceil(scene.gravitie/.2.) +. ys) + yp)))

      
  let decreaseClock scene =
    {scene with entities = List.map (fun x -> Objet.decreaseClock x) scene.entities}
      
  (*Animation du personnage *)
  let changeAnim l = 
    List.map 
      (fun x -> 
        let (xs,ys) = Objet.getSpeed x in 
        let dir =
          if Objet.canJump x then
            if xs >= 1.0 then Anim.Droite else if xs <= -1.0 then  Anim.Gauche else Milieu
            else Anim.Saut
        in Objet.changeFrame x dir) l 

  let collision_All scene  =      
    let out_of_bound obj =
      match Objet.isMovable obj with
      |false  -> obj
      |true ->
	 let (x,y) = Objet.getPos obj in
	 let (w,h) = Objet.getSize obj in
	 let (sizeX,sizeY) = getSize scene in
	 (* calcul des collisions avec les bords de la scene, on regarde si l'objet est "sorti" de la scene*)
	 if (((x+w) < 0) || ((y+h) < 0) || ((x)>sizeX) || ((y)>sizeY)) then Objet.kill obj else obj
    in
    let temp = {scene with entities =
                             List.map (fun obj -> 
                                 (List.fold_left (Collision.collision) obj (getEntitie (removeEntitie scene obj))  )) (getEntitie scene)} in
    {temp with entities = (List.map (out_of_bound) (getEntitie temp))}


      
  (* gestion des déplacements *)
  let moveAll scene =
    let rec moveAll_sub listObjet listRes cam =
      match listObjet with
      |[]->  {scene with entities = listRes ; cam = cam }
      (* traitement des déplacement pour un projectile ou un ennemi volant *)
      |x::s when (((Objet.getGenre x)=(Projectile)) || ((Objet.getGenre x) = (Ennemi Fly)) || ((Objet.getGenre x) = (Ennemi Both))) ->
	 begin
	   let (xs,ys) = Objet.getSpeed x in
	   let xs_int = int_of_float xs in
	   let ys_int = int_of_float ys in
	   let (xp,yp) = Objet.getPos x in
	   let objTemp = Objet.move x (xs_int + xp , ys_int + yp) in
	   moveAll_sub s (objTemp::listRes) cam;
	 end
      (* traitement des déplacements pour le personnage et les ennemis restant*)
      |x::s when (Objet.isMovable x) ->
         begin
	   let temp = if (Objet.getGenre x = Personnage) then Camera.move (Objet.getPos x) cam else cam in
	   let objTemp = Objet.move x (nextPos x scene) in
	   moveAll_sub s ((Objet.setSpeed objTemp (0.0,scene.gravitie))::listRes) temp
	 end
      (* Ne rien faire pour les autres objets *)
      |x2::s -> moveAll_sub s (x2::listRes) cam
    in

    let doorCollision scene =
      let perso = getPers scene in
      let (x1,y1) = Objet.getPos perso in
      let (w1,h1) = Objet.getBaseSize perso in
      let rectPerso = Sdl.Rect.create x1 y1 w1 h1 in
      let rec sub obj list =
	match list with
	|[]-> None
	|d::s ->
	   match (Objet.getGenre d) with
	   |Door t ->
	      let (x,y) = Objet.getPos d in
	      let (w,h) = Objet.getBaseSize d in
	      let rectDoor = Sdl.Rect.create x y w h in
	      if Sdl.has_intersection rectPerso rectDoor
	      then
		let (l,g,b,t,p) = Lexer.lex t (Some perso ) scene.renderer  in
		Some (create l g b (Camera.create (Objet.getPos p) (Objet.getSize b) (Camera.getWindowSize scene.cam)) scene.renderer  t)
	      else sub obj s
	   |_ -> sub obj s
      in
      sub perso scene.entities
    in

    
    let temp1 = (moveAll_sub  scene.entities [] scene.cam) in
    match doorCollision temp1 with
    |None ->
       let temp2 = {temp1 with entities = changeAnim temp1.entities} in
       collision_All temp2
    |Some (sc) ->  sc 

  let shootAll scene =
    (* récupère le personnage *)
    let p = getPers scene in
    let rec shootAll_sub listObjet listRes =
      (* on récupère les coordonnées du personnage pour déterminer les tirs des ennemis *)
      let (xp,yp) = Objet.getPos p in
      let (wp,hp) = Objet.getBaseSize p in
      match listObjet with
      |[]->  {scene with entities = listRes }
      (* On fait tirer les ennemis qui le peuvent *)
      |x::s ->
	 match Objet.getGenre x with
	 |Ennemi k when ((k  = Shooter) || (k = Both)) ->
	    begin
	      (* on fait tirer les ennemis qui le peuvent *)
	      if (not (Objet.canShoot x)) then shootAll_sub s (x::listRes)
	      else
		begin
		  let (xt,yt) = Objet.getPos x in
		  let (w,h) = Objet.getBaseSize x in
		  (* on calcul le milieu du tireur et le milieu du personnage *)
		  let (ppX,ppY) = (xp + (wp/2),yp + (hp/2)) in
		  let (peX,peY) = (xt + (w/2),yt + (h/2)) in
		  (* on calcul la position du personnage par rapport au tireur (dans le référentiel du tireur donc) *)
		  let (vecx,vecy) = (ppX-peX,ppY-peY) in
		  let norm = sqrt(float_of_int(vecx*vecx + vecy*vecy))in
		  (* on ne fait pas tirer l'ennemi si il est trop loin du personnage *)
		  if norm > 900.0 then
		    shootAll_sub s (x::listRes)
		  else
		    let (normX,normY) = ((float_of_int vecx)/.norm , (float_of_int vecy)/.norm) in
		    (*decalage afin que le tireur ne tire pas dans lui meme on calcul par rapport aux milieu des objets, puis on refera un décalage*)
		    (*pythagore*)
		    let distdiagEnProj = int_of_float(sqrt(float_of_int(h*h+w*w)))+10 in
		    (*on applique notre vecteur normalisé afin de définir la "direction" dans laquelle ira le projectil à sa creation le +6 c'est 
		      la taille du projectile divisé par 2 (w et h, sont les meme vus qu'il est carré)*)
		    let (posX,posY) = (int_of_float((normX)*.(float_of_int(distdiagEnProj+1+6))),
				       int_of_float((normY)*.(float_of_int(distdiagEnProj+1+6)))) in
		    let rng = Random.int 36 in
		    let proj = Objet.create Projectile 
                      (xt+posX,yt+posY) 
                      ((normX)*.(8.0),(normY)*.(8.0))  (10.0,10.0) 10 
                      (Anim.create [||] [|"Image/Ennemi_proj.bmp"|] [||] [||] scene.renderer) 
                      scene.renderer in
		    shootAll_sub s (proj::(Objet.triggerShoot x (120+rng))::listRes)
		end
	    end
	 (* Ne rien faire pour les autres objets *)
	 |_ -> shootAll_sub s (x::listRes)
    in
    shootAll_sub  scene.entities []
	 
    
  let shoot tireur scene (x,y) = 
    if (not (Objet.canShoot tireur)) then scene
    else
      begin
	Sound.play_sound Sound.Tir  ;
	let (xP,yP) = Objet.getPos tireur in
	let (xs,ys) = Objet.getBaseSize tireur in
	(*decalage afin que le tireur ne tire pas dans lui meme*)
	let decX = if x = 1 then xs else if x = 0 then xs/2 else 0 in       
	let decY = if y = 1 then ys else if y = 0 then ys/2 else 0 in
	let temp = List.rev (List.fold_left (fun acc x -> if x = tireur then ((Objet.triggerShoot x 15)::acc) else (x::acc)) [] scene.entities) in
	let temp2 = {scene with entities = temp } in
	addEntitie temp2 (Objet.create Projectile 
                            (xP+decX+x*10,yP+decY+y*10) 
                            ((float_of_int x)*.(9.0),(float_of_int y)*.(9.0))  (9.0,9.0) 10 
                            (Anim.create [||] [|"Image/Samus_proj_10_10.bmp"|] [||] [||] scene.renderer) 
                            scene.renderer)
      end
	
  (* gestion des déplacements du personnage*)
  let movePersonnage scene (xs,ys)=
    let l = List.map 
      (fun x ->
	if (((Objet.getGenre x) = Personnage) && (Objet.canBeDmg x)) then
 	  if ys < 0.0  then
            if not (Objet.canJump x) then 
              Objet.setSpeed x (xs,0.0)
	    else 
              begin
		Sound.play_sound Sound.Saut ;
		Objet.forbidJump (Objet.setSpeed x (xs,ys))
              end
	  else 
            Objet.setSpeed x (xs,ys)
	else
          x
      ) scene.entities
    in {scene with entities = l}
    
   
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
	     begin
             if ((Objet.getGenre x) = Personnage ) then
	       if (Objet.clignote x) then
		 Tools.loadPicture sceneNew.renderer (Camera.convertPosObjet (Objet.getPos x) sceneNew.cam) (Objet.getSize x) (Objet.getTexture x)
	       else ()
	     else
	       Tools.loadPicture sceneNew.renderer (Camera.convertPosObjet (Objet.getPos x) sceneNew.cam) (Objet.getSize x) (Objet.getTexture x)
	     end;
             refresh_sub s
           end
    in
    match Sdl.render_clear sceneOld.renderer with
    |Error (`Msg e) -> Sdl.log "Init render error: %s" e; exit 1
    |Ok () ->
       begin
	 Tools.loadPicture sceneNew.renderer (Camera.convertPosBackground sceneNew.cam)
           (Objet.getSize sceneNew.background) (Objet.getTexture sceneNew.background);
	 refresh_sub sceneNew.entities;
	 Tools.loadPicture sceneNew.renderer (Objet.getPos sceneNew.lifebar) (Objet.getSize sceneNew.lifebar) (Objet.getTexture sceneNew.lifebar)
       end
	 
  let getTexture scene =
    let rec sub list res =
      match list with
      |[] -> res
      |x::s -> sub s ((Objet.getTexture x)::res)
    in
    sub scene.entities []
      
  let getGravitie scene = scene.gravitie
    
  let closeScene scene =
    List.iter (fun x -> Sdl.destroy_texture (Objet.getTexture x)) scene.entities;
    Sdl.destroy_texture (Objet.getTexture scene.background)
      
end
  
