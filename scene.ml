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
  val addEntitie : scene -> Objet.objet -> scene
  val generateScene : scene -> Objet.objet list -> scene
  val loadPicture : Sdl.renderer -> (int*int) -> (int*int) -> Sdl.texture -> unit
  val refresh : scene -> scene -> unit
  val closeScene : scene -> unit
  val moveAll : scene -> scene
  val movePersonnage : scene -> (float*float) -> scene
  val suicide : scene -> scene 
end

module Scene : Scene =  struct
  type scene = {entities:Objet.objet list ; gravitie:float ; background : Objet.objet ; cam : Camera.camera ; renderer : Sdl.renderer}

  let create objs grav back camera render = {entities = objs ; gravitie = grav ; background = back ; cam = camera ;renderer = render}

  let getEntitie scene = scene.entities
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
        let dir = if xs > 0.0 then Anim.Droite else if xs < 0.0 then  Anim.Gauche else Milieu 
        in changeAnimRec s ((Objet.changeFrame x dir)::res)
    in changeAnimRec l []

  (* gestion des déplacements *)
  let moveAll scene =   
    let rec moveAll_sub listObjet listRes cam =
      match listObjet with
      |[]-> (listRes,cam)
      (* traitement des déplacement / collision pour les objets en déplacements autres que projectile*)
      |x::s when (Objet.isMovable x) ->
        begin
        match Collision.hit_boxObjet x scene.entities (nextPos x scene)  with
        |None ->
	  let objTemp = Objet.move x (nextPos x scene) in
          let temp = if (Objet.getGenre x) = Personnage then Camera.move (Objet.getPos x) cam else cam in  
	  moveAll_sub s ((Objet.setSpeed objTemp (0.0,scene.gravitie))::listRes) temp
        |Some ((xNew,yNew),(xsNew,ysNew)) ->
	  let objTemp = Objet.allowJump (Objet.move x (xNew,yNew)) in
          let temp = if (Objet.getGenre x) = Personnage then Camera.move (Objet.getPos x) cam else cam in  
	  moveAll_sub s ((Objet.setSpeed (Objet.resetSpeed objTemp) ((0.0 +. xsNew),(scene.gravitie +. ysNew)))::listRes) temp
      end
    (* Ne rien faire pour les autres objets *)
    |x2::s -> moveAll_sub s (x2::listRes) cam
      in
      let (temp,tempCam) = moveAll_sub scene.entities [] scene.cam in 
      let sceneTemp = {scene with entities = temp; cam = tempCam}in
      {sceneTemp with entities = changeAnim sceneTemp.entities}
 
  (* gestion des déplacements *)

          
  let movePersonnage scene (xs,ys) =
    let rec changePerso listObjet listRes =
      match listObjet with
      |[] -> listRes
      |x::s -> 
        if (Objet.getGenre x) = Personnage then 
          if (Objet.canJump x) then
            changePerso s (((Objet.setSpeed x (xs,ys)))::listRes)
          else
             changePerso s ((Objet.setSpeed x (xs,0.0))::listRes)
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
       if ((Objet.getPV x) < 1) then 
         if ((Objet.getGenre x) = Personnage) then
           exit 0
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

  let addEntitie scene objet =
    {scene with entities =  (objet::scene.entities)}

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
