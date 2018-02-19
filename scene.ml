open Tsdl
open Objet

module type Scene = sig
  type scene
    
  val create : Objet.objet list -> int -> Objet.objet -> Sdl.renderer -> scene
  val getEntitie : scene -> Objet.objet list
  val getTexture : scene -> Sdl.texture list
  val getGravitie : scene -> int
  val addEntitie : scene -> Objet.objet -> scene
  val generateScene : scene -> Objet.objet list -> scene
  val applyGravitie : scene -> scene
  val moveAll : scene -> scene
  val movePersonnage : scene -> (int*int) -> scene
  val loadPicture : Sdl.renderer -> (int*int) -> (int*int) -> Sdl.texture -> unit
  val refresh : scene -> scene -> unit
  val closeScene : scene -> unit
    
    
end

module Scene : Scene =  struct
  type scene = {entities:Objet.objet list ; gravitie:int ; background : Objet.objet ; renderer : Sdl.renderer}
    
  let create objs grav back render = {entities = objs ; gravitie = grav ; background = back ; renderer = render}
    
  let getEntitie scene = scene.entities
    
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

  let applyGravitie scene =
    let rec setAll listObjet listRes =
      match listObjet with
      |[]-> listRes
      |x::s -> setAll s ((Objet.setSpeed x (0,scene.gravitie))::listRes)
    in
    {scene with entities = (setAll scene.entities [] ) }
    
      
  let moveAll scene =
    let rec moveAll_sub listObjet listRes =
      match listObjet with
      |[]-> listRes
      |x::s -> moveAll_sub s ((Objet.move x)::listRes)
    in
    let temp = applyGravitie scene in
    {temp with entities = (moveAll_sub temp.entities [] ) }
  
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
	       |Ok () -> loadPicture sceneNew.renderer (0,0) (1160,870) (Objet.getTexture sceneNew.background);
		  refresh_sub sceneNew.entities;
		  Sdl.render_present sceneNew.renderer
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
  ;;
  
end
  
