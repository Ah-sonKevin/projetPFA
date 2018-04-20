open Tsdl
open Objet
open Anim
open Scene
open Tools

module type GameMap = sig
  val evenement :  Sdl.event ->  Sdl.window -> Sdl.renderer ->  bool -> bool
  val startMap : Sdl.window ->  Sdl.renderer -> Scene.scene ->  unit 
  val startMapMini : Sdl.window ->  Sdl.renderer -> Scene.scene ->  unit 
end
  
module GameMap : GameMap  = struct

  let my_exit (window,renderer)  =
    Sdl.destroy_renderer renderer;
    Sdl.destroy_window window;
    Sdl.quit();
    exit 0
      
  let evenement e window render bool =
    let evenementFenetre e exit_arg = 
      match Sdl.poll_event (Some e) with
      |true->
	 begin
	   match Sdl.Event.enum (Sdl.Event.get e Sdl.Event.typ) with
	   |`Window_event ->
              begin
		let nom = Sdl.Event.window_event_enum (Sdl.Event.get e Sdl.Event.window_event_id ) in
		match nom with
		|`Close -> my_exit exit_arg 
		|_ -> ()
              end
	   | _ -> ()
	 end
      |false -> ()
    in 
    evenementFenetre e (window,render); 
    Sdl. pump_events ();
    let tab = Sdl.get_keyboard_state () in    
    begin
      if tab.{(Sdl.get_scancode_from_key Sdl.K.escape)}=1 then
	exit 0;
      if (tab.{(Sdl.get_scancode_from_key Sdl.K.m )} = 1) then 
	false
      else
        bool
    end

  let selectColor g render=
    match g with
    |Objet.Personnage -> Tools.chooseColor  0 255  0 255 render
    |Wall _ ->   Tools.chooseColor 255 255 255 255  render
    |Door _ -> Tools.chooseColor 0 0 255 255 render
    |Ennemi _  -> Tools.chooseColor 255 0 0 255 render
    |Plateforme _ -> Tools.chooseColor 100 0 100 255 render 
    |Projectile -> Tools.chooseColor 0 0 0 255 render
    | _ -> ()
    
 let draw scene window render =
    Tools.chooseColor  255 255  255 255 render;
    let (sceneWidth,sceneHeight) = Scene.getSize scene in
    let (windowWidth,windowHeight) = Sdl.get_window_size window in
    Tools.drawRect (50,50) (windowWidth-100,windowHeight-100) render;
    let ratioX = (float_of_int (windowWidth-100)) /. (float_of_int sceneWidth) in
    let ratioY =(float_of_int (windowHeight-100))/. (float_of_int sceneHeight) in
    List.iter (fun x ->
      selectColor (Objet.getGenre x) render;
      let (objX,objY) = Objet.getPos x in
      let (objWidth,objHeight) = Objet.getSize x in
      Tools.drawFillRect (50 + (int_of_float ((float_of_int objX)*.ratioX)), 50 + (int_of_float ((float_of_int objY)*.ratioY)))
    (int_of_float ((float_of_int objWidth) *. ratioX),(int_of_float  ((float_of_int objHeight) *. ratioY))) render) (Scene.getEntitie scene)

  let drawMini scene window render sizeMap =
    Tools.chooseColor  255 255  255 255 render;
    let (sceneWidth,sceneHeight) = Scene.getSize scene in
    let (windowWidthInt,windowHeightInt) = sizeMap in
    let (windowWidthFloat,windowHeightFloat) = (float_of_int windowWidthInt, float_of_int windowHeightInt) in
    Tools.drawRect (0,0) (windowWidthInt,windowHeightInt) render;
    let ratioX = windowWidthFloat /. (float_of_int sceneWidth)  in
    let ratioY =  windowHeightFloat /. (float_of_int sceneHeight)  in
    List.iter (fun x ->
      selectColor (Objet.getGenre x) render;
      let (objX,objY) = Objet.getPos x in
      let (objWidthTemp,objHeightTemp) = Objet.getSize x in
      let (objWidth,objHeight) = (int_of_float (ceil( (float_of_int objWidthTemp)*.ratioX)),int_of_float ( ceil ( (float_of_int objHeightTemp)*.ratioY))) in
      let (x,y) = (((float_of_int (objX))*.ratioX),((float_of_int (objY))*.ratioY)) in
      Tools.drawFillRect (int_of_float x, int_of_float y)  (objWidth,objHeight) 	
	  render) (Scene.getEntitie scene)

  let startMap window render scene =
    (*chargement des éléments de jeu*)
    let background = Objet.create Background (0,0) (0.0,0.0) (0.0,0.0) 10000 
                       (Anim.create [||] [|"Image/map.bmp"|]  [||] [||] render ) render in    
    let refresh renderer =
      match Sdl.render_clear renderer with
      |Error (`Msg e) -> Sdl.log "Init render error: %s" e; exit 1
      |Ok () ->
         let loadPicture renderer (x1,y1) (w,h) texture =
           let frag_rect = Sdl.Rect.create 0 0 w h in
           let position_background = Sdl.Rect.create x1 y1 w h in
           match Sdl.render_copy ~dst:position_background ~src:frag_rect renderer texture with
           |Error (`Msg e) -> Sdl.log "Init texture on screen error: %s" e; exit 1
           |Ok () -> ()
         in
         begin
           loadPicture renderer (0,0) (Objet.getSize background) (Objet.getTexture background);
	   draw scene window render;
           Sdl.render_present renderer
         end
    in    
    (* gestion du jeu une fois lancé *)
    let event = Sdl.Event.create() in  
    let rec sub window renderer bool =
      refresh renderer;
      Sdl.delay 17l;
      let temp  = evenement event window renderer bool  in
      if temp then
	sub window renderer bool
      else ()
    in sub window render true


  let startMapMini window render scene =
    (*chargement des éléments de jeu*)
    let background_map  = Objet.create Background (0,0) (0.0,0.0) (0.0,0.0) 10000 
                       (Anim.create [||] [|"Image/map250.bmp"|]  [||] [||] render ) render in    
    let refresh renderer =
      let loadPicture renderer (x1,y1) (w,h) texture =
        let frag_rect = Sdl.Rect.create 0 0 w h in
        let position_background_map = Sdl.Rect.create x1 y1 w h in
        match Sdl.render_copy ~dst:position_background_map ~src:frag_rect renderer texture with
        |Error (`Msg e) -> Sdl.log "Init texture on screen error: %s" e; exit 1
        |Ok () -> ()
      in
      begin
        loadPicture renderer (0,0) (Objet.getSize background_map) (Objet.getTexture background_map);
	drawMini scene window render (Objet.getSize background_map) (*;
        Sdl.render_present renderer*)
      end
    in    
    (* gestion du jeu une fois lancé *)
    let rec sub window renderer bool =
      refresh renderer;
    in sub window render true
end
  
