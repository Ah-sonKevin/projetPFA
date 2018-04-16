open Tsdl
open Objet
open Anim
open Scene

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
    (*evenementFenetre e (window,render); *)
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

  let drawRect (x1,y1) (w,h) render =
    match Sdl.render_draw_rect render (Some (Sdl.Rect.create x1 y1  w h)) with
    |Error (`Msg e) -> Sdl.log "Init loading error: %s" e; exit 1
    |Ok () ->  ()

  let drawFillRect (x1,y1) (w,h) render =
    match Sdl.render_fill_rect render (Some (Sdl.Rect.create x1 y1  w h)) with
    |Error (`Msg e) -> Sdl.log "Init loading error: %s" e; exit 1
    |Ok () ->  ()

  let chooseColor  r g b a  render =
    match (Sdl.set_render_draw_color render r g b a) with
    |Error (`Msg e) -> Sdl.log "Init loading error: %s" e; exit 1
    |Ok () ->  ()

  let selectColor g render=
    match g with
    |Objet.Personnage -> chooseColor  0 255  0 255 render
    |Wall ->   chooseColor 255 255 255 255  render
    |Door _ -> chooseColor 0 0 255 255 render
    |Ennemi -> chooseColor 255 0 0 255 render
    |Plateforme -> chooseColor 100 0 100 255 render 
    |Projectile -> chooseColor 0 0 0 255 render
    | _ -> ()
    
      

  let draw scene window render =
    let (ws,hs) = Scene.getSize scene in
    let (sw1,sw2) = Sdl.get_window_size window in
    drawRect (50,50) (sw1-100,sw2-100) render;
    let ratioX = (float_of_int (sw1-100)) /. (float_of_int ws) in
    let ratioY =(float_of_int (sw2-100))/. (float_of_int hs) in
    List.iter (fun x ->
      selectColor (Objet.getGenre x) render;
      let (xo,yo) = Objet.getPos x in
      let (wo,ho) = Objet.getSize x in
      drawFillRect (50 + (int_of_float ((float_of_int xo)*.ratioX)), 50 + (int_of_float ((float_of_int yo)*.ratioY)))
       (int_of_float ((float_of_int wo) *. ratioX),(int_of_float  ((float_of_int ho) *. ratioY))) render) (Scene.getEntitie scene)


  let drawMini scene window render =
    let (ws,hs) = Scene.getSize scene in
    let (sw1,sw2) = (200.0,140.0)in
    drawRect (0,0) (200,140) render;
    let ratioX = sw1 /. (float_of_int ws) in
    let ratioY = sw2 /. (float_of_int hs) in
    List.iter (fun x ->
      selectColor (Objet.getGenre x) render;
      let (xo,yo) = Objet.getPos x in
      let (wo,ho) = Objet.getSize x in
      drawFillRect ( (int_of_float ((float_of_int xo)*.ratioX)), (int_of_float ((float_of_int yo)*.ratioY)))
       (int_of_float ((float_of_int wo) *. ratioX),(int_of_float  ((float_of_int ho) *. ratioY))) render) (Scene.getEntitie scene)

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
    let background = Objet.create Background (0,0) (0.0,0.0) (0.0,0.0) 10000 
                       (Anim.create [||] [|"Image/map200.bmp"|]  [||] [||] render ) render in    
    let refresh renderer =
      let loadPicture renderer (x1,y1) (w,h) texture =
        let frag_rect = Sdl.Rect.create 0 0 w h in
        let position_background = Sdl.Rect.create x1 y1 w h in
        match Sdl.render_copy ~dst:position_background ~src:frag_rect renderer texture with
        |Error (`Msg e) -> Sdl.log "Init texture on screen error: %s" e; exit 1
        |Ok () -> ()
      in
      begin
        loadPicture renderer (0,0) (Objet.getSize background) (Objet.getTexture background);
	drawMini scene window render(*;
        Sdl.render_present renderer*)
      end
    in    
    (* gestion du jeu une fois lancé *)
    let event = Sdl.Event.create() in  
    let rec sub window renderer bool =
      refresh renderer;
    in sub window render true

	

  


	
end
  
