open Tsdl

let my_exit (window,renderer,background) =
   Sdl.destroy_texture background;
   Sdl.destroy_renderer renderer;
   Sdl.destroy_window window;
   Sdl.quit();
   exit 0

let evenement e exit_arg =
  match Sdl.poll_event (Some e) with
  |true->
     begin
       match Sdl.Event.enum (Sdl.Event.get e Sdl.Event.typ) with        
       |`Window_event ->
	  begin
	    let nom = Sdl.Event.window_event_enum (Sdl.Event.get e Sdl.Event.window_event_id ) in
	    match nom with
	    |`Close -> my_exit exit_arg
	    |_->print_string  "Evenement fenetre non géré :  \n"
	  end
       |`Key_up ->
	  begin
	    let touche = Sdl.Scancode.enum (Sdl.Event.get e Sdl.Event.keyboard_scancode) in 	    
	    match touche  with
	    |`Escape -> my_exit exit_arg
	    |_->print_string "Evenement clavier non géré\n"
	  end	  
       |_->print_string "Evenement inconnu non géré\n "
     end
  |false-> ()

let load_picture (window,renderer,background) =
   match Sdl.load_bmp "Image/Menu_backscreen_1160_870.bmp" with
     | Error (`Msg e) -> Sdl.log "Init load picture error: %s" e; exit 1
     | Ok surface_temp ->
	 match Sdl.create_texture_from_surface render surface_temp with
	  | Error (`Msg e) -> Sdl.log "Init surface to texture error: %s" e; exit 1
	  | Ok background -> Sdl.free_surface surface_temp 
	    match Sdl.query_texture background with
	     |Error (`Msg e) -> Sdl.log "Init query texture error: %s" e; exit 1
	     |Ok (_,_,(w,h)) ->
	       let position_background = Sdl.Rect.create 0 0 w h in
		 match Sdl.render_copy ~dst:position_background renderer background with
		  |Error (`Msg e) -> Sdl.log "Init texture on screen error: %s" e; exit 1
		  |Ok () -> Sdl.render_present renderer;
     
let affichage () =
   match Sdl.init Sdl.Init.video with
  | Error (`Msg e) -> Sdl.log "Init error: %s" e; exit 1
  | Ok () ->
     match Sdl.create_window  ~w:1160  ~h:870 "Metroidvania"  Sdl.Window.shown with
      | Error (`Msg e) -> Sdl.log "Init window error: %s" e; exit 1
      | Ok window ->
	 match Sdl.create_renderer window ~index:(-1) ~flags:Sdl.Renderer.(accelerated + presentvsync) with
	 | Error (`Msg e) -> Sdl.log "Init render error: %s" e; exit 1
	 | Ok render ->
	    Sdl.render_present render;
	     (* gestion de l'image de fond*)
	    load_picture(window,render,fond)
	  (* match Sdl.load_bmp "Image/Menu_backscreen_1160_870.bmp" with
	   | Error (`Msg e) -> Sdl.log "Init load picture error: %s" e; exit 1
	   | Ok menu_back ->
	     match Sdl.create_texture_from_surface render menu_back with
	     | Error (`Msg e) -> Sdl.log "Init surface to texture error: %s" e; exit 1
	     | Ok menu_back_texture -> Sdl.free_surface menu_back 
		match Sdl.query_texture menu_back_texture with
		|Error (`Msg e) -> Sdl.log "Init query texture error: %s" e; exit 1
		|Ok (_,_,(w,h)) ->
		   let position_background = Sdl.Rect.create 0 0 w h in
		   match Sdl.render_copy ~dst:position_background render menu_back_texture with
		     |Error (`Msg e) -> Sdl.log "Init window error: %s" e; exit 1
	     |Ok () -> Sdl.render_present render;*)
	     (*fin de la gestion de l'image de fond*)

	     (* gestion d'un évènement *)
	     
	     let event = Sdl.Event.create() in
	     while (true) do
	       evenement event (window,render,menu_back)
	     done

let main () =
  affichage ()      


let () = main()
