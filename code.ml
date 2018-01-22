open Tsdl

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
	   match Sdl.load_bmp "Image/Menu_backscreen_1160_870.bmp" with
	   | Error (`Msg e) -> Sdl.log "Init window error: %s" e; exit 1
	   | Ok menu_back ->
	     match Sdl.create_texture_from_surface render menu_back with
	     | Error (`Msg e) -> Sdl.log "Init window error: %s" e; exit 1
	     | Ok menu_back_texture ->
		match Sdl.query_texture menu_back_texture with
		|Error (`Msg e) -> Sdl.log "Init window error: %s" e; exit 1
		|Ok (_,_,(w,h)) ->
		   let position_background = Sdl.Rect.create 0 0 w h in
		   match Sdl.render_copy ~dst:position_background render menu_back_texture with
		     |Error (`Msg e) -> Sdl.log "Init window error: %s" e; exit 1
		     |Ok () -> Sdl.render_present render;




	     Sdl.render_present render;
	     Sdl.delay 5000l;
	     Sdl.free_surface menu_back;
	     Sdl.destroy_renderer render;
	     Sdl.destroy_window window;
	     Sdl.quit();
	     exit 0
let main () =
  affichage ()      


let () = main()
