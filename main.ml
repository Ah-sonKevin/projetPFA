open Tsdl
open Objet
open Scene

let my_exit (window,renderer) scene =
  Scene.closeScene scene;
  Sdl.destroy_renderer renderer;
  Sdl.destroy_window window;
  Sdl.quit();
  exit 0;;

(*le jeu*)

let evenement e exit_arg scene =
  match Sdl.poll_event (Some e) with
  |true->
     begin
       match Sdl.Event.enum (Sdl.Event.get e Sdl.Event.typ) with        
       |`Window_event ->
	  begin
	    let nom = Sdl.Event.window_event_enum (Sdl.Event.get e Sdl.Event.window_event_id ) in
	    match nom with
	    |`Close -> begin my_exit exit_arg scene; scene end
	    |_->print_string  "Evenement fenetre non géré :  \n"; scene
	  end
       |`Key_down ->
	  begin
	    let touche = Sdl.Scancode.enum (Sdl.Event.get e Sdl.Event.keyboard_scancode) in
	    match touche with
	    |`Escape -> begin my_exit exit_arg scene; scene end
	    |`Left  -> Scene.movePersonnage scene ((-1),0)
	    |`Right -> Scene.movePersonnage scene (1,0)
	    |`Up    -> Scene.movePersonnage scene (0,(-10))
	    |`Down  -> Scene.movePersonnage scene (0,1)
	    |_-> print_string "Evenement clavier non géré\n"; scene
	  end	  
       |_-> scene
     end
  |false-> scene
;;

let jeu () =
  (*gestion de l'ouverture de la fenetre*)
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

	     (*charegement des éléments de jeu*)
	   let perso = Objet.create Personnage (400,0) (0,0) (5,10) 100 "Image/samus/Samus_face_23_40.bmp" (100,100) render in
	   let sprite = Objet.create Wall (200,200) (0,0) (0,0) 10000 "Image/sprite_obstacle.bmp" (100,50) render in
	   let background = Objet.create Background (0,0) (0,0) (0,0) 10000 "Image/Menu_backscreen_1160_870.bmp" (1160,870) render in
	   let scene = Scene.create (sprite::perso::[]) 1 background render in 
	  
	     (* gestion du jeu une fois lancé *)
	   let event = Sdl.Event.create() in
	   
	   let rec game scene window renderer =
	     let sceneEvent = evenement event (window,renderer) scene in
	     let sceneActive = Scene.moveAll sceneEvent in
	     Scene.refresh scene sceneActive;
	     game sceneActive window renderer in
	   game scene window render
	     
	       
	     
;;
let main () = jeu () ;;

let () = main();;
