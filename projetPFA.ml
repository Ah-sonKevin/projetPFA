(* ocamlbuild -use-ocamlfind -package tsdl,tsdl_mixer projetPFA.byte *)

open Tsdl
open Objet

let background = ref [];;
  
let my_exit (window,renderer) =
  let rec texture background =
    match !background with
    |[] -> background := [];
    |t::list -> Sdl.destroy_texture t;
      background := list;
       texture background;
  in
  texture background;
  Sdl.destroy_renderer renderer;
  Sdl.destroy_window window;
  Sdl.quit();
  exit 0;;



let load_picture_full (window,renderer) (x,y) source=
   match Sdl.load_bmp source with
     | Error (`Msg e) -> Sdl.log "Init load picture error: %s" e; exit 1
     | Ok surface_temp ->
	 match Sdl.create_texture_from_surface renderer surface_temp with
	  | Error (`Msg e) -> Sdl.log "Init surface to texture error: %s" e; exit 1
	  | Ok name -> background := name::(!background);
	     Sdl.free_surface surface_temp;
	    match Sdl.query_texture name with
	     |Error (`Msg e) -> Sdl.log "Init query texture error: %s" e; exit 1
	     |Ok (_,_,(w,h)) ->
	       let position = Sdl.Rect.create x y (w+x) (h+y) in
		 match Sdl.render_copy ~dst:position renderer name with
		  |Error (`Msg e) -> Sdl.log "Init texture on screen error: %s" e; exit 1
		  |Ok () -> Sdl.render_present renderer
;;


let evenement e perso exit_arg =
  Sdl.destroy_texture (Objet.getTexture !perso);
  load_picture_full exit_arg (Objet.getPos !perso) "Image/perso.bmp";
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
       |`Key_down ->
	  begin
	    let touche = Sdl.Scancode.enum (Sdl.Event.get e Sdl.Event.keyboard_scancode) in 	    
	    match touche  with
	    |`Escape -> my_exit exit_arg
	    |`Left  ->let (x,y) = Objet.getPos !perso in perso := (Objet.move !perso (x-1,y))	      
	    |`Right ->let (x,y) = Objet.getPos !perso in perso := (Objet.move !perso (x+1,y))	       
	    |`Up    -> let (x,y) = Objet.getPos !perso in perso := (Objet.move !perso (x,y-1))	       
	    |`Down  -> let (x,y) = Objet.getPos !perso in perso := (Objet.move !perso (x,y+1))  		 
	    |_->print_string "Evenement clavier non géré\n"
	  end	  
       |_-> ()
     end
  |false-> ()
;;

let load_picture_fragment (window,renderer) (x1,y1) (x2,y2,x3,y3) source =
   match Sdl.load_bmp source with
     | Error (`Msg e) -> Sdl.log "Init load picture error: %s" e; exit 1
     | Ok surface_temp ->
	 match Sdl.create_texture_from_surface renderer surface_temp with
	  | Error (`Msg e) -> Sdl.log "Init surface to texture error: %s" e; exit 1
	  | Ok name -> background := name::(!background);
	    Sdl.free_surface surface_temp;
	    let frag_rect = Sdl.Rect.create x2 y2 x3 y3 in
	    let position_background = Sdl.Rect.create x1 y1 ((x3-x2)+x1) ((y3-y2)+y1) in
		 match Sdl.render_copy ~dst:position_background ~src:frag_rect renderer name with
		  |Error (`Msg e) -> Sdl.log "Init texture on screen error: %s" e; exit 1
		  |Ok () -> Sdl.render_present renderer
;;

let affichage () =
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
	     (* gestion de l'image de fond*)
	   load_picture_full(window,render) (0,0) "Image/Menu_backscreen_1160_870.bmp";
	   let perso = ref (Objet.create Personnage (0,0) (100,100) 100 "Image/perso.bmp" (100,100)) in	  
	     (* gestion d'un évènement *)
	     let event = Sdl.Event.create() in
	     while (true) do
	       evenement event perso (window,render);
	       let (x,y) = Objet.getPos !perso in
	       Printf.printf "%d %d" x y ;
	     done
;;


let main () = affichage () ;;



let () = main();;

