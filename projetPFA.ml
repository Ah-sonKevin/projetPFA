(* ocamlbuild -use-ocamlfind -package tsdl,tsdl_mixer projetPFA.byte *)

open Tsdl

let evenement e =
  match Sdl.poll_event (Some e) with
  |true->
     begin
       match Sdl.Event.enum (Sdl.Event.get e Sdl.Event.typ) with        
       |`Window_event ->
	  begin
	    let nom = Sdl.Event.window_event_enum (Sdl.Event.get e Sdl.Event.window_event_id ) in
	    match nom with
	    |`Close -> exit 0
	    |_->print_string  "Evenement fenetre non géré :  \n"
	  end
       |`Key_up ->
	  begin
	    let touche = Sdl.Scancode.enum (Sdl.Event.get e Sdl.Event.keyboard_scancode) in 	    
	    match touche  with
	    |`Escape -> exit 0 
	    |_->print_string "Evenement clavier non géré\n"
	  end	  
       |_->print_string "Evenement inconnu non géré\n "
     end
  |false-> ()

     
     
 
  
let main () =
  match Sdl.init Sdl.Init.video  with
  |Error (`Msg e) ->  Sdl.log "erreur %s" e ;exit 1
  |Ok () ->
     match Sdl.create_window "projet" ~w:500 ~h:500 Sdl.Window.resizable with 
     |Error (`Msg e ) -> Sdl.log "erreur %s " e       ;
       exit 1
     |Ok w ->
	match Sdl.create_renderer w ~index:(-1)  ~flags:Sdl.Renderer.accelerated with
	|Error(`Msg e) -> Sdl.log "erreur %s" e ;
	  exit 1
	|Ok r ->
	   match Sdl.set_render_draw_color r 120 20 120 20 with
	   |Error (`Msg e ) -> Sdl.log "Erreur %s " e ;
	     exit 1
	   |Ok () -> match Sdl.render_clear r with
	     |Error (`Msg e ) -> Sdl.log "Erreur %s " e ;
	       exit  1
	     |Ok () -> Sdl.render_present r ;
	       match Sdl.set_render_draw_color r 0 0 0 0 with
	       |Error (`Msg e ) -> Sdl.log "Erreur %s " e ;
		 exit 1		 
	       |Ok () ->
		  let rect = Sdl.Rect.create 0 0 250 250 in
		  Sdl.render_fill_rect r (Some rect);
		  Sdl.render_present r;
		  let e = Sdl.Event.create () in 
		  while(true) do		    
		    evenement e 
		  (* evenement () *)		    
		  done;
		  exit 0

		  
let () = main ()
