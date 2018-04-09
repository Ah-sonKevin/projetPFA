open Tsdl
open Objet
open Scene
open Camera
open Anim

module type Menu = sig 
  val my_exit : Sdl.window * Sdl.renderer -> unit 
  val evenement : Sdl.window * Sdl.renderer -> bool -> bool option
  val startMenu : Sdl.window  ->  Sdl.renderer -> unit
end

module Menu = struct
  
  let my_exit (window,renderer) =
    Sdl.destroy_renderer renderer;
    Sdl.destroy_window window;
    Sdl.quit();
    exit 0

  (*le jeu*)
  let evenement exit_arg bool =
    Sdl. pump_events ();
    let tab = Sdl.get_keyboard_state () in    
    begin 
      if ((tab.{(Sdl.get_scancode_from_key Sdl.K.q)} = 1)) then
        let _ = exit 0 in None
      else
      if ((tab.{(Sdl.get_scancode_from_key Sdl.K.return)} = 1) || ((tab.{(Sdl.get_scancode_from_key Sdl.K.space)} = 1))) then 
        None
      else
        if (tab.{(Sdl.get_scancode_from_key Sdl.K.up )} = 1) then 
           Some true
        else
          if (tab.{(Sdl.get_scancode_from_key Sdl.K.down )} = 1) then 
            Some false
          else
            Some bool
    end

  let startMenu window render  =
    (*chargement des éléments de jeu*)
    let background = Objet.create Background (0,0) (0.0,0.0) (0.0,0.0) 10000  (Anim.create [||] [|"Image/Menu_backscreen_1200_900.bmp"|]  [||] [||] render ) render in
    let jouer    = Objet.create Wall (200,100) (0.0,0.0) (0.0,0.0) 10000 (Anim.create [||] [|"Image/jouer.bmp"|] [||] [||] render)  render in 
    let quitter  = Objet.create Wall (200,400) (0.0,0.0) (0.0,0.0) 10000 (Anim.create [||] [|"Image/quitter.bmp"|] [||] [||] render)  render in
    let jouer2   = Objet.create Wall (200,100) (0.0,0.0) (0.0,0.0) 10000 (Anim.create [||] [|"Image/jouer2.bmp"|] [||] [||] render)  render in 
    let quitter2 = Objet.create Wall (200,400) (0.0,0.0) (0.0,0.0) 10000 (Anim.create [||] [|"Image/quitter2.bmp"|] [||] [||] render)  render in
    
    let refresh jouerBool renderer =
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
          let tempJouer = if jouerBool   then jouer2 else jouer in 
          let tempQuitter = if jouerBool then quitter else quitter2 in
          loadPicture renderer (Objet.getPos tempJouer) (Objet.getSize tempJouer) (Objet.getTexture tempJouer);
          loadPicture renderer (Objet.getPos tempQuitter) (Objet.getSize tempQuitter) (Objet.getTexture tempQuitter);
          Sdl.render_present renderer
        end
    in
    
    (* gestion du jeu une fois lancé *)
    let rec sub window renderer bool =
      refresh bool renderer;
      Sdl.delay 17l;
      let temp  = evenement (window,renderer) bool in 
      match temp with 
      |Some b -> sub window renderer b
      |None when bool = true -> ()
      |None -> exit 0 
    in sub window render true 

end
