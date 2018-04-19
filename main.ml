open Tsdl
open Objet
open Scene
open Camera 
open Menu
open Tsdl_mixer
open Sound
open GameMap
open Random

exception ErreurScene

let my_exit (window,renderer) scene =
  Scene.closeScene scene;
  Sdl.destroy_renderer renderer;
  Sdl.destroy_window window;
  Sdl.quit();
  exit 0

(*le jeu*)
let evenement e window renderer scene =
  let evenementFenetre e exit_arg = 
  match Sdl.poll_event (Some e) with
  |true->
    begin
      match Sdl.Event.enum (Sdl.Event.get e Sdl.Event.typ) with
      |`Window_event ->
        begin
          let nom = Sdl.Event.window_event_enum (Sdl.Event.get e Sdl.Event.window_event_id ) in
          match nom with
          |`Close -> my_exit exit_arg scene
          |_ -> ()
        end
      | _ -> ()
    end
  |false -> ()
  in 
  (*evenementFenetre e exit_arg; *)
  Sdl. pump_events ();
  let tab = Sdl.get_keyboard_state () in
  
  begin 
    if tab.{(Sdl.get_scancode_from_key Sdl.K.escape)}=1 then
      exit 0;
    if tab.{(Sdl.get_scancode_from_key Sdl.K.m)}=1 then
      GameMap.startMap window renderer scene;
    let temp1 = 
      if tab.{(Sdl.get_scancode_from_key Sdl.K.r)}=1 then
        Scene.suicide scene
      else
        scene
    in
    let x =  (0.2 *. (float_of_int (tab.{(Sdl.get_scancode_from_key Sdl.K.right)})))
             -. 0.2 *. float_of_int (tab.{(Sdl.get_scancode_from_key Sdl.K.left)}) in
    let y = float_of_int (-10 * tab.{(Sdl.get_scancode_from_key Sdl.K.up )}) in
    let xt = (tab.{(Sdl.get_scancode_from_key Sdl.K.d)})-(tab.{(Sdl.get_scancode_from_key Sdl.K.q)}) in
    let yt = (tab.{(Sdl.get_scancode_from_key Sdl.K.s)})-(tab.{(Sdl.get_scancode_from_key Sdl.K.z)}) in
    
    let temp2 =  if ((xt != 0)||(yt!=0)) then Scene.shoot (Scene.getPers scene) temp1 (xt,yt) else scene in
    Scene.movePersonnage temp2 (x,y)
  end


let jeu () =
  Random.self_init ();
  (*gestion de l'ouverture de la fenetre*)
    match Sdl.init Sdl.Init.video with
    | Error (`Msg e) -> Sdl.log "Init error: %s" e; exit 1
    | Ok () ->
      match Sdl.create_window  ~w:1000  ~h:700 "Metroidvania"  Sdl.Window.shown with
      | Error (`Msg e) -> Sdl.log "Init window error: %s" e; exit 1
      | Ok window ->
        match Sdl.create_renderer window ~index:(-1) ~flags:Sdl.Renderer.(accelerated + presentvsync) with
        | Error (`Msg e) -> Sdl.log "Init render error: %s" e; exit 1
        | Ok render ->
          Sdl.render_present render;

          let son = Sound.init ()
          in
          let genererScene t pers render  =
            let (l,g,b,t,p) = Lexer.lex t pers render in
            (Scene.create l g b
               (Camera.create (Objet.getPos p) (Objet.getSize b) (Sdl.get_window_size window))
               render son t)
            in
            (* gestion du jeu une fois lancé *)
            let event = Sdl.Event.create() in     
            let rec menu window renderer = 
              Sound.play_mus "Son/kingdomHeartMenuMusic.wav";	    
              Menu.startMenu window renderer son;	     
              let scene = genererScene "scene1_begin.txt" None render in 
              let rec game scene wesyindow renderer =
		let sceneTemp = Scene.decreaseClock scene in 
                let sceneEvent = evenement event  window renderer sceneTemp in
                let sceneMove = Scene.moveAll sceneEvent in
		Sdl.delay 17l;
	        let sceneActive = Scene.refreshLifebar (Scene.kickDead sceneMove) in
                Scene.refresh scene sceneActive;
                GameMap.startMapMini window renderer scene;
                Sdl.render_present renderer;
              if Scene.continue sceneActive then 
                game sceneActive window renderer
              else ()
              in
              game scene window render;
              menu window renderer 
            in menu window render
               
let main () = jeu ()

let () = main()
