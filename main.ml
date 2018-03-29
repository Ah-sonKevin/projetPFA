open Tsdl
open Objet
open Scene

let my_exit (window,renderer) scene =
  Scene.closeScene scene;
  Sdl.destroy_renderer renderer;
  Sdl.destroy_window window;
  Sdl.quit();
  exit 0

(*le jeu*)


let evenement e exit_arg scene =
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
  (*evenementFenetre e exit_arg;  *)
  Sdl. pump_events ();
  let tab = Sdl.get_keyboard_state () in
  
  begin 
    if tab.{(Sdl.get_scancode_from_key Sdl.K.escape)}=1 then
      exit 0;
    let x =  (0.2 *. (float_of_int (tab.{(Sdl.get_scancode_from_key Sdl.K.right)})))
 -. 0.2 *. float_of_int (tab.{(Sdl.get_scancode_from_key Sdl.K.left)}) in
    let y = float_of_int (-10 * tab.{(Sdl.get_scancode_from_key Sdl.K.up )}) in
    Scene.movePersonnage scene (x,y)
  end


let jeu () =
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

             (*charegement des éléments de jeu*)
           let perso = Objet.create Personnage (500,200) (0.0,0.0) (4.0,5.0) 100 "Image/samus/Samus_face_23_40.bmp" (23,40) render in
           let sprite = Objet.create Wall (200,500) (0.0,0.0) (0.0,0.0) 10000 "Image/sprite_obstacle.bmp" (231,89) render in
           let plateform = Objet.create Plateforme (600,500) (0.0,0.0) (0.0,0.0) 10000 "Image/Plateforme_700_5.bmp" (200,5) render in
           let background = Objet.create Background (-100,0) (0.0,0.0) (0.0,0.0) 10000 "Image/Background_2.bmp" (2560,1540) render in
           let sol = Objet.create Plateforme (0,668) (0.0,0.0) (0.0,0.0) 10000  "Image/Plateforme_700_5.bmp" (2560,5) render in
           let scene = Scene.create (sol::plateform::sprite::perso::[]) 1.06 background render in 
           
          
             (* gestion du jeu une fois lancé *)
           let event = Sdl.Event.create() in
           
           let rec game scene window renderer =
             let sceneEvent = evenement event (window,renderer) scene in
             let sceneActive = Scene.moveAll sceneEvent in
             Scene.refresh scene sceneActive;
             Sdl.delay 17l;
             game sceneActive window renderer in
           game scene window render

;;
let main () = jeu () ;;

let () = main();;
