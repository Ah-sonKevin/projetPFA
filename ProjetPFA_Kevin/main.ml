open Tsdl
open Objet
open Scene
open Camera
open Menu

let my_exit (window,renderer) scene =
  Scene.closeScene scene;
  Sdl.destroy_renderer renderer;
  Sdl.destroy_window window;
  Sdl.quit();
  exit 0

(*le jeu*)
let evenement e exit_arg scene clock =
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
  (*evenementFenetre e exit_arg;*) 
  Sdl. pump_events ();
  let tab = Sdl.get_keyboard_state () in
  
  begin 
    if tab.{(Sdl.get_scancode_from_key Sdl.K.escape)}=1 then
      exit 0;
    let temp1 = 
      if tab.{(Sdl.get_scancode_from_key Sdl.K.r)}=1 then
        Scene.suicide scene
      else
        scene
    in
    let x =  (0.2 *. (float_of_int (tab.{(Sdl.get_scancode_from_key Sdl.K.right)})))
             -. 0.2 *. float_of_int (tab.{(Sdl.get_scancode_from_key Sdl.K.left)}) in
    let y = float_of_int (-20 * tab.{(Sdl.get_scancode_from_key Sdl.K.up )}) in
    let xt = (tab.{(Sdl.get_scancode_from_key Sdl.K.d)})-(tab.{(Sdl.get_scancode_from_key Sdl.K.q)}) in
    let yt = (tab.{(Sdl.get_scancode_from_key Sdl.K.s)})-(tab.{(Sdl.get_scancode_from_key Sdl.K.z)}) in
    let temp2 = Scene.shoot temp1 (xt,yt) clock in
    Scene.movePersonnage temp2 (x,y)
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
          let perso = Objet.create Personnage (500,200) (0.0,0.0) (4.0,5.0) 100 
                       ( [|"Image/samus/Samus_left/Samus_left_walk/Samus_left_walk1_25_38.bmp";
                          "Image/samus/Samus_left/Samus_left_walk/Samus_left_walk2_25_38.bmp";
                          "Image/samus/Samus_left/Samus_left_walk/Samus_left_walk3_21_38.bmp";
                          "Image/samus/Samus_left/Samus_left_walk/Samus_left_walk4_20_39.bmp";
                          "Image/samus/Samus_left/Samus_left_walk/Samus_left_walk5_20_38.bmp";
                          "Image/samus/Samus_left/Samus_left_walk/Samus_left_walk6_26_38.bmp";
                          "Image/samus/Samus_left/Samus_left_walk/Samus_left_walk7_25_38.bmp";
                          "Image/samus/Samus_left/Samus_left_walk/Samus_left_walk8_23_38.bmp";
                          "Image/samus/Samus_left/Samus_left_walk/Samus_left_walk9_21_38.bmp";
                          "Image/samus/Samus_left/Samus_left_walk/Samus_left_walk10_18_38.bmp"
                        |], 
                        [|"Image/samus/Samus_face_23_40.bmp"|] ,
                        [|
                          "Image/samus/Samus_right/Samus_right_walk/Samus_right_walk1_19_38.bmp";
                          "Image/samus/Samus_right/Samus_right_walk/Samus_right_walk2_21_38.bmp";
                          "Image/samus/Samus_right/Samus_right_walk/Samus_right_walk3_23_38.bmp";
                          "Image/samus/Samus_right/Samus_right_walk/Samus_right_walk4_25_38.bmp";
                          "Image/samus/Samus_right/Samus_right_walk/Samus_right_walk5_26_38.bmp";
                          "Image/samus/Samus_right/Samus_right_walk/Samus_right_walk6_20_38.bmp";
                          "Image/samus/Samus_right/Samus_right_walk/Samus_right_walk7_20_39.bmp";
                          "Image/samus/Samus_right/Samus_right_walk/Samus_right_walk8_21_38.bmp";
                          "Image/samus/Samus_right/Samus_right_walk/Samus_right_walk9_25_38.bmp";
                          "Image/samus/Samus_right/Samus_right_walk/Samus_right_walk10_18_38.bmp"
                        |],
                        [|"Image/samus/Samus_saut/Samus_saut8_16_16.bmp";
                         "Image/samus/Samus_saut/Samus_saut7_16_16.bmp";
                         "Image/samus/Samus_saut/Samus_saut6_16_16.bmp";
                         "Image/samus/Samus_saut/Samus_saut5_16_16.bmp";
                         "Image/samus/Samus_saut/Samus_saut4_16_16.bmp";
                         "Image/samus/Samus_saut/Samus_saut3_16_16.bmp";
                         "Image/samus/Samus_saut/Samus_saut2_16_16.bmp";
                         "Image/samus/Samus_saut/Samus_saut1_16_16.bmp"|])
                       (23,40) render in
	  let ennemi = Objet.create Ennemi (300,500) (3.0,0.0) (5.0,0.0) 200 ([||], [|"Image/ennemies/ennemi_test_24_14.bmp"|] ,[||],[||]) (24,20) render in
          let sprite = Objet.create Wall (200,500) (0.0,0.0) (0.0,0.0) 10000 ([||], [|"Image/sprite_obstacle.bmp"|] ,[||],[||]) (231,89) render in
          let plateform = Objet.create Plateforme (600,500) (0.0,0.0) (0.0,0.0) 10000  ([||], [|"Image/Plateforme_700_5.bmp"|],  [||],[||]) (200,5) render in
          let background = Objet.create Background (0,0) (0.0,0.0) (0.0,0.0) 10000  ([||], [|"Image/Background_2.bmp"|] , [||],[||]) (3494,982) render in
          let sol = Objet.create Plateforme (-100,668) (0.0,0.0) (0.0,0.0) 10000   ([||], [|"Image/Plateforme_700_5.bmp"|],  [||],[||]) (3600,5) render in
          let cam = Camera.create (Objet.getPos perso) (Objet.getSize background) (Sdl.get_window_size window) in
          let scene = Scene.create (sol::ennemi::plateform::sprite::perso::[]) 0.15 background cam render in
         
          (* gestion du jeu une fois lancé *)
          let event = Sdl.Event.create() in     
          let rec menu window renderer = 
            Menu.startMenu window renderer;
            let rec game scene window renderer clock =
              Sdl.delay 17l;
              let sceneEvent = evenement event (window,renderer) scene clock in
              let sceneMove = Scene.moveAll sceneEvent in
	      let sceneActive = Scene.kickDead sceneMove in
              Scene.refresh scene sceneActive;
              if Scene.continue sceneActive then 
                game sceneActive window renderer ((clock+1) mod 5)
              else ()
            in
            game scene window render 0;
            menu window renderer 
          in menu window render
               
let main () = jeu ()

let () = main()
