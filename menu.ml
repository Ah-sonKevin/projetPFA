open Tsdl
open Objet
open Scene
open Camera
open Anim
open Sound

module type Menu = sig 
  type choix
  val my_exit : Sdl.window * Sdl.renderer -> unit 
  val evenement : Sdl.window * Sdl.renderer -> choix -> Sound.sound -> choix option
  val startMenu : Sdl.window  ->  Sdl.renderer -> Sound.sound ->unit
end

module Menu = struct
  type choix = Game|Exit|Config|Key
  
  let my_exit (window,renderer) =
    Sdl.destroy_renderer renderer;
    Sdl.destroy_window window;
    Sdl.quit();
    exit 0

  let choiceDown c =
    match c with
    |Game -> Key
    |Key -> Config
    |Config -> Exit
    |Exit -> Game

  let choiceUp c =
    match c with
    |Game -> Exit
    |Key -> Game
    |Config -> Key
    |Exit -> Config

  (*le jeu*)
  let evenement exit_arg choice son=
    Sdl. pump_events ();
    let tab = Sdl.get_keyboard_state () in    
    begin 
      if ((tab.{(Sdl.get_scancode_from_key Sdl.K.return)} = 1) || ((tab.{(Sdl.get_scancode_from_key Sdl.K.space)} = 1))) then 
        begin
          Sound.play_sound Sound.MenuO son;
          None
        end
      else if (tab.{(Sdl.get_scancode_from_key Sdl.K.up )} = 1) then 
        begin
          Sound.play_sound Sound.MenuC son;
          Some (choiceUp choice)
        end
      else if (tab.{(Sdl.get_scancode_from_key Sdl.K.down )} = 1) then 
        begin
          Sound.play_sound Sound.MenuC son;
          Some (choiceDown choice)
        end
      else
        Some choice
    end

  let evenementSousMenu son = 
    Sdl. pump_events ();
    let tab = Sdl.get_keyboard_state () in    
    begin
      if ((tab.{(Sdl.get_scancode_from_key Sdl.K.return)} = 1) || ((tab.{(Sdl.get_scancode_from_key Sdl.K.space)} = 1))) then 
        begin
          Sound.play_sound Sound.MenuO son;
          true
        end
      else
        false
    end
      
  let startMenu window render son =
    (*chargement des éléments de jeu*)
    let background = Objet.create Background (0,0) (0.0,0.0) (0.0,0.0) 10000  
                       (Anim.create [||] [|"Image/Menu_backscreen_1200_900.bmp"|]  [||] [||] render ) render in

    let jouer    = Objet.create Background (200,50) (0.0,0.0) (0.0,0.0) 10000 
                     (Anim.create [||] [|"Image/jouer.bmp"|] [||] [||] render)  render in 
    let jouer2   = Objet.create Background (200,50) (0.0,0.0) (0.0,0.0) 10000
                     (Anim.create [||] [|"Image/jouer2.bmp"|] [||] [||] render)  render in 
    let key  = Objet.create Background (200,200) (0.0,0.0) (0.0,0.0) 10000 
                 (Anim.create [||] [|"Image/commande.bmp"|] [||] [||] render)  render in
    let key2 = Objet.create Background (200,200) (0.0,0.0) (0.0,0.0) 10000 
                 (Anim.create [||] [|"Image/commande2.bmp"|] [||] [||] render)  render in
    let config  = Objet.create Background (200,350) (0.0,0.0) (0.0,0.0) 10000 
                    (Anim.create [||] [|"Image/config.bmp"|] [||] [||] render)  render in
    let config2 = Objet.create Background (200,350) (0.0,0.0) (0.0,0.0) 10000 
                    (Anim.create [||] [|"Image/config2.bmp"|] [||] [||] render)  render in
    let quitter  = Objet.create Background (200,600) (0.0,0.0) (0.0,0.0) 10000 
                     (Anim.create [||] [|"Image/quitter.bmp"|] [||] [||] render)  render in
    let quitter2 = Objet.create Background (200,600) (0.0,0.0) (0.0,0.0) 10000 
                     (Anim.create [||] [|"Image/quitter2.bmp"|] [||] [||] render)  render in
    let background_sousMenu = Objet.create Background (0,0)  (0.0,0.0) (0.0,0.0) 10000 
                                (Anim.create [||] [|"Image/map.bmp"|] [||] [||] render)  render in

    let shootKey    = Objet.create Background (200,10) (0.0,0.0) (0.0,0.0) 10000 
                     (Anim.create [||] [|"Image/shootKey.bmp"|] [||] [||] render)  render in 
    let moveKey   = Objet.create Background (200,250) (0.0,0.0) (0.0,0.0) 10000
                     (Anim.create [||] [|"Image/move.bmp"|] [||] [||] render)  render in 
    let mapKey  = Objet.create Background (200,400) (0.0,0.0) (0.0,0.0) 10000 
                    (Anim.create [||] [|"Image/mapKey.bmp"|] [||] [||] render)  render in

    
    let rec commande renderer son = 
      match Sdl.render_clear renderer with
      |Error (`Msg e) -> Sdl.log "Init render error: %s" e; exit 1
      |Ok () ->
        begin
          let loadPicture renderer (x1,y1) (w,h) texture =
            let frag_rect = Sdl.Rect.create 0 0 w h in
            let position_background = Sdl.Rect.create x1 y1 w h in
            match Sdl.render_copy ~dst:position_background ~src:frag_rect renderer texture with
            |Error (`Msg e) -> Sdl.log "Init texture on screen error: %s" e; exit 1
            |Ok () -> ()
          in
          loadPicture renderer (0,0) (Objet.getSize background_sousMenu) (Objet.getTexture background_sousMenu);
          loadPicture renderer (0,0) (Objet.getSize shootKey) (Objet.getTexture shootKey);
          loadPicture renderer (0,0) (Objet.getSize moveKey) (Objet.getTexture moveKey);
          loadPicture renderer (0,0) (Objet.getSize mapKey) (Objet.getTexture mapKey);
          if (evenementSousMenu son ) then ()
          else 
            begin
              Sdl.delay 50l;
              commande renderer son
            end
        end
    in

    
    let refresh choice renderer =
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
           let (game,key,config,ext) = 
            match choice with 
            |Game -> (jouer2,key,config,quitter)
            |Key -> (jouer,key2,config,quitter)
            |Config -> (jouer,key,config2,quitter)
            |Exit -> (jouer,key,config,quitter2)
           in
          loadPicture renderer (Objet.getPos game) (Objet.getSize game) (Objet.getTexture game);
          loadPicture renderer (Objet.getPos ext) (Objet.getSize ext) (Objet.getTexture ext);
          loadPicture renderer (Objet.getPos key) (Objet.getSize key) (Objet.getTexture key);
          loadPicture renderer (Objet.getPos config) (Objet.getSize config) (Objet.getTexture config);
          Sdl.render_present renderer
        end
    in    
    (* gestion du jeu une fois lancé *)
    let rec sub window renderer choice son =
      refresh choice renderer;
      Sdl.delay 150l;
      let temp  = evenement (window,renderer) choice son in 
      match temp with 
      |Some b -> sub window renderer b son
      |None when choice = Game -> ()
      |None when choice = Key -> 
        begin
          sub window renderer choice son
        end
      |None when choice = Config -> 
        begin
          commande render son;
          sub window renderer choice son
        end
      |None when choice = Exit -> exit 0
      |None -> failwith "n'existe pas" 
    in sub window render Game son

end
