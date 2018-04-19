open Tsdl
open Objet
open Scene
open Camera
open Tsdl_mixer
open Anim
open Tools
open Sound

module type Menu = sig 
  type choix
  type soundSetting
  val my_exit : Sdl.window * Sdl.renderer -> unit 
  val evenement : Sdl.window * Sdl.renderer -> choix -> Sound.sound -> choix option
  val startMenu : Sdl.window  ->  Sdl.renderer -> Sound.sound  ->unit
end

module Menu = struct
  type choix = Game|Exit|Config|Key
  type soundSetting = Music|SoundEffect
  
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
	  print_string "bwark";
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

  let evenementSousMenuCommande son = 
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
      
  let evenementSousMenuConfig son soundSetting=    
    Sdl. pump_events ();
    let tab = Sdl.get_keyboard_state () in    
    begin
      if ((tab.{(Sdl.get_scancode_from_key Sdl.K.return)} = 1) || ((tab.{(Sdl.get_scancode_from_key Sdl.K.space)} = 1))) then 
        begin
          Sound.play_sound Sound.MenuO son;
          None
        end
      else
	let x =   5*(tab.{(Sdl.get_scancode_from_key Sdl.K.right)} - tab.{(Sdl.get_scancode_from_key Sdl.K.left)}) in	
	if x != 0 then
	  match soundSetting with
	  |Music ->
	     begin
	       let vol = Tsdl_mixer.Mixer.volume_music (-1) in
	       let _ = Tsdl_mixer.Mixer.volume_music (vol +x) in
	       Some soundSetting
	     end
	  |soundEffect ->
	     begin
	       let vol = Tsdl_mixer.Mixer.volume (-1) (-1) in
	       let _ = Tsdl_mixer.Mixer.volume (-1) (vol +x) in
	       Sound.play_sound Sound.MenuO son;
	       Some soundSetting
	     end
	else if (tab.{(Sdl.get_scancode_from_key Sdl.K.up )} = 1) then 
          begin
            Sound.play_sound Sound.MenuC son;
            match soundSetting with
	    |Music -> Some SoundEffect
	    |soundEffect -> Some Music
          end
	else if (tab.{(Sdl.get_scancode_from_key Sdl.K.down )} = 1) then 
          begin
            Sound.play_sound Sound.MenuC son;
            match soundSetting with
	    |Music -> Some SoundEffect
	    |soundEffect -> Some Music
          end	     
	else
	  Some soundSetting
    end
      
  let startMenu window render son  =
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
                     (Anim.create [||] [|"Image/moveKey.bmp"|] [||] [||] render)  render in 
    let mapKey  = Objet.create Background (200,400) (0.0,0.0) (0.0,0.0) 10000 
                    (Anim.create [||] [|"Image/mapKey.bmp"|] [||] [||] render)  render in

    
    let rec menuCommande renderer son = 
      match Sdl.render_clear renderer with
      |Error (`Msg e) -> Sdl.log "Init render error: %s" e; exit 1
      |Ok () ->
        begin
          Tools.loadPicture renderer (0,0) (Objet.getSize background_sousMenu) (Objet.getTexture background_sousMenu);
          Tools.loadPicture renderer (0,0) (Objet.getSize shootKey) (Objet.getTexture shootKey);
          Tools.loadPicture renderer (0,0) (Objet.getSize moveKey) (Objet.getTexture moveKey);
          Tools.loadPicture renderer (0,0) (Objet.getSize mapKey) (Objet.getTexture mapKey);
	  Sdl.render_present renderer;
	  Sdl.delay 150l;
          if (evenementSousMenuCommande son ) then ()
          else menuCommande renderer son            
        end
    in

    let rec menuConfig renderer son soundSetting =
      match Sdl.render_clear renderer with
      |Error (`Msg e) -> Sdl.log "Init render error: %s" e; exit 1
      |Ok () ->
	 begin
	   let (sw,sh) = Sdl.get_window_size window in
	   let volM = float_of_int (Tsdl_mixer.Mixer.volume_music (-1)) in
	   let ratioVolM = volM/.(float_of_int Tsdl_mixer.Mixer.max_volume) in

	   let volE = float_of_int (Tsdl_mixer.Mixer.volume (-1)(-1)) in
	   let ratioVolE = volE/.(float_of_int Tsdl_mixer.Mixer.max_volume) in
	   
	   begin
	     Tools.loadPicture renderer (0,0) (Objet.getSize background_sousMenu) (Objet.getTexture background_sousMenu);
	     Tools.chooseColor 55 15 200 255 renderer;	          
	     Tools.drawFillRect (50,50) (int_of_float ((float_of_int (sw-100))*.ratioVolM),50) renderer;    
	     Tools.drawFillRect (50,200) (int_of_float ((float_of_int (sw-100))*.ratioVolE),50) renderer;
	     Tools.chooseColor 255 255 255 255 renderer;
	     Tools.drawRect (50,50) (sw-100,50) render;
	     Tools.drawRect (50,200) (sw-100,50) render;
	     Sdl.render_present render;
	     Sdl.delay 150l;
             match evenementSousMenuConfig son soundSetting with
	     |None -> ()
	     |Some b -> menuConfig renderer son b
	   end
	 end
      in

    
    let refresh choice renderer =      
      match Sdl.render_clear renderer with
      |Error (`Msg e) -> Sdl.log "Init render error: %s" e; exit 1
      |Ok () ->
        begin         
          Tools.loadPicture renderer (0,0) (Objet.getSize background) (Objet.getTexture background);
           let (game,key,config,ext) = 
            match choice with 
            |Game -> (jouer2,key,config,quitter)
            |Key -> (jouer,key2,config,quitter)
            |Config -> (jouer,key,config2,quitter)
            |Exit -> (jouer,key,config,quitter2)
           in
          Tools.loadPicture renderer (Objet.getPos game) (Objet.getSize game) (Objet.getTexture game);
          Tools.loadPicture renderer (Objet.getPos ext) (Objet.getSize ext) (Objet.getTexture ext);
          Tools.loadPicture renderer (Objet.getPos key) (Objet.getSize key) (Objet.getTexture key);
          Tools.loadPicture renderer (Objet.getPos config) (Objet.getSize config) (Objet.getTexture config);
          Sdl.render_present renderer
        end
    in    
    (* gestion du jeu une fois lancé *)
    let rec sub window renderer choice son soundSetting =
      refresh choice renderer;
      Sdl.delay 150l;
      let temp  = evenement (window,renderer) choice son in    
      match temp with 
      |Some b -> sub window renderer b son soundSetting
      |None when choice = Game -> begin print_string "bwirk";()end
      |None when choice = Key -> 
         begin
	   menuCommande render son;
           sub window renderer choice son soundSetting
        end
      |None when choice = Config -> 
         begin
    	   menuConfig render son soundSetting;
          sub window renderer choice son soundSetting
        end
      |None when choice = Exit -> exit 0
      |None -> failwith "n'existe pas" 
    in sub window render Game son Music;

    
end
