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
  val evenement : Sdl.window * Sdl.renderer -> choix -> choix option
  val startMenu : Sdl.window  ->  Sdl.renderer   ->unit
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
  let evenement exit_arg choice =
    Sdl. pump_events ();
    let tab = Sdl.get_keyboard_state () in    
    begin 
      if ((tab.{(Sdl.get_scancode_from_key Sdl.K.return)} = 1) || ((tab.{(Sdl.get_scancode_from_key Sdl.K.space)} = 1))) then 
        begin
          Sound.play_sound Sound.MenuO ;
          None
        end
      else if (tab.{(Sdl.get_scancode_from_key Sdl.K.up )} = 1) then 
        begin
          Sound.play_sound Sound.MenuC ;
          Some (choiceUp choice)
        end
      else if (tab.{(Sdl.get_scancode_from_key Sdl.K.down )} = 1) then 
        begin
          Sound.play_sound Sound.MenuC ;
          Some (choiceDown choice)
        end
      else
        Some choice
    end

  let evenementSousMenuCommande () = 
    Sdl. pump_events ();
    let tab = Sdl.get_keyboard_state () in    
    begin
      if ((tab.{(Sdl.get_scancode_from_key Sdl.K.return)} = 1) || ((tab.{(Sdl.get_scancode_from_key Sdl.K.space)} = 1))) then 
        begin
          Sound.play_sound Sound.MenuO ;
          true
        end
      else
        false
    end
      
  let evenementSousMenuConfig  soundSetting=    
    Sdl. pump_events ();
    let tab = Sdl.get_keyboard_state () in    
    begin
      if ((tab.{(Sdl.get_scancode_from_key Sdl.K.return)} = 1) || ((tab.{(Sdl.get_scancode_from_key Sdl.K.space)} = 1))) then 
        begin
          Sound.play_sound Sound.MenuO ;
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
	       Sound.play_sound Sound.MenuO ;
	       Some soundSetting
	     end
	else if (tab.{(Sdl.get_scancode_from_key Sdl.K.up )} = 1) then 
          begin
            Sound.play_sound Sound.MenuC ;
            match soundSetting with
	    |Music -> Some SoundEffect
	    |soundEffect -> Some Music
          end
	else if (tab.{(Sdl.get_scancode_from_key Sdl.K.down )} = 1) then 
          begin
            Sound.play_sound Sound.MenuC ;
            match soundSetting with
	    |Music -> Some SoundEffect
	    |soundEffect -> Some Music
          end	     
	else
	  Some soundSetting
    end
      
  let startMenu window render   =
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
 

    let touches  = Objet.create Background (300,50) (0.0,0.0) (0.0,0.0) 10000 
                    (Anim.create [||] [|"Image/touches.bmp"|] [||] [||] render)  render in
    let moveKey   = Objet.create Background (50,150) (0.0,0.0) (0.0,0.0) 10000
                     (Anim.create [||] [|"Image/moveKey.bmp"|] [||] [||] render)  render in 
    let shootKey    = Objet.create Background (50,250) (0.0,0.0) (0.0,0.0) 10000 
                     (Anim.create [||] [|"Image/shootKey.bmp"|] [||] [||] render)  render in 
    let mapKey  = Objet.create Background (50,350) (0.0,0.0) (0.0,0.0) 10000 
                    (Anim.create [||] [|"Image/mapKey.bmp"|] [||] [||] render)  render in
    let suicideKey  = Objet.create Background (50,450) (0.0,0.0) (0.0,0.0) 10000 
                    (Anim.create [||] [|"Image/suicideKey.bmp"|] [||] [||] render)  render in
    let reglageMus  = Objet.create Background (300,50) (0.0,0.0) (0.0,0.0) 10000 
                    (Anim.create [||] [|"Image/reglageSon.bmp"|] [||] [||] render)  render in
    let reglageEffet  = Objet.create Background (300,400) (0.0,0.0) (0.0,0.0) 10000 
                    (Anim.create [||] [|"Image/reglageEffet.bmp"|] [||] [||] render)  render in
    let back  = Objet.create Background (400,650) (0.0,0.0) (0.0,0.0) 10000 
                       (Anim.create [||] [|"Image/back.bmp"|] [||] [||] render)  render in

  
    let rec menuCommande renderer = 
      match Sdl.render_clear renderer with
      |Error (`Msg e) -> Sdl.log "Init render error: %s" e; exit 1
      |Ok () ->
        begin
          Tools.loadPicturePosObj renderer background_sousMenu;
          Tools.loadPicturePosObj renderer touches;
          Tools.loadPicturePosObj renderer shootKey;
          Tools.loadPicturePosObj renderer moveKey;
          Tools.loadPicturePosObj renderer mapKey;
          Tools.loadPicturePosObj renderer suicideKey;
          Tools.loadPicturePosObj renderer back;
	  Sdl.render_present renderer;
	  Sdl.delay 150l;
          if (evenementSousMenuCommande () ) then ()
          else menuCommande renderer             
        end
    in

    let rec menuConfig renderer soundSetting =
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
	     Tools.loadPicturePosObj renderer background_sousMenu;
             Tools.loadPicturePosObj renderer reglageMus;
             Tools.loadPicturePosObj renderer  reglageEffet;
             Tools.loadPicturePosObj renderer back;
	     Tools.chooseColor 55 15 200 255 renderer;	          
	     Tools.drawFillRect (50,200) (int_of_float ((float_of_int (sw-100))*.ratioVolM),50) renderer;    
	     Tools.drawFillRect (50,550) (int_of_float ((float_of_int (sw-100))*.ratioVolE),50) renderer;
             begin
               match soundSetting with 
               |Music       -> Tools.chooseColor 0 255 0 255 renderer
               |SoundEffect ->Tools.chooseColor 255 255 255 255 renderer
             end;
	     Tools.drawRect (50,200) (sw-100,50) render;
             begin
               match soundSetting with 
               |Music       -> Tools.chooseColor 255 255 255 255 renderer
               |SoundEffect ->Tools.chooseColor 0 255 0 255 renderer
             end;	   
	     Tools.drawRect (50,550) (sw-100,50) render;
	     Sdl.render_present render;
	     Sdl.delay 150l;
             match evenementSousMenuConfig  soundSetting with
	     |None -> ()
	     |Some b -> menuConfig renderer  b
	   end
	 end
      in

    
    let refresh choice renderer =      
      match Sdl.render_clear renderer with
      |Error (`Msg e) -> Sdl.log "Init render error: %s" e; exit 1
      |Ok () ->
        begin         
          Tools.loadPicturePosObj renderer  background;
           let (game,key,config,ext) = 
            match choice with 
            |Game -> (jouer2,key,config,quitter)
            |Key -> (jouer,key2,config,quitter)
            |Config -> (jouer,key,config2,quitter)
            |Exit -> (jouer,key,config,quitter2)
           in
          Tools.loadPicturePosObj renderer game;
          Tools.loadPicturePosObj renderer ext;
          Tools.loadPicturePosObj renderer key;
          Tools.loadPicturePosObj renderer config;
          Sdl.render_present renderer
        end
    in    
    (* gestion du jeu une fois lancé *)
    let rec sub window renderer choice  soundSetting =
      refresh choice renderer;
      Sdl.delay 150l;
      let temp  = evenement (window,renderer) choice  in    
      match temp with 
      |Some b -> sub window renderer b  soundSetting
      |None when choice = Game -> begin print_string "bwirk";()end
      |None when choice = Key -> 
         begin
	   menuCommande render ;
           sub window renderer choice  soundSetting
        end
      |None when choice = Config -> 
         begin
    	   menuConfig render  soundSetting;
          sub window renderer choice  soundSetting
        end
      |None when choice = Exit -> exit 0
      |None -> failwith "n'existe pas" 
    in sub window render Game Music;

    
end
