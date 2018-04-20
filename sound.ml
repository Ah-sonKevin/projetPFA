open Tsdl
open Tsdl_mixer

module  type Sound = sig 
  type effet = Tir|Saut|Degat|MenuC|MenuO|PowerUp
  val loadChunk : string -> Tsdl_mixer.Mixer.chunk
  val play_mus  : string -> unit
  val play_a_sound : Tsdl_mixer.Mixer.chunk -> unit
  val play_sound : effet ->  unit 
  val close : unit -> unit
  val soundMenuO : Tsdl_mixer.Mixer.chunk
  val soundMenuC : Tsdl_mixer.Mixer.chunk
  val soundShoot : Tsdl_mixer.Mixer.chunk
  val soundJump : Tsdl_mixer.Mixer.chunk 
  val soundDommage : Tsdl_mixer.Mixer.chunk
  val soundPU : Tsdl_mixer.Mixer.chunk
  val init : unit
end

module Sound : Sound = struct 

  type effet = Tir|Saut|Degat|MenuC|MenuO|PowerUp
                              
  let init  =
    match (Tsdl_mixer.Mixer.open_audio 44100 Tsdl_mixer.Mixer.default_format  Tsdl_mixer.Mixer.default_channels 1024) with 
    |Error (`Msg e) -> Sdl.log "Init opening error: %s" e; exit 1
    |Ok () ->  ignore (Tsdl_mixer.Mixer.allocate_channels 100 100)      

         
  let loadChunk s = 
    match (Tsdl_mixer.Mixer.load_wav s) with
    |Error (`Msg e) -> Sdl.log "Init opening error: %s" e; exit 1
    |Ok t -> t
             

  let soundShoot = loadChunk "Son/tir.wav" 
  let soundJump = loadChunk "Son/saut.wav" 
  let soundDommage = loadChunk "Son/degat.wav"    
  let soundMenuC = loadChunk "Son/menuChoice.wav" 
  let soundMenuO = loadChunk "Son/menuOK.wav" 
  let soundPU = loadChunk "Son/powerUp.wav" 
                 

        
  let play_mus mus =  
    if Tsdl_mixer.Mixer.playing_music () then 
      begin
        match Tsdl_mixer.Mixer.halt_music () with 
        |Error (`Msg e) -> Sdl.log "Init loading error: %s" e; exit 1
        |Ok () -> ()
      end;
        match (Tsdl_mixer.Mixer.load_mus mus ) with
        |Error (`Msg e) -> Sdl.log "Init loading error: %s" e; exit 1
        |Ok m -> 
          begin
            match (Tsdl_mixer.Mixer.play_music m (-1))  with 
            |Error (`Msg e) -> Sdl.log "Init playing error: %s" e; exit 1
            |Ok x -> ()
          end
        
  let play_a_sound chunk = 
    match (Tsdl_mixer.Mixer.play_channel (-1) chunk 0) with
    |Error (`Msg e) -> Sdl.log "Init playing error: %s" e; exit 1
    |Ok x -> ()

  let play_sound effet = 
    match effet with 
    |Tir     -> play_a_sound soundShoot
    |Saut    -> play_a_sound soundJump
    |PowerUp -> play_a_sound soundPU
    |Degat   -> play_a_sound soundDommage
    |MenuC   -> play_a_sound soundMenuC
    |MenuO   -> play_a_sound soundMenuO

  let close ()  =
    Tsdl_mixer.Mixer.free_chunk soundShoot;
    Tsdl_mixer.Mixer.free_chunk soundDommage;
    Tsdl_mixer.Mixer.free_chunk soundJump;
    Tsdl_mixer.Mixer.close_audio ();
end
