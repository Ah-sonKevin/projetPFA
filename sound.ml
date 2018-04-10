open Tsdl
open Tsdl_mixer

module  type Sound = sig 
  type sound
  type effet = Tir|Saut|Degat|MenuC|MenuO
  val loadChunk : string -> Tsdl_mixer.Mixer.chunk
  val init : unit -> sound
  val play_mus  : string -> unit
  val play_a_sound : Tsdl_mixer.Mixer.chunk -> unit
  val play_sound : effet -> sound -> unit 
  val close : sound -> unit 
end

module Sound : Sound = struct 

  type sound = {soundMenuO : Tsdl_mixer.Mixer.chunk; soundMenuC : Tsdl_mixer.Mixer.chunk; soundTir : Tsdl_mixer.Mixer.chunk; soundSaut : Tsdl_mixer.Mixer.chunk; soundDegat : Tsdl_mixer.Mixer.chunk ; channels : int;  } 
  type effet = Tir|Saut|Degat|MenuC|MenuO
                              
  let loadChunk s = 
    match (Tsdl_mixer.Mixer.load_wav s) with
    |Error (`Msg e) -> Sdl.log "Init opening error: %s" e; exit 1
    |Ok t -> t
             

  let init () =
    match (Tsdl_mixer.Mixer.open_audio 44100 Tsdl_mixer.Mixer.default_format  Tsdl_mixer.Mixer.default_channels 1024) with 
    |Error (`Msg e) -> Sdl.log "Init opening error: %s" e; exit 1
    |Ok () ->  
      let t = loadChunk "Son/tir.wav" in
      let s = loadChunk "Son/saut.wav" in
      let d = loadChunk "Son/tir.wav" in   
      let m_c = loadChunk "Son/menuChoice.wav" in
      let m_o = loadChunk "Son/menuOK.wav" in
      {soundMenuC = m_c; soundMenuO = m_o ;soundTir = t ; soundSaut = s; soundDegat =d ; channels = (Tsdl_mixer.Mixer.allocate_channels 100 100)}
        
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

  let play_sound effet sound = 
    match effet with 
    |Tir   ->  play_a_sound sound.soundTir
    |Saut  ->  play_a_sound sound.soundSaut
    |Degat ->  play_a_sound sound.soundDegat
    |MenuC -> play_a_sound sound.soundMenuC
    |MenuO -> play_a_sound sound.soundMenuO

  let close sound =
    Tsdl_mixer.Mixer.free_chunk sound.soundTir;
    Tsdl_mixer.Mixer.free_chunk sound.soundDegat;
    Tsdl_mixer.Mixer.free_chunk sound.soundSaut;
    Tsdl_mixer.Mixer.close_audio ();
end
