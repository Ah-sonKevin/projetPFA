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

module Sound : Sound 
