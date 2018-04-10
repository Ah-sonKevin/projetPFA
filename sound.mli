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

module Sound : Sound 
