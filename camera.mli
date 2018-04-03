open Tsdl

module type Camera = sig 
  type camera
  val create : (int * int) -> (int * int ) -> (int * int) -> camera
  val convertPosObjet : (int * int) -> camera -> (int * int )
  val convertPosBackground : camera -> (int * int )
  val move :  (int * int) -> camera -> camera
end

module Camera : Camera
