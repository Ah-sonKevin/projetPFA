open Tsdl

module type Camera = sig 
  type camera
  val create : (int * int) -> (int * int ) -> (int * int) -> camera
  val convertPosObjet : (int * int) -> camera -> (int * int )
  val convertPosBackground : camera -> (int * int )
  val move :  (int * int) -> camera -> camera
  val getWindowSize : camera -> (int * int)
end

module Camera : Camera = struct 
  type camera = {posCamera : int * int; backgroundSize : int * int ; windowSize : int * int}
  let create pos sizeBack sizeW =
    {posCamera = pos;
     backgroundSize = sizeBack;
     windowSize=sizeW}

  let move (x,y) cam = { cam with posCamera = (x,y)}
  let getWindowSize cam = cam.windowSize

  (* convertie la position de l'objet sur la scene en la position de l'objet sur l'ecran*)
  let convertPosObjet (x,y) cam =
    let (xw,yw) = cam.windowSize in 
    let (xPer,yPer) = cam.posCamera in 
    let (sbx,sby) = cam.backgroundSize in 
    let xPers = 
      if xPer >sbx - xw/2 then  sbx - xw/2
      else if xPer < xw/2 then xw/2 else xPer
    in
    let (yPers) = 
      if yPer >sby - yw/2 then 
        (sby - yw/2)
      else if yPer < yw/2 then (yw/2) else yPer
    in
    (x+(-xPers+xw/2), y+(-yPers+yw/2))  (*position dans la camera *)

  let convertPosBackground cam = 
    let (xw,yw) = cam.windowSize in 
    let (xPers,yPers) = cam.posCamera in 
    let (sbx,sby) = cam.backgroundSize in 
    let xr = (xPers-xw/2) in
    let xf = 
      if xr <0 then 0 
      else if xr > sbx - xw then sbx - xw (*distance DebutScene DebutCamera*)
      else xr 
    in
    let yr = (yPers-yw/2) in
    let yf = 
      if yr <0 then 0 
      else if yr > sby - yw then sby - yw (*distance DebutScene DebutCamera*)
      else yr 
    in
    (-xf,-yf) 

end
