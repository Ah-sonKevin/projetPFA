open Tsdl

module type Tools = sig
  val loadPicture : Sdl.renderer -> int * int -> int * int -> Sdl.texture -> unit
  val drawRect : int * int -> int * int -> Sdl.renderer -> unit
  val drawFillRect : int * int -> int * int -> Sdl.renderer -> unit 
  val chooseColor  : int -> int -> int -> int -> Sdl.renderer   -> unit
end
  
module Tools = struct

  let loadPicture renderer (x1,y1) (w,h) texture =
    let frag_rect = Sdl.Rect.create 0 0 w h in
    let position_background = Sdl.Rect.create x1 y1 w h in
    match Sdl.render_copy ~dst:position_background ~src:frag_rect renderer texture with
    |Error (`Msg e) -> Sdl.log "Init texture on screen error: %s" e; exit 1
    |Ok () -> ()

  let drawRect (x1,y1) (w,h) render =
    match Sdl.render_draw_rect render (Some (Sdl.Rect.create x1 y1  w h)) with
    |Error (`Msg e) -> Sdl.log "Init loading error: %s" e; exit 1
    |Ok () ->  ()
       
  let drawFillRect (x1,y1) (w,h) render =
    match Sdl.render_fill_rect render (Some (Sdl.Rect.create x1 y1  w h)) with
    |Error (`Msg e) -> Sdl.log "Init loading error: %s" e; exit 1
    |Ok () ->  ()

  let chooseColor  r g b a  render =
    match (Sdl.set_render_draw_color render r g b a) with
    |Error (`Msg e) -> Sdl.log "Init loading error: %s" e; exit 1
    |Ok () ->  ()


end
