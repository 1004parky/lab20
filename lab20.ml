open Graphics ;;
open List ;; 

(*helper function that takes an image and a function and applies the function to
every element in the image*)
let apply_func (f : float -> float) =
  map (fun row -> map f row)
  
(* threshold threshold image -- image where pixels above the threshold
value are black *)
let threshold img threshold =
  apply_func (fun v -> if v <= threshold 
                  then 0. 
                  else 1.) img
       
(* dither max image -- dithered image *)
let dither =
  apply_func (fun v -> if v > Random.float 1.
                 then 1.
                 else 0.)

(* show the image *)
let depict img =
  open_graph ""; 
  let x, y = length (hd img), length img in resize_window x y;
  let depict_pix v r c = 
    let lvl = int_of_float (255. *. (1. -. v)) in set_color (rgb lvl lvl lvl);
    plot c (y - r) in
  iteri (fun r row -> iteri (fun c pix -> depict_pix pix r c) row) img;
  Unix.sleep 2; close_graph () ;;

let run = 
  let mona = Monalisa.image in
  depict mona ;
  depict (threshold mona 0.75) ;
  depict (dither mona) ;; 
           
run ;; 
