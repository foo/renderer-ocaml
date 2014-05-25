(* autor: Maciej Pacut *)

let int (f : float) = int_of_float f;;
let float (i : int) = float_of_int i;;

let max a b = if a > b then a else b;;
let min a b = if a > b then b else a;;

let positive x = max 0.0 x;;

let sum lst = List.fold_right (+.) lst 0.0;;
let average lst = (sum lst) /. (float (List.length lst));;

(* operacje na wektorach *)
type vector = float * float * float;;

let (+|) (x1, y1, z1) (x2, y2, z2) =
  (x1 +. x2, y1 +. y2, z1 +. z2);;

let reflect (x,y,z) =
  (-.x,-.y,-.z);;

let (-|) a b =
  a +| (reflect b);;

let ( *| ) (x, y, z) scalar =
  (x *. scalar, y *. scalar, z *. scalar);;

let normalize (x, y, z) =
  let len = sqrt (x *. x +. y *. y +. z *. z) in
    (x /. len, y /. len, z/. len);;

let dot_product (x1, y1, z1) (x2, y2, z2) =
  let (nx2, ny2, nz2) = normalize (x2, y2, z2) in
    x1 *. nx2 +. y1 *. ny2 +. z1 *. nz2;;

let cross_product (x1, y1, z1) (x2, y2, z2) =
  (y1 *. z2 -. y2 *. z1,
   -.x1 *. z2 +. x2 *. z1,
   x1 *. y2 -. x2 *. y1);;

let mul_vector_by_scalar (x, y, z) s =
  (x *. s, y *. s, z *. s);;

let normal_vector (x1, y1, z1) (x2, y2, z2) (x3, y3, z3) =
  let a = (x1 -. x2, y1 -. y2, z1 -. z2)
  and b = (x2 -. x3, y2 -. y3, z2 -. z3) in
    normalize (cross_product a b);;
