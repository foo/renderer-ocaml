(*
  autor: Maciej Pacut
  zbiór czysto funkcyjnych funkcji poruszających modelem
  to funkcje typu (model -> model); nie modyfikują współrzędnych tylko tworzą nową kopię modelu
*)

open Math;;

(* najprostsze "przekształcenie" *)
let identity x = x;;

(* model to lista trójkątów, to pomocnicza funkcja aplikująca przekształcenie do każdego wierzchołka każdego trójkąta *)
let transform vertex_transformation model =
  List.map (fun (v1, v2, v3) -> (vertex_transformation v1,
				 vertex_transformation v2,
				 vertex_transformation v3)) model;;

(* aplikuje przesunięcie do wszystkich wierzchołków trójkąta *)
let move (vx, vy, vz) model =
  transform (fun (x, y, z) ->
	       (x +. vx, y +. vy, z +. vz)) model;;

(* ruch wzdłuż współrzędnej x *)
let move_left delta = move (-.delta, 0.0, 0.0);;
let move_right delta = move (delta, 0.0, 0.0);;

(* ruch wzdłuż współrzędnej y *)
let move_up delta = move (0.0, delta, 0.0);;
let move_down delta = move (0.0, -.delta, 0.0);;

(* ruch wzdłuż współrzędnej z *)
let move_closer delta = move (0.0, 0.0, -.delta);;
let move_further delta = move (0.0, 0.0, delta);;

let average_position model =
  let x_coords = List.map (fun ((x1,_,_),(x2,_,_),(x3,_,_)) -> (x1 +. x2 +. x3) /. 3.0) model (* liczy średnie z 3 wierzchołków trójkąta *)
  and y_coords = List.map (fun ((_,y1,_),(_,y2,_),(_,y3,_)) -> (y1 +. y2 +. y3) /. 3.0) model
  and z_coords = List.map (fun ((_,_,z1),(_,_,z2),(_,_,z3)) -> (z1 +. z2 +. z3) /. 3.0) model
  in (average x_coords,
      average y_coords,
      average z_coords);;


(*
  obroty wg stałych osi
  funkcje rotujące przesuwają model do punktu 0,0,0
  następnie wykonują obrót i przywracają do punktu początkowego;
  w przeciwnym przypadku model nie obracał się wg własnego środka, ale wokół punktu 0,0,0
*)

let rotate_x angle model =
  let (cx, cy, cz) = (average_position model) in
    transform (fun (x, y, z) ->
		 let (dx, dy, dz) = (cx -. x, cy -. y, cz -. z) (* przywrócenie obiektu do środka układu współrzędnych *)
		 in (x,
		     dy *. (cos angle) -. dz *. (sin angle) +. cy,
		     dy *. (sin angle) +. dz *. (cos angle) +. cz)) model;;


let rotate_y angle model =
  let (cx, cy, cz) = (average_position model) in
    transform (fun (x, y, z) ->
		 let (dx, dy, dz) = (cx -. x, cy -. y, cz -. z) (* przywrócenie obiektu do środka układu współrzędnych *)
		 in (dx *. (cos angle) +. dz *. (sin angle) +. cx,
		     y,
		     dz *. (cos angle) -. dx *. (sin angle) +. cz)) model;;

let rotate_z angle model =
  let (cx, cy, cz) = (average_position model) in
    transform (fun (x, y, z) ->
		 let (dx, dy, dz) = (cx -. x, cy -. y, cz -. z) (* przywrócenie obiektu do środka układu współrzędnych *)
		 in (dx *. (cos angle) -. dy *. (sin angle) +. cx,
		     dx *. (sin angle) +. dy *. (cos angle) +. cy,
		     z)) model;;

