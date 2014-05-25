(* autor: Maciej Pacut *)

open Math;;
open Transformations;;

exception Break;; (* symulacja break loop z C *)

let (|>) x f = f x;;

type color = float * float * float;; (* światło ma ten sam typ co wektor, co pozwala używać sumowania i mnożenia przez skalar *)

let projection_d = 1.0;;

(* projekcja modelu na dwuwymiarową płaszczyznę ekranu *)
let flatten_vertex (x, y, z) =
  (600.0 *. projection_d *. x /. z +. (600.0 /. 2.0),
   600.0 *. projection_d *. y /. z +. (600.0 /. 2.0));;

let flatten_tri (a, b, c) =
  (flatten_vertex a,
   flatten_vertex b,
   flatten_vertex c);;

(* oświetlenie *)
type light = Ambient of color | Directed of vector * color;;

(*
  lista wszystkich świateł oświetlających model
  model jest oświetlany słabym światłem rozproszonym
  i dwoma światłami kierunkowymi - zielonym i niebieskim z rogów
*)
let lights = [
  Ambient (0.2, 0.2, 0.2); (* światło rozproszone oświetla tak samo każdy trójkąt niezależnia od położenia *)
  Directed ((0.5, 0.5, 0.0), (0.0, 0.5, 0.5)); (* światło kierunkowe oświetla trójkąt w zależności od kąta padania na niego *)
  Directed ((0.5, -.0.5, 0.0), (0.5, 0.5, 0.0));
];;

let lighten_tri (v1, v2, v3) =
  let values = List.map (function
			   | Ambient (r, g, b) -> (r, g, b)
			   | Directed (direction, (r,g,b)) ->
			       let diff = dot_product (normal_vector v1 v2 v3) direction
			       in (((positive diff) *. r,
				    (positive diff) *. g,
				    (positive diff) *. b)))
    lights in
  let (r, g, b) = List.fold_right (+|) values (0.0, 0.0, 0.0)
  in Graphics.set_color (Graphics.rgb
			   (int (r *. 255.0))
			   (int (g *. 255.0))
			   (int (b *. 255.0)));
    (v1,v2,v3);;

(*
  algorytm malarza, który jest zastosowany w moim silniku
  wymaga, aby trójkąty znajdujące się dalej zostały narysowane pierwsze
*)

let coordinate_z t1 t2 =
  let avg_z ((_,_,z1), (_,_,z2), (_,_,z3)) =
    (z1 +. z2 +. z3) /. 3.
  in avg_z t1 > avg_z t2;;  

(*
  wykrywanie niewidocznych trójkątów;
  trójkąt nie jest wyswietlany, gdy jego wektor normalny nie jest zwrócony w stronę kamery
  technika ta wymaga, aby model nie miał dziur, przez które można zajrzeć do jego środka
  oraz aby trójkąty były definiowane przez wierzchołki w kierunku przeciwnym do ruchu wskazówek zegara;
  tą ostatnią właśność zapewnia Blender
*)
let visible (v1,v2,v3) =
  let vector_of_view = (0.0,0.0,1.0) in
  let diff =
    (dot_product (normal_vector v1 v2 v3) vector_of_view)
  in diff < 1.0;;

let render_tri (v1,v2,v3) =
  Graphics.fill_poly (Array.map (fun (x,y) ->
				   (int x, int y)) [| v1; v2; v3 |]);;

(*
  rendering świata za pomocą algorytmu malarza
  to kluczowa część silnika, opisująca cały potok renderowania;
  operator |> działa jak unixowy pipe: $ grep xyz | wc | less
*)
let render_world list_of_triangles =
  list_of_triangles
	     |> List.filter visible
	     |> Sort.list coordinate_z
	     |> List.iter (fun single_triangle ->
			     single_triangle
			   |> lighten_tri
			   |> flatten_tri
			   |> render_tri);;

(*
  główna pętla renderująca
  nie modyfikuje obiektu, jedynie zwraca nowy, z nowymi współrzędnymi
*)
let rec rendering_loop model =
  Graphics.auto_synchronize false;

  Graphics.clear_graph ();

  render_world model;

  Graphics.set_color (Graphics.rgb 0 0 0);
  Graphics.moveto 0 0;
  Graphics.draw_string "W/S/A/D przesuwa model, Q/E obraca, R/F zbliza/oddala, X aby wyjsc";

  Graphics.auto_synchronize true;
  
  (*
    transformation ma typ funkcji częsciowo zaaplikowanej;
    rendering_loop jest wywoływane rekurencyjnie z argumentem - transformowanym modelem;
    osobiście uważam, że to najładniejszy fragment tego programu; czegoś takiego nie da się napisać w C
  *)
  let transformation =
    match Graphics.read_key () with (* read_key wstrzymuje pętlę do czasu naciśnięcia klawisza *)
      | 'x' -> raise Break (* wyjście z pętli renderującej *)
      | 'd' -> move_right 0.1
      | 'a' -> move_left 0.1
      | 'w' -> move_up 0.1
      | 's' -> move_down 0.1
      | 'r' -> move_closer 0.1
      | 'f' -> move_further 0.1
      | 'q' -> rotate_y 0.01
      | 'e' -> rotate_y (-.0.01)
      | _   -> identity (* przypadkowe naciśnięcie innego klawisza nie zmienia stanu *)
  in
    rendering_loop (transformation model);;
