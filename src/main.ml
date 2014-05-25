(* autor: Maciej Pacut *)

open Engine;;
open Model;;
open Math;;
open Transformations;;

let (screen_width, screen_height) = (600., 600.);;

(*
  pomocnicza funkcja parsująca;
  specyfika modeli w formacie .OBJ polega na tym,
  że nie są w nim bezpośrednio zapisywane współrzędne wierzchołków każdego trójkąta;
  na początku pliku jest lista wierzchołków a następnie współrzędne trójkątów
  brane są przez indeksowanie tej listy;
  ta funkcja zamienia indeksy współrzędnych na współrzędne;
  jedyne użycie imperatywnych struktur danych w tym projekcie  
*)
let indices_to_coordinates model =
  let indexed_vertices = Array.of_list model.vertices
  in List.map (fun (ind1 ,ind2, ind3) -> (indexed_vertices.(ind1 - 1),
					  indexed_vertices.(ind2 - 1),
					  indexed_vertices.(ind3 - 1))) model.tris;;

(* parsowanie modeli za pomocą ocamlyacc i ocamllex *)
let parse_model filename =
  let model = Parser.main Lexer.next
    (Lexing.from_channel (open_in filename))
  in indices_to_coordinates model;;

let init_args =
  Printf.sprintf " %dx%d" (int screen_width) (int screen_height);;


(* tworzy okno, parsuje model i wchodzi do pętli renderującej *)
let run model_name =
  Graphics.open_graph init_args;
  Graphics.set_window_title "3d rendering engine";
  let model = parse_model model_name in
    try
      rendering_loop (rotate_y (-.0.5) (move_further 5.0 model)) (* oddalam na początku model, żeby kamera nie była wewnątrz *)
    with Break -> ()
      
let path_to_model () =
  if (Array.length Sys.argv) <> 2 then
    failwith "aby uruchomic program, nalezy podac jako argument sciezke do pliku modelu: 3d.e /path/to/model.obj\n"
  else
    Sys.argv.(1);;

(* punkt startowy programu *)
let () =
  run (path_to_model ());;
