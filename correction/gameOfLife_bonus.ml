(*****************************************************************)
(*                 Bonus: Entrée/Sortie                *)
let save_board filename board =
    let oc = open_out filename in
    let rec write_list = function
        [] -> Printf.fprintf oc "%s" "\n";
    |e::l -> Printf.fprintf oc "%s " (string_of_int e); write_list l
    in
  let rec aux = function
      [] -> close_out oc
    |e::l -> write_list e; aux l
    in aux board;;

(*let board = [*)
(*[1; 1; 1; 1; 1; 1; 1; 1; 1; 1];*)
(*[0; 0; 0; 0; 0; 0; 0; 0; 0; 0];*)
(*[1; 0; 1; 0; 1; 0; 1; 0; 1; 0];*)
(*[0; 1; 0; 1; 0; 1; 0; 1; 0; 1];*)
(*[0; 0; 0; 0; 0; 0; 0; 0; 0; 0];*)
(*[1; 1; 1; 1; 1; 1; 1; 1; 1; 1];*)
(*[0; 0; 0; 0; 0; 0; 0; 0; 0; 0];*)
(*[1; 0; 1; 0; 1; 0; 1; 0; 1; 0];*)
(*[0; 1; 0; 1; 0; 1; 0; 1; 0; 1];*)
(*[0; 0; 0; 0; 0; 0; 0; 0; 0; 0]*)
(*] ;;*)
(*save_board "board.txt" board;;*)

let load_board filename =
    let file = load filename
  in
  let convert str =
      let size_str = String.length str in
      let rec conv_rec index =
          if index >= size_str then
              []
          else
              int_of_string (Char.escaped str.[index]) :: conv_rec (index+2)
      in conv_rec 0
      in
  let rec load_rec = function
      [] -> []
    |e::l -> (convert e) :: load_rec l
  in load_rec file;;

(*let new_board = load_board "board.txt";;*)
(*new_board = board;;*)

(*****************************************************************)
(*                 Bonus1: Where there's life...                *)

let remaining board =
    let rec exists_row = function
        | [] -> false
    | e::l -> exists_column e || exists_row l
and exists_column = function
    | [] -> false
    | e::l -> is_alive e || exists_column l (* or e <> 0 || ... *)
  in
    exists_row board ;;

(* infty = 0 -> game stops after n generations
   infty = 1 -> game does never stop unless there no remaining alive cells (never happens...)
*)

let rec game board size n infty =
    if n <> 0 || infty = 1 && remaining board then
        (
            draw_board board cell_size ;
  game (next_generation board size) size (n - 1 + infty) infty
  ) ;;


let new_game size nb_cells n =
    open_window (size* cell_size + 40) ;
  let board = new_board size nb_cells in
  if n = 0 then
      game board size 1 1
          else
              game board size n 0 ;;

(* new_game 50 500 0 ;; *)

(*****************************************************************)
(*                 Bonus: Patterns                *)

(* size is the board size *)
let init_pattern pattern size =
    let rec add_cells board = function
        | [] -> board
    | (x,y)::l -> add_cells (put_cell new_cell (x,y) board) l
  in
    add_cells (init_board (size,size) 0) pattern ;;

let new_game_pattern pattern size n =
    open_window (size*10 + 40) ;
  let board = init_pattern pattern size in
  (draw_board board cell_size ;
    if n = 0 then
        game board size 1 1
  else
      game board size n 0) ;;

(*******************************************)
(*         Bonus : optimisations           *)


(* cells are drawn only if born or died... *)

let next_generation board size =
    let rec map_row x = function
        | [] -> []
        | e::l -> map_column x 0 e :: map_row (x+1) l
    and map_column x y = function
        | [] -> []
        | e::l ->
                let new_status = rules0 e (count_neighbours (x,y) board size) in
                (if new_status <> e then
                    (draw_cell (x,y) cell_size (cell_color new_status)) ;
   new_status :: map_column x (y+1) l)
                in
    map_row 0 board ;;

(* game displays the board only once *)

let rec game board size n infty =
    if n <> 0 || infty = 1 && remaining board then
        game (next_generation board size) size (n - 1 + infty) infty  ;;

let new_game size nb_cells n =
    open_window (size* cell_size + 40) ;
  let board = new_board size nb_cells in
  (draw_board board cell_size ;
    if n = 0 then
        game board size 1 1
    else
        game board size n 0) ;;

(* new_game 50 200 ;; *)


(**************************************************************)
(*             Bonus : count_neighbours without get_cell         *)

(* get_cell_neighborhood: builds a list of cells near (x, y) *)

(* TODO: build the result in an accu to avoid @...
   better: do not build the list
   + stop when (x+1, y+1) reached?
   *)

let get_cell_neighborhood (x,y) board =
    let rec scan_row = function
        | (_,[]) -> []
        | (n, e::l) when (n >= x-1) && (n <= x+1) ->
                extract_cells n (0, e) @ scan_row (n+1, l)
        | (n, _::l) -> scan_row (n+1, l)
    and extract_cells m = function
        | (_, []) -> []
        | (n, e::l) when (n >= y-1) && (n <= y+1) && ((x,y) <> (m,n)) ->
                e :: extract_cells m (n+1, l)
        | (n, _::l) -> extract_cells m (n+1, l)
in
    scan_row (0, board);;

let rec cell_count = function
    | [] -> 0
    | e::l when is_alive e -> 1 + cell_count l
    | _::l -> cell_count l ;;


let count_neighbours (x,y) board size  =
    cell_count (get_cell_neighborhood (x,y) board) ;;


(* reload last next-generation, game and new_game! *)



(*******************************************)
(*         Bonus: compilation              *)

(*
let read s = print_string (s ^ "\n") ; read_int () ;;

let execute () =
    print_string "1 : aléatoire\n";
  print_string "2 : clown\n";
  print_string "3 : canon à planeur\n";
  let (size, nb, board) =
      match read_int() with
  1 -> let size = read "nb cells ?" and nb = read "nb generations ?" in
(size, nb, new_board size (size*10))
    | 2 -> (40, 111, board_clown)
      | 3 -> (50, read "nb generations ?", board_canon)
      | _ -> failwith "..."
in
    new_game_pattern board size nb ;
    print_string "press a key to quit";
    read_key ()
;;

execute () ;;
*)
