(************************************************************)
(*                     Game of life                         *)
(************************************************************)

(* graphics *)

#load "graphics.cma" ;; (* comment it in compiled version! *)
open Graphics ;;

let open_window size =
  open_graph(" " ^ string_of_int size ^ "x" ^ string_of_int (size+20));;

let grey = rgb 127 127 127 ;;

let cell_color = function
  | 0 -> white
  | _ -> black ;;

let cell_size = 10 ;;

(* original game of life definitions *)

let new_cell = 1 ;;

let empty = 0 ;;

let is_alive cell = cell <> empty ;;


(************************************************************)
(*                  graphics                                *)
(*        from the board to the graphic window              *)

(* draw_fill_square: 
   draw a grey size x size square with left corner at (x,y) 
   filled with color 
*)

let draw_fill_square (x,y) size color = 
  set_color color; fill_rect x y size size;
  set_color grey; draw_rect x y size size;;

(* draw_cell: draw the cell (at position (x, y) in the board) on the graphics windows 
   with ratio size *)
(* add (1,1) to not stick to the frame! *)

let draw_cell (x,y) size color =  
  draw_fill_square ((x+1) * size, (y+1) * size) size color ;;


(* draw_board: draw a size x size board ('a list list) *)

let draw_board board size =
  let rec iter_row x = function
    | [] -> ()
    | e::l -> iter_column x 0 e; iter_row (x+1) l
  and iter_column x y = function
    | [] -> ()
    | e::l -> draw_cell(x,y) size (cell_color e); iter_column x (y+1) l
  in 
    clear_graph () ;      
    iter_row 0 board       
;;


(************************************************************)
(*                     Game of life                         *)
(************************************************************)

(* rules0: returns the new status of the cell that have near neighbours *)

let rules0 cell near = 
  if (cell = empty) && (near = 3) then 
    new_cell
  else 
    if (near = 3) || (near = 2) then 
      cell
    else 
      0 ;;

(* count_neighbours: extract number of alive cells around cell at (x,y) 
   from a lines x columns board  *)

(* get_cell_neighborhood: builds a list of cells near (x, y) *)
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

(* seed_life: place count new cells randomly in board (size is the board size!) *)

let seed_life board size count = 
  let rec plant board = function
    | 0 -> board
    | n ->
      plant
	(put_cell new_cell (Random.int size, Random.int size) board)
	(n-1)
  in
    plant board count ;;

(* new_board: returns a new size x size board filled with n alive cells *)

let new_board size n = seed_life (init_board (size, size) 0) size n ;;

(* next_generation: 
   applies rules to each cell of the board 
   draws cells only if born or died
   and returns the new board *)

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

(* counts remaining alive cells *)

let remaining board =
  let rec exists_row = function
    | [] -> false
    | e::l -> exists_column e || exists_row l
  and exists_column = function
    | [] -> false
    | e::l -> is_alive e || exists_column l (* or e <> 0 || ... *)
  in
    exists_row board ;;

(* game: applies the game of life rules on board *)
(* infty = 0 -> game stops after n generations
   infty = 1 -> game does never stop unless there no remaining alive cells 
*)

let rec game board size n infty = 
   if n <> 0 || infty = 1 && remaining board then
      (
	game (next_generation board size) size (n - 1 + infty) infty 
      ) ;;


(* new_game: launches a new game *)

let new_game size nb_cells n = 
  open_window (size* cell_size + 40) ;
  let board = new_board size nb_cells in
    (draw_board board cell_size ; 
    if n = 0 then
      game board size 1 1
    else
      game board size n 0) ;;
