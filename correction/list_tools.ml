(*******************   Toolbox *********************)
(*             list list functions                *)

(*          simple functions on lists                *)

let nth n list =  
  if n < 0 then
    invalid_arg "nth: index must be a natural"
  else
    let rec nth_rec = function
      | ([], _) -> failwith "nth: out of bound"
      | (e::_, 0) -> e
      | (_::l, n) -> nth_rec (l, n-1)
    in
      nth_rec (list, n) ;;

let rec init_list n value =
  if n = 0 then
    []
  else
    value :: init_list (n-1) value ;;

(*             list list functions                *)

(* init_board: generate a nblines x nbcolumn  matrix filled with x (1) *) 

let init_board (nblines, nbcolumns) x =
  init_list nblines (init_list nbcolumns x) ;;

(* get_cell: extract value at position (x, y) from an 'a list list (1) *)

let get_cell (x, y) board =
  nth y (nth x board) ;;

(* put_cell: replace value at (x,y) in board by cell (no "out of bound" test!) (2) *)

let put_cell cell (x,y) board =
  let rec put_list = function
    | (_, []) -> []
    | (0, e::l) -> cell :: l
    | (n, e::l) -> e :: (put_list ((n-1), l))
  and process_row = function
    | (_, []) -> []
    | (0, e::l) -> (put_list (y,e)) :: l
    | (n, e::l) -> e :: (process_row ((n-1), l))
  in
    process_row (x, board);;
