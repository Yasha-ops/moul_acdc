exception Timeout
exception Not_Done


let delayed_fun_2 timeout f x y =
    let _ =
        Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise Timeout))
    in
  ignore (Unix.alarm timeout);
  try
      let r = f x y in
      ignore (Unix.alarm 0); r
  with
  | e  -> ignore (Unix.alarm 0); raise e;;


let square n c =
    print_endline "#Function {square} not done !\nsquare_array=[NULL]";
        raise Not_Done

let build_line n c =
    print_endline "#Function {build_line} not done !\nsquare_array=[NULL]";
        raise Not_Done

let square2 n (str1, str2) =
    print_endline "#Function {square} not done !\nsquare_array=[NULL]";
        raise Not_Done

let triangle a str =
    print_endline "#Function {triangle} not done !\ntriangle_array=[NULL]";
        raise Not_Done

let pyramid n (c1, c2) =
    print_endline "#Function {pyramid} not done !\npyramid_array=[NULL]";
        raise Not_Done

let cross n (str1, str2) =
    print_endline "#Function {cross} not done !\ncross_array=[NULL]";
        raise Not_Done

INCLUDE "patterns.ml"

try
    match Sys.argv.(1) with
    | "build_line" -> print_endline (delayed_fun_2 2 build_line (int_of_string Sys.argv.(2)) "$");
    | "square" -> delayed_fun_2 2 square (int_of_string Sys.argv.(2)) "$";
    | "square2" -> delayed_fun_2 2 square2 (int_of_string Sys.argv.(2)) ("$", "@");
    | "cross" -> delayed_fun_2 2 cross (int_of_string Sys.argv.(2)) ("$","@");
    | "pyramid" -> delayed_fun_2 2 pyramid (int_of_string Sys.argv.(2)) ("$","@");
    | "triangle" -> delayed_fun_2 2 triangle (int_of_string Sys.argv.(2)) "$";
    with
    Timeout -> print_endline "Timeout on function";
    |Not_Done -> print_endline "Function Not_Done";
