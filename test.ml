exception Timeout

INCLUDE "patterns.ml"

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



try
    match Sys.argv.(1) with
    | "square" -> delayed_fun_2 5 square (int_of_string Sys.argv.(2)) "$";
    | "square2" -> delayed_fun_2 5 square2 (int_of_string Sys.argv.(2)) ("$", "@");
    | "cross" -> delayed_fun_2 5 cross (int_of_string Sys.argv.(2)) ("$","@");
    | "pyramid" -> delayed_fun_2 5 pyramid (int_of_string Sys.argv.(2)) ("$","@");
    | "triangle" -> delayed_fun_2 5 triangle (int_of_string Sys.argv.(2)) "$";
with
    Timeout -> print_endline "Timeout on function";
