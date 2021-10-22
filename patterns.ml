(* TP2 - Abiga?lle Panhelleux - A3 - 19/10/2021 *)

(* 1.1.1 Build me a line *)
let rec build_line n str =
   if n<=0 then
      ""
   else
      str^build_line (n-1) str;;

    (* Some examples to use *)

(* 1.1.2 Draw me a square *)
let square n str =
   if n<1 then
      invalid_arg "square : cannot build a square"
   else
      let rec aux x =
         if x=0 then
            print_string ("")
         else
            let rec build_line n str =
               if n<=0 then
                 ""
               else
                 str^build_line (n-1) str
            in match x with
               1 -> print_string( build_line n str ) ; print_newline ()
              |_ -> print_string( build_line n str) ; print_newline () ; aux (x-1)
      in aux n;;

    (* Some examples to use *)

(* 1.1.3 Draw me a square - bis *)
let square2 n (str1, str2) =
  if n<1 then
    invalid_arg "square2 : cannot build a square"
  else
    let rec aux (x,a) =
      if x=0 then
        print_string ("")
      else
        let rec build_line n (str1, str2) =
          if n<=0 then
            ""
          else
            str1^str2^build_line (n-1) (str1, str2);
        in
            match (x,a) with
              (1,a) when a mod 2 = 0 -> print_string (build_line n (str2, str1) ) ; print_newline () ; aux((x-1), a+1)
             |(1,_) -> print_string (build_line n (str1, str2)); print_newline (); aux((x-1), a+1)
             |(_,a) when a mod 2 = 0 -> print_string (build_line n (str2, str1)); print_newline () ; aux ((x-1), a+1)
             |_ -> print_string (build_line n (str1, str2)); print_newline () ; aux ((x-1), a+1)
    in aux (n, 1);;

    (* Some examples to use *)

(* 1.1.4 Draw me a triangle *)
let rec triangle n str =
   if n<0 then
      invalid_arg "triangle : cannot draw a triangle"
   else
      if n=0 then
        ()
      else
         let rec build_line n str =
            if n<=0 then
              ""
            else
              str^build_line (n-1) str

        in ( triangle (n-1) str ; print_string (build_line n str) ; print_newline () );;

     (* Somes examples to use *)
