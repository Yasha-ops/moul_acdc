let rec build_line n str =
  if n = 0 then
    ""
  else
    str ^ build_line (n-1) str ;;

(* Patterns *)

let square n c =
  let line =  build_line n c ^"\n"
  in
  let rec print_square = function
      0 -> ()
    | n -> (print_string line ; print_square (n-1))
  in
    print_square n ;;


let square2 n (c1, c2) =
  let line1 = build_line n (c1 ^ c2) ^ "\n"
  and line2 = build_line n (c2 ^ c1) ^ "\n"
  in
  let rec print_square = function
      0 -> ()
    | n -> (print_string line1 ; print_string line2 ; print_square (n-1))
  in
    print_square (n/2);
    if n mod 2 = 1 then print_string line1 ;;




let triangle n c =
  let rec print_triangle = function
      0 -> ()
    | i -> (print_triangle (i-1) ;
            print_string (build_line i c ^ "\n"))
  in
    print_newline();
    print_triangle n;;


(* another version *)

let triangle n c =
  let rec print_triangle = function
      0 -> ()
    | i -> (print_string (build_line(n-i+1) c ^ "\n");
            print_triangle (i-1))
  in
    print_triangle n;;


(************************ Pyramid: a little hard... **********************)
(* without global function build_line *)

(* with global function build_line *)

let pyramid n =
  let aline i =
    let line1 = build_line (n - i) "."
    and line2 = build_line (2 * i) "*"
    in
      line1 ^ line2 ^ line1 ^"\n"
  in
  let rec print_pyr = function
    | 0 -> print_newline()
    | i -> (print_pyr (i - 1);
	    print_string(aline i))
  in
    print_pyr n ;;


let pyramid n (c1, c2) =
  let rec build_line j = function
      0 -> ""
    | i -> let c = if i > j then c1 else c2
           in c ^ build_line j (i-1) ^ c
  in
  let rec print_pyr = function
      0 -> ()
    | i -> (print_string ((build_line (n-i+1) n) ^ "\n") ; print_pyr (i-1))
  in
    print_pyr n ;;


(************************ Cross: harder! **********************)
(* without global function build_line *)

let cross n =
  let rec build_line j i =
    let car =
      if i = j then
	"*"
      else
	"."
    in
      match i with
          0 -> car
	| i -> car ^ build_line j (i-1) ^ car
  and print_cross = function
      1 -> print_string (build_line 0 (n-1) ^ "\n")
    | i -> let line = build_line (i-1) (n-1) ^ "\n"
	   in
             (print_string line ; print_cross (i-1) ; print_string line)
  in
  print_newline();
  print_cross n ;;


let cross n (c1, c2) =
  let aline i =
    let line1 = build_line i c1
    and line2 = build_line (2*(n-i-1)-1) c1
    in
      line1 ^ c2 ^ line2 ^ c2 ^ line1 ^ "\n"
  in
  let rec print_cross i =
    if i < n-1 then
      let line = aline i in
	(print_string line ;
	 print_cross (i+1);
	 print_string line)
      else
	let line = build_line (n-1) c1 in
	  print_string (line ^ c2 ^ line ^ "\n")
  in
    print_cross 0 ;;
