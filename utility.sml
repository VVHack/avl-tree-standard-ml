fun max (x) = fn y => if x > y then x else y;
fun maxlist (x::y::xs) = 
      max (maxlist (y::xs)) x
    | maxlist [x] = x
    | maxlist [] = raise Domain;
  
fun sumlist (x::xs) =
      x + sumlist xs
    | sumlist [] = 0;

fun callall arg = 
    let
    fun call (f::fs) = f(arg)::(call fs)
      | call [] = []
    in
    call
    end;

fun print_list l =
    let fun printIntAndComma num = (Int.toString num) ^ ","
    in
    (List.map print (List.map printIntAndComma l));
    print "\n"
    end;

fun println x = print(x ^ "\n");

fun printInt x = print(Int.toString(x));

fun printIntLn x = print(Int.toString(x) ^ "\n");

fun stringListCombined [] = ""
  | stringListCombined (x::xs) = x ^ (stringListCombined xs);

(* Repeats a string n times *)
fun repeat 0 = (fn s => "") | repeat n = fn s => s ^ (repeat (n-1) s);
