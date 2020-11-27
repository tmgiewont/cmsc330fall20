(* Part 1: Type inference *)

let f1 a b = a + b;;

let f2 a b = if a then b else a;;

let f3 a b c = if (a +. b) == 0.0 then "Hi" else c;;

(* Part 2: Type definition *)

let tf1 a = if a == "hi" then 1 else 0;;

let tf2 a b c = if b = c then true else false;;

let tf3 a b = match a with
  h::_ -> if (a = b) then h else h ;;

(* Part 3: Functions *)

let rec concat str1 str2 = str1 ^ str2;;

let add_to_float integer flt = flt +. float_of_int(integer);;

let rec fib n = if n =0 then 0  else if n < 3 then 1 else fib(n-1) + fib(n-2);;

(* Part 4: Lists *)

let rec add_three lst = match lst with
  [] -> []
  |h::t -> (h +3):: add_three t ;;

let rec filter n lst = match lst with
  [] -> []
  |h::t -> if(h <= n) then h::filter n t else filter  n t;;

let rec double lst = match lst with
  [] -> []
  |h::t -> h::h:: double t ;;
