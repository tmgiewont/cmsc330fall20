(*Provided for your benefit*)
let rec map f xs = match xs with
| [] -> []
| x :: xt -> (f x)::(map f xt)

let rec foldl f a xs = match xs with
| [] -> a
| x :: xt -> foldl f (f a x) xt

let rec foldr f xs a = match xs with
| [] -> a
| x :: xt -> f x (foldr f xt a) 

(********************)
(* Currying Functions and Partial Application *)
(********************)

let mul_n n lst = map(( * ) n) lst;;

(* Joins together the strings in xs by separator sep
   e.g. join ["cat"; "dog"; "fish"] "," = "cat,dog,fish". *)
let join xs sep = match xs with
   |[] -> ""
   |h::t -> foldl (^) h (map ((^) sep) t)

(********************)
(* Option Functions *)
(********************)

(* Converts an option to a list. Some is a singleton containing
   the value, while None is an empty list. *)
let list_of_option (o : 'a option) : 'a list =  match o with
   |None -> []
   |Some(a) -> [a]

(* If the pair's key matches k returns the value as an option. Otherwise
   return None. *)
let match_key (k : 'k) (p : ('k * 'v)) : 'v option = 
   let (key,v) = p in if(k = key) then Some v else None

(******************)
(*LENGTHLIST FUNCTIONS*)
(******************)

(*This list encodes the idea of a list having multiple elements in a row*)
(*Ex: [1;2;3;4] = Cons(1, 1, Cons(2, 1, Cons(3, 1, Cons(4, 1, Empty))))*)
(*Ex: [1; 1; 1; 1; 2; 2; 2; 3; 3; 4] = Cons(1, 4, Cons(2, 3, Cons(3, 2, Cons(4, 1, Empty))))*)

type 'a lengthlist = 
    Cons of ('a * int * 'a lengthlist)
    | Empty
;;

let rec add v n= 
   if(n = 0) then [] else v::add v (n-1)

let rec list_of_lengthlist llst = match llst with
   | Empty -> []
   | Cons(v,n,t) -> (add v n)@list_of_lengthlist t

let rec map_lengthlist fn llst = match llst with
   | Empty -> Empty
   | Cons(v,n,t) -> Cons(fn v, n, map_lengthlist fn t)

let rec decrement_count llst = match llst with
   | Empty -> Empty
   | Cons(v,n,t) when (n-1) > 0 -> Cons(v, (n-1), decrement_count t)
   | Cons(v,n,t) when (n-1) = 0 -> decrement_count t