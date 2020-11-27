(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup tup = match tup with
(a,b,c) -> (c,b,a);;

let abs x = if(x< 0) then x * (-1) else x;;

let area x y = match x, y with (a,b) , (c,d) -> abs(a-c) * abs(b-d);;

let volume x y = match x,y with (a,b,c), (d,e,f) -> abs(a-d) * abs(b-e) * abs(c-f);;

(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec factorial x = 
    if x = 0 then 1
    else x * factorial(x -1)
;;
let rec pow x y = 
  if y = 0 then 1
  else if y = 1 then x
  else x * pow x (y-1) 
;;

let rec log x y = if y/x < 1 then 0 else 1 + log x (y/x);;

let rec check x d = match d with
  |1 -> true
  | _-> (x mod d <> 0) && check x (d-1)
;;

let rec is_prime x = 
  if x < 2 then false 
  else check x (x-1)
;;


let rec next_prime x = 
  if(x < 2) then 2
  else if(is_prime x = true) then x
  else (next_prime (x+1))
;;

(*****************)
(* Part 3: Lists *)
(*****************)

let rec get idx lst = 
  if (idx < 0) then failwith "Out of bounds"
  else match lst with
  |[] -> failwith "Out of bounds"
  |h::t -> if(idx = 0) then h else get (idx -1) t 
;;

let rec length lst = 
  match lst with 
  [] -> 0
  | _::t -> length t + 1
;;
let larger lst1 lst2 = 
  if length lst1 = length lst2 then [] else if length lst1 > length lst2 then lst1 else lst2
;;

let rec combine lst1 lst2 = match lst1 with
  [] -> lst2
  | h::t -> h::combine t lst2
;;

let rec reverse lst = match lst with
  [] -> []
  | [x] -> [x]
  | h::t -> combine (reverse t) [h]
;;

let rec rotate shift lst = 
  if shift = 0 then lst
  else match lst with
  [] -> []
  |h::t -> rotate (shift - 1) (combine t [h])
;;
