open Funs

(************************)
(* Part 2: Integer BSTs *)
(************************)

type int_tree =
  | IntLeaf
  | IntNode of int * int_tree * int_tree

let empty_int_tree = IntLeaf

let rec int_insert x t =
  match t with
  | IntLeaf -> IntNode(x, IntLeaf, IntLeaf)
  | IntNode (y, l, r) when x > y -> IntNode (y, l, int_insert x r)
  | IntNode (y, l, r) when x = y -> t
  | IntNode (y, l, r) -> IntNode (y, int_insert x l, r)

let rec int_mem x t =
  match t with
  | IntLeaf -> false
  | IntNode (y, l, r) when x > y -> int_mem x r
  | IntNode (y, l, r) when x = y -> true
  | IntNode (y, l, r) -> int_mem x l

(* Implement the functions below. *)

let rec int_size t = match t with
    IntLeaf -> 0
    |IntNode (y,l,r) -> int_size l + int_size r + 1
;;

let rec int_max t = match t with
  IntLeaf -> invalid_arg "int_max"
  |IntNode (y,l,r) when r <> IntLeaf -> int_max r 
  |IntNode (y,l,r) (*when r = IntLeaf*) -> y
;;

(****************************)
(* Part 3: Polymorphic BSTs *)
(****************************)

type 'a atree =
  | Leaf
  | Node of 'a * 'a atree * 'a atree
type 'a compfn = 'a -> 'a -> int
type 'a ptree = 'a compfn * 'a atree

let empty_ptree f : 'a ptree = (f,Leaf)

(* Implement the functions below. *)

let rec a_insert x t comp =
  match t with
  | Leaf -> Node(x, Leaf, Leaf)
  | Node (y, l, r) when comp x y > 0 -> Node (y, l, a_insert x r comp) 
  | Node (y, l, r) when comp x y = 0 -> t
  | Node (y, l, r) -> Node (y, a_insert x l comp, r)
;;

let pinsert x t = match t with
    | (f,t) -> (f,a_insert x t f) ;;
;;

let rec pfind x t comp = match t with
    | Leaf -> false
    | Node(y,l,r) when comp x y = 0 -> true
    | Node(y,l,r) when comp x y > 0 -> pfind x r comp
    | Node(y,l,r) (*when comp x y < 0 *)-> pfind x l comp
;;
let pmem x t = match t with
    | (f,t) -> pfind x t f
;;

let pinsert_all lst t = fold_right (pinsert) lst t 

let rec p_inorder t lst = match t with
    |Leaf -> lst
    |Node(y,l,r) -> let right = p_inorder r lst in let value = y::right in p_inorder l value
;;


let p_as_list t = match t with
    | (f,t) -> p_inorder t []
;;

let pmap f t = let (comp,tail) = t in pinsert_all (map f (p_as_list t)) (empty_ptree comp);; 


(***************************)
(* Part 4: Variable Lookup *)
(***************************)

(* Modify the next line to your intended type *)
type lookup_table = 
  | Empty 
  | Scope of lookup_table
  | Variable of string * int * lookup_table

let empty_table () : lookup_table = Empty

let push_scope (table: lookup_table) : lookup_table = Scope(table)

let rec pop_scope (table: lookup_table) : lookup_table = match table with
| Empty -> failwith "No scopes remain!"
| Scope(l) -> l
| Variable(n,v,l) -> pop_scope l 

let rec add_var name value (table: lookup_table) : lookup_table = match table with 
  | Empty -> failwith "There are no scopes to add a variable to!"
  | Scope(l) -> Variable(name,value,table)
  | Variable(n,v,l) -> Variable(name,value,table)

let rec lookup name (table: lookup_table) = match table with
  | Empty -> failwith "Variable not found!"
  | Scope(l) -> lookup name l
  | Variable(n,v,l) when n = name -> v
  | Variable(n,v,l) -> lookup name l

(*******************************)
(* Part 5: Shapes with Records *)
(*******************************)

type pt = { x: int; y: int };;
type shape =
  | Circ of { radius: float; center: pt }
  | Square of { length: float; upper: pt }
  | Rect of { width: float; height: float; upper: pt }
;;

(* Implement the functions below. *)

let area s = match s with
| Circ {radius = r; center = _} -> r *. r *. 3.14
| Square {length = l; upper = _} -> l *. l
| Rect {width = w; height = h; upper = _} -> w *. h
;;

let filter f lst = fold (fun a x -> if (f x = true) then a@[x] else a) [] lst;;
