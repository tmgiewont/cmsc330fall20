open List
open Sets

(*********)
(* Types *)
(*********)

(*Test values

let nfa_ex1 = {
  sigma = ['a'; 'b'];
  qs = [0; 1; 2];
  q0 = 0;
  fs = [2];
  delta = [(0, Some 'b', 1); (0, Some 'a', 0); (0, Some 'b', 0); (1, Some 'b', 2)]
}

let nfa_ex1 = {
  sigma = ['a'; 'b'];
  qs = [0; 1; 2; 3;4];
  q0 = 0;
  fs = [0;4];
  delta = [(0, Some 'a', 1); (0, Some 'b', 2); (1, Some 'b', 3); (1, None, 2); (2, None, 3); (2, Some 'a', 4); (3, Some 'a',4)]
}

*)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

(****************)
(* Part 1: NFAs *)
(****************)

let insert_all alst blst = 
  List.fold_left (fun a x -> insert x a) blst alst


let rec move_help (delta: ('q,'s) transition list) (qs: 'q) (s: 's option) (acc: 'q list) : 'q list = 
    match delta with
    | (start, on, endstate)::t when start = qs && s = on && (elem endstate acc = false)-> (move_help t qs s (insert endstate acc))
    | [] -> acc
    |_::t -> move_help t qs s acc

let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list = 
  let {sigma = alpha; qs = ql; q0 = _; fs = _; delta = trans} = nfa in
    match s with
    | Some i when (elem i alpha) = false -> [] 
    | _ -> List.fold_left (fun a x-> (insert_all (move_help trans x s a) a)) [] qs

let rec e_help (delta: ('q,'s) transition list) (qs: 'q) (acc: 'q list) (main: ('q,'s) transition list): 'q list = 
  match delta with
  | (start, on, endstate)::t when start = qs && on = None && (elem endstate acc = false) -> 
          union (e_help t start (insert endstate acc) main ) (e_help main endstate (insert endstate acc) main)
  | [] -> acc
  |_::t -> e_help t qs acc main


let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
  List.fold_left  (fun a x -> union (e_help nfa.delta x a nfa.delta) a ) qs qs


(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let rec new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  List.fold_left (fun a x -> insert (e_closure nfa (move nfa qs (Some x))) a) [] nfa.sigma

let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  List.fold_left (fun a x -> insert (qs, Some x , e_closure nfa (move nfa qs (Some x) )) a) [] nfa.sigma

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list = 
  if(intersection qs nfa.fs = []) then [] else [qs]

let rec adder (dfa: ('q list, 's) nfa_t) qlst =
  match qlst with
    |(start, on ,endstate)::t -> adder {dfa with qs = (union [endstate;start] dfa.qs); delta = (insert (start,on,endstate) dfa.delta )}  t
    |[] -> dfa

let rec final2 (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t) qlst =
  match qlst with
    |h::t when List.length (new_finals nfa h) > 0 -> final2 nfa ({dfa with fs = insert h dfa.fs}) t
    |h::t -> final2 nfa dfa t
    |[] -> dfa

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t =
    let unmarked = diff dfa.qs work in  
    match unmarked with 
    | [] -> dfa
    | h::t-> let dfa =(adder dfa (new_trans nfa h)) in nfa_to_dfa_step nfa {dfa with qs = diff dfa.qs [[]]} (insert h work) 

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  let q = (e_closure nfa [nfa.q0]) in 
    let dfa = {sigma = nfa.sigma; qs = [q]; q0 = q; fs = []; delta = []} in 
      let dfa2 = nfa_to_dfa_step nfa dfa [] in
        (final2 nfa dfa2 dfa2.qs) 

       
let rec getstate trans c state = 
  match trans with
  |(start,on,endstate)::t when start = state && on = Some c ->  endstate
  |_::t -> getstate t c state
  | [] -> state

let rec checker dfa clst state = 
  match clst with
  |h::t when (elem h dfa.sigma) = false -> []
  |h::t -> checker dfa t (getstate dfa.delta h state)
  |[] -> state


let accept (nfa: ('q,char) nfa_t) (s: string) : bool =
  let qlst = (explode s) in let dfa = (nfa_to_dfa nfa) in let state = checker dfa qlst (dfa.q0) in
  elem state dfa.fs