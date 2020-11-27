open List
open Nfa
open Sets

(*********)
(* Types *)
(*********)

type regexp_t =
  | Empty_String
  | Char of char
  | Union of regexp_t * regexp_t
  | Concat of regexp_t * regexp_t
  | Star of regexp_t

(***********)
(* Utility *)
(***********)

let fresh =
  let cntr = ref 0 in
  fun () ->
    cntr := !cntr + 1 ;
    !cntr

(*******************************)
(* Part 3: Regular Expressions *)
(*******************************)

  let rec regexp_to_nfa (regexp: regexp_t) : (int, char) nfa_t =
    match regexp with
    | Empty_String -> let start = fresh() in let ends = fresh() in {sigma = []; qs = [start; ends]; q0 = start; fs = [ends]; delta = [(start, None, ends)]}
    | Char c -> let start = fresh() in let ends = fresh() in {sigma = [c]; qs = [start;ends]; q0 = start; fs = [ends]; delta = [(start, Some c, ends)]}
    | Union (r1,r2) -> let nfal = regexp_to_nfa r1 in let nfar = regexp_to_nfa r2 in let start = fresh() in let ends = fresh() in
      {sigma = union nfal.sigma nfar.sigma; qs = union [start;ends] (union nfal.qs nfar.qs); q0 = start; fs = [ends];
      delta = union (union ([(start, None, nfal.q0); (start, None, nfar.q0)]) 
              (union (List.fold_left(fun a x-> insert (x,None,ends) a) [] nfal.fs) (List.fold_left(fun a x -> insert (x,None,ends) a) [] nfar.fs)))
              (union nfal.delta nfar.delta) 
        }
    | Concat(r1,r2) -> let nfal = regexp_to_nfa r1 in let nfar = regexp_to_nfa r2 in
      {sigma = union nfal.sigma nfar.sigma; qs =(union nfal.qs nfar.qs); q0 = nfal.q0; fs = nfar.fs;
      delta = union (List.fold_left(fun a x-> insert (x,None,nfar.q0) a) [] nfal.fs) (union nfal.delta nfar.delta)}
    | Star r1 -> let nfa = regexp_to_nfa r1 in let start = fresh() in let ends = fresh() in 
      {sigma = nfa.sigma; qs = union [start;ends] nfa.qs; q0 = start; fs = [ends]; 
      delta = union [(start, None, nfa.q0);(start, None,ends);(ends,None,start)] (union (List.fold_left(fun a x -> insert (x,None,ends) a)  [] nfa.fs) nfa.delta) }


(*****************************************************************)
(* Below this point is parser code that YOU DO NOT NEED TO TOUCH *)
(*****************************************************************)

exception IllegalExpression of string

(* Scanner *)
type token =
  | Tok_Char of char
  | Tok_Epsilon
  | Tok_Union
  | Tok_Star
  | Tok_LParen
  | Tok_RParen
  | Tok_END

let tokenize str =
  let re_var = Str.regexp "[a-z]" in
  let re_epsilon = Str.regexp "E" in
  let re_union = Str.regexp "|" in
  let re_star = Str.regexp "*" in
  let re_lparen = Str.regexp "(" in
  let re_rparen = Str.regexp ")" in
  let rec tok pos s =
    if pos >= String.length s then [Tok_END]
    else if Str.string_match re_var s pos then
      let token = Str.matched_string s in
      Tok_Char token.[0] :: tok (pos + 1) s
    else if Str.string_match re_epsilon s pos then
      Tok_Epsilon :: tok (pos + 1) s
    else if Str.string_match re_union s pos then Tok_Union :: tok (pos + 1) s
    else if Str.string_match re_star s pos then Tok_Star :: tok (pos + 1) s
    else if Str.string_match re_lparen s pos then Tok_LParen :: tok (pos + 1) s
    else if Str.string_match re_rparen s pos then Tok_RParen :: tok (pos + 1) s
    else raise (IllegalExpression ("tokenize: " ^ s))
  in
  tok 0 str

let tok_to_str t =
  match t with
  | Tok_Char v -> Char.escaped v
  | Tok_Epsilon -> "E"
  | Tok_Union -> "|"
  | Tok_Star -> "*"
  | Tok_LParen -> "("
  | Tok_RParen -> ")"
  | Tok_END -> "END"

(*
   S -> A Tok_Union S | A
   A -> B A | B
   B -> C Tok_Star | C
   C -> Tok_Char | Tok_Epsilon | Tok_LParen S Tok_RParen

   FIRST(S) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(A) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(B) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(C) = Tok_Char | Tok_Epsilon | Tok_LParen
 *)

let parse_regexp (l : token list) =
  let lookahead tok_list =
    match tok_list with
    | [] -> raise (IllegalExpression "lookahead")
    | h :: t -> (h, t)
  in
  let rec parse_S l =
    let a1, l1 = parse_A l in
    let t, n = lookahead l1 in
    match t with
    | Tok_Union ->
        let a2, l2 = parse_S n in
        (Union (a1, a2), l2)
    | _ -> (a1, l1)
  and parse_A l =
    let a1, l1 = parse_B l in
    let t, n = lookahead l1 in
    match t with
    | Tok_Char c ->
        let a2, l2 = parse_A l1 in
        (Concat (a1, a2), l2)
    | Tok_Epsilon ->
        let a2, l2 = parse_A l1 in
        (Concat (a1, a2), l2)
    | Tok_LParen ->
        let a2, l2 = parse_A l1 in
        (Concat (a1, a2), l2)
    | _ -> (a1, l1)
  and parse_B l =
    let a1, l1 = parse_C l in
    let t, n = lookahead l1 in
    match t with Tok_Star -> (Star a1, n) | _ -> (a1, l1)
  and parse_C l =
    let t, n = lookahead l in
    match t with
    | Tok_Char c -> (Char c, n)
    | Tok_Epsilon -> (Empty_String, n)
    | Tok_LParen ->
        let a1, l1 = parse_S n in
        let t2, n2 = lookahead l1 in
        if t2 = Tok_RParen then (a1, n2)
        else raise (IllegalExpression "parse_C 1")
    | _ -> raise (IllegalExpression "parse_C 2")
  in
  let rxp, toks = parse_S l in
  match toks with
  | [Tok_END] -> rxp
  | _ -> raise (IllegalExpression "parse didn't consume all tokens")


let string_to_regexp str = parse_regexp @@ tokenize str

let string_to_nfa str = regexp_to_nfa @@ string_to_regexp str
