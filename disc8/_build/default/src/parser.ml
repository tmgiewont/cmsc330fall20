open Lexer

(* Types *)
type expr =
| Int of int
| Plus of expr * expr
| Mult of expr * expr

(* Provided helper function - takes a token list and an exprected token.
 * Handles error cases and returns the tail of the list *)
let match_token (toks : token list) (tok : token) : token list =
  match toks with
  | [] -> raise (Failure(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (Failure(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)
    ))

let lookahead toks = match toks with
	 h::t -> h
	| _ -> raise (Failure("Empty input to lookahead"))



(* Parses a token list. *)
let rec parser (toks : token list) : expr =
  let (remaining, expr) = parse_S toks in 
  if remaining <> [Tok_EOF] then raise (Failure("Bad"))
  else expr

(* Parses the S rule. *)
and parse_S (toks : token list) : (token list * expr) =
  let (toks_after, expr) = parse_M toks in 
  match (lookahead toks_after) with 
  | Tok_Plus -> let toks2 = match_token toks_after Tok_Plus in 
      let (toks3, expr_after) = parse_S toks2 in 
      (toks3, Plus(expr, expr_after))
  | _ -> (toks_after, expr)

(* Parses the M rule. *)
and parse_M (toks : token list) : (token list * expr) =
  let (toks_after, expr) = parse_N toks in 
  match (lookahead toks_after) with 
  | Tok_Mult -> let toks2 = match_token toks_after Tok_Mult in 
      let (toks3, expr_after) = parse_M toks2 in 
      (toks3, Mult(expr, expr_after))
  | _ -> (toks_after, expr)
            
(* Parses the N rule. *)
and parse_N (toks : token list) : (token list * expr) =
  match lookahead toks with 
  | Tok_Int i -> let toks2 = match_token toks (Tok_Int i) in (toks2, Int i)
  | Tok_LParen -> let toks2 = match_token toks (Tok_LParen) in 
                  let (toks3, expr) = parse_S toks2 in
                  let toks4 = match_token toks3 Tok_RParen in (toks4, expr)
  | _ -> raise(Failure("Bad "))
