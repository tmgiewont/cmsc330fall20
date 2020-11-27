open SmallCTypes
open Utils
open TokenTypes

(* Parsing helpers (you don't need to modify these) *)

(* Return types for parse_stmt and parse_expr *)
type stmt_result = token list * stmt
type expr_result = token list * expr

(* Return the next token in the token list, throwing an error if the list is empty *)
let lookahead (toks : token list) : token =
  match toks with
  | [] -> raise (InvalidInputException "No more tokens")
  | h::_ -> h

let get_id toks = 
  match toks with 
  |Tok_ID i::t -> i
  |_ -> raise (InvalidInputException "Failure")

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) : token list =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)
    ))

(* Parsing (TODO: implement your code below) *)

let rec parse_expr toks : expr_result =
  parse_OrExpr toks

  and parse_OrExpr toks : expr_result = 
    let(toks2, expr) = parse_AndExpr toks in
    match(lookahead toks2) with
    |Tok_Or -> let toks3 = match_token toks2 Tok_Or in 
      let(toks4, expr2) = parse_OrExpr toks3 in
      (toks4, Or(expr, expr2))
    |_ -> (toks2, expr)
  
  and parse_AndExpr toks : expr_result =
    let(toks2,expr) = parse_EqualityExpr toks in
    match(lookahead toks2) with 
    | Tok_And -> let tokA = match_token toks2 Tok_And in 
      let (toks3, expr2) = parse_AndExpr tokA in
      (toks3, And(expr,expr2))
    | _ -> (toks2,expr)
  
  and parse_EqualityExpr toks : expr_result =
    let(toks2, expr) = parse_RelationalExpr toks in
    match(lookahead toks2) with
    |Tok_Equal -> let tokA = match_token toks2 Tok_Equal in 
    let (toks3, expr2) = parse_EqualityExpr tokA in
    (toks3, Equal(expr,expr2))
    |Tok_NotEqual -> let tokA = match_token toks2 Tok_NotEqual in 
    let (toks3, expr2) = parse_EqualityExpr tokA in
    (toks3, NotEqual(expr,expr2))
    | _-> (toks2,expr)
  
    and parse_RelationalExpr toks : expr_result = 
    let (toks2,expr) = parse_AdditiveExpr toks in
    match(lookahead toks2) with
    |Tok_Less -> let tokA = match_token toks2 Tok_Less in 
      let (toks3, expr2) = parse_RelationalExpr tokA in
      (toks3, Less(expr,expr2))
    |Tok_Greater -> let tokA = match_token toks2 Tok_Greater in 
      let (toks3, expr2) = parse_RelationalExpr tokA in
      (toks3, Greater(expr,expr2))
    |Tok_GreaterEqual -> let tokA = match_token toks2 Tok_GreaterEqual in 
    let (toks3, expr2) = parse_RelationalExpr tokA in
    (toks3, GreaterEqual(expr,expr2))
    |Tok_LessEqual -> let tokA = match_token toks2 Tok_LessEqual in 
    let (toks3, expr2) = parse_RelationalExpr tokA in
    (toks3, LessEqual(expr,expr2))
    | _-> (toks2,expr)

    and parse_AdditiveExpr toks : expr_result =
    let(toks2, expr) = parse_MultiplicativeExpr toks in
    match(lookahead toks2) with
    |Tok_Add -> let tokA = match_token toks2 Tok_Add in 
    let (toks3, expr2) = parse_AdditiveExpr tokA in
    (toks3, Add(expr,expr2))
    |Tok_Sub -> let tokA = match_token toks2 Tok_Sub in 
    let (toks3, expr2) = parse_AdditiveExpr tokA in
    (toks3, Sub(expr,expr2))
    | _-> (toks2,expr)

    and parse_MultiplicativeExpr toks : expr_result =
    let(toks2, expr) = parse_PowerExpr toks in
    match(lookahead toks2) with
    |Tok_Mult -> let tokA = match_token toks2 Tok_Mult in 
    let (toks3, expr2) = parse_MultiplicativeExpr tokA in
    (toks3, Mult(expr,expr2))
    |Tok_Div -> let tokA = match_token toks2 Tok_Div in 
    let (toks3, expr2) = parse_MultiplicativeExpr tokA in
    (toks3, Div(expr,expr2))
    | _-> (toks2,expr)

    and parse_PowerExpr toks : expr_result = 
    let(toks2, expr) = parse_UnaryExpr toks in
    match(lookahead toks2) with
    |Tok_Pow -> let toks3 = match_token toks2 Tok3_Pow in 
      let(toks4, expr2) = parse_PowerExpr toks3 in
      (toks4, Pow(expr, expr2))
    |_ -> (toks2, expr)

    and parse_UnaryExpr toks : expr_result = 
    match(lookahead toks) with
    |Tok_Not -> let tokA = match_token toks Tok_Not in
      let(toks3, expr) = parse_UnaryExpr tokA in
      (toks3, Not(expr))  
    |_ -> parse_PrimaryExpr toks

    and parse_PrimaryExpr toks : expr_result = 
    match lookahead toks with
    | Tok_Int i -> let toks2 = match_token toks (Tok_Int i) in (toks2, Int i)
    | Tok_Bool i -> let toks2 = match_token toks (Tok_Bool i) in (toks2, Bool i)
    | Tok_ID i -> let toks2 = match_token toks (Tok_ID i) in (toks2, ID i)
    | Tok_LParen -> let toks2 = match_token toks (Tok_LParen) in 
                  let (toks3, expr) = parse_expr toks2 in
                  let toks4 = match_token toks3 Tok_RParen in (toks4, expr)
    | _ -> raise(InvalidInputException("Fail"))



let rec parse_stmt toks : stmt_result =
  match lookahead toks with 
  | Tok_Int_Type -> let toks2 = match_token toks (Tok_Int_Type) in 
    let var = get_id toks2 in let toks3 = match_token toks2 (Tok_ID var) in 
    let toks4 = match_token toks3 (Tok_Semi) in 
    let (toks5,stmt) = parse_stmt toks4 in (toks5, Seq(Declare(Int_Type, var), stmt))

  | Tok_Bool_Type -> let toks2 = match_token toks (Tok_Bool_Type) in 
    let var = get_id toks2 in let toks3 = match_token toks2 (Tok_ID var) in 
    let toks4 = match_token toks3 (Tok_Semi) in
    let (toks5,stmt) = parse_stmt toks4 in (toks5, Seq(Declare(Bool_Type, var), stmt))

  | Tok_ID i -> let toks2 = match_token toks (Tok_ID i) in let toks3 = match_token toks2 Tok_Assign in
    let (toks4,expr) = parse_expr toks3 in let toks5 = match_token toks4 Tok_Semi in 
    let (toks6,stmt) = parse_stmt toks5 in (toks6, Seq(Assign(i, expr), stmt))
  
  | Tok_Print -> let toksA = match_token toks Tok_Print in 
    let toks2 = match_token toksA Tok_LParen in let (toks3,expr) = parse_expr toks2 in 
    let toks4 = match_token toks3 Tok_RParen in let toks5 = match_token toks4 Tok_Semi in 
    let (toks6,stmt) = parse_stmt toks5 in (toks6, Seq(Print(expr), stmt))

  | Tok_If -> let toksA = match_token toks Tok_If in 
    let toks2 = match_token toksA Tok_LParen in let (toks3,expr) = parse_expr toks2 in 
    let toks4 = match_token toks3 Tok_RParen in let toks5 = match_token toks4 Tok_LBrace in 
    let (toks6,stmt) = parse_stmt toks5 in let toks7 = match_token toks6 Tok_RBrace in 
    let (toks8, stmt2) = parse_else toks7 in let (toks9,stmt3) = parse_stmt toks8 in 
    (toks9, Seq(If(expr,stmt,stmt2), stmt3))
  
  | Tok_For -> let toks2 = match_token toks Tok_For in let toks3 = match_token toks2 Tok_LParen in 
    let var = get_id toks3 in let toks4 = match_token toks3 (Tok_ID var) in 
    let toks5 = match_token toks4 Tok_From in let (toks6,expr1) = parse_expr toks5 in 
    let toks7 = match_token toks6 Tok_To in let (toks8,expr2) = parse_expr toks7 in 
    let toks9 = match_token toks8 Tok_RParen in let toks10 = match_token toks9 Tok_LBrace in 
    let (toks11, stmt) = parse_stmt toks10 in let toks12 = match_token toks11 Tok_RBrace in 
    let (toks13,stmt2) = parse_stmt toks12 in (toks13,Seq(For(var,expr1,expr2,stmt), stmt2))

  | Tok_While -> let toks2 = match_token toks Tok_While in let toks3 = match_token toks2 Tok_LParen in
    let (toks4,expr) = parse_expr toks3 in let toks5 = match_token toks4 Tok_RParen in
    let toks6 = match_token toks5 Tok_LBrace in let (toks7,stmt) = parse_stmt toks6 in 
    let toks8 = match_token toks7 Tok_RBrace in let (toks9,stmt2) = parse_stmt toks8 in
    (toks9, Seq(While(expr,stmt), stmt2))
  
  | EOF -> (toks,NoOp)
  | _ -> (toks,NoOp)
  
  and parse_else toks : stmt_result = 
    match lookahead toks with 
    |Tok_Else -> let toks2 = match_token toks Tok_Else in let toks3 = match_token toks2 Tok_LBrace in 
    let (toks4,stmt) = parse_stmt toks3 in let toks5 = match_token toks4 Tok_RBrace in (toks5,stmt)
    |_ -> (toks,NoOp)

let parse_main toks : stmt =
  let toks2 = match_token toks Tok_Int_Type in let toks3 = match_token toks2 Tok_Main in 
  let toks4 = match_token toks3 Tok_LParen in let toks5 = match_token toks4 Tok_RParen in 
  let toks6 = match_token toks5 Tok_LBrace in let (toks7, stmt) = parse_stmt toks6 in 
  let toks8 = match_token toks7 Tok_RBrace in let toks9 = match_token toks8 EOF in
  if(toks9 <> []) then raise(InvalidInputException("Fail")) else stmt


