open SmallCTypes
open EvalUtils
open TokenTypes


exception TypeError of string
exception DeclareError of string
exception DivByZeroError

let rec getId id (env : environment) : value = 
  match env with 
  |(s,v)::_ when s = id -> v
  |_::t -> getId id t
  |[] -> raise(DeclareError("It Don't Exist Idiot"))

let rec pow x y = 
  if y < 0 then 0
  else if y = 0 then 1
  else x * pow x (y-1)
  
let rec checkType v = 
  match v with 
  |Int_Val i -> true
  |Bool_Val i -> false 

let rec getInt v =
  match v with 
  |Int_Val x -> x
  |Bool_Val x -> raise(TypeError("Expected Int"))

let rec getBool v : bool =
  match v with 
  |Int_Val x -> raise(TypeError("Expected Bool"))
  |Bool_Val x -> x

let rec eval_expr env t =
  match t with 
  |Int i -> Int_Val i
  |Bool i -> Bool_Val i
  |ID i -> getId i env
  |Add(e1,e2) -> let v1 = getInt (eval_expr env e1) in 
                 let v2 = getInt (eval_expr env e2) in
                 Int_Val(v1 + v2)
  |Sub(e1,e2) -> let v1 = getInt (eval_expr env e1) in 
                 let v2 = getInt (eval_expr env e2) in
                 Int_Val(v1 - v2)
  |Mult(e1,e2) -> let v1 = getInt (eval_expr env e1) in 
                  let v2 = getInt (eval_expr env e2) in
                  Int_Val(v1 * v2)
  |Div(e1,e2) -> let v1 = getInt (eval_expr env e1) in 
                 let v2 = getInt (eval_expr env e2) in
                 if(v2 = 0) then raise(DivByZeroError) else
                 Int_Val(v1 / v2)
  |Pow(e1,e2) -> let v1 = getInt (eval_expr env e1) in 
                 let v2 = getInt (eval_expr env e2) in
                 Int_Val(pow v1 v2)
  |Or(e1,e2) -> let v1 = getBool (eval_expr env e1) in 
                let v2 = getBool (eval_expr env e2) in
                Bool_Val(v1 || v2)
  |And(e1,e2) -> let v1 = getBool (eval_expr env e1) in 
                 let v2 = getBool (eval_expr env e2) in
                 Bool_Val(v1 && v2)
  |Not(e1) -> let v1 =getBool (eval_expr env e1) in 
              Bool_Val(not(v1))
  |Greater(e1,e2) -> let v1 = getInt (eval_expr env e1) in 
                  let v2 = getInt (eval_expr env e2) in
                  Bool_Val(v1>v2)
  |Less(e1,e2) -> let v1 = getInt (eval_expr env e1) in 
                  let v2 = getInt (eval_expr env e2) in
                  Bool_Val(v1<v2)
  |GreaterEqual(e1,e2) -> let v1 = getInt (eval_expr env e1) in 
                          let v2 = getInt (eval_expr env e2) in
                          Bool_Val(v1>=v2)
  |LessEqual(e1,e2) -> let v1 = getInt (eval_expr env e1) in 
                       let v2 = getInt (eval_expr env e2) in
                       Bool_Val(v1<=v2)
  |Equal(e1,e2) -> if checkType (eval_expr env e1) then 
        let v1 = getInt (eval_expr env e1) in 
        let v2 = getInt (eval_expr env e2) in
        Bool_Val(v1=v2)
        else
        let v1 = getBool (eval_expr env e1) in 
        let v2 = getBool (eval_expr env e2) in
        Bool_Val(v1=v2)
  |NotEqual(e1,e2) -> if checkType (eval_expr env e1) then 
        let v1 = getInt (eval_expr env e1) in 
        let v2 = getInt (eval_expr env e2) in
        Bool_Val(v1<>v2)
        else
        let v1 = getBool (eval_expr env e1) in 
        let v2 = getBool (eval_expr env e2) in
        Bool_Val(v1<>v2)        

let rec checkId env id = 
  match env with 
  |[] -> id
  |(s,v):: t when s = id -> raise(DeclareError("It Exists Idiot"))
  |(s,v):: t -> checkId t id

let rec eval_stmt env s =
  match s with 
  |NoOp -> env 
  |Seq(s1,s2) -> let env1 = eval_stmt env s1 in eval_stmt env1 s2
  |Declare(d1, id) -> let id = checkId env id in 
      if(d1 = Int_Type) then ((id,Int_Val 0))::env else ((id,Bool_Val false))::env

  |Assign(id,e1) -> let v = getId id env in 
    if(checkType v) then let v1 = getInt (eval_expr env e1) in (id, Int_Val v1)::env
    else let v1 = getBool (eval_expr env e1) in (id,Bool_Val v1)::env

  |If(e,s1,s2) -> let b = getBool (eval_expr env e) in 
    if(b) then (eval_stmt env s1)
    else (eval_stmt env s2)

  |While(e,s1) -> let b = getBool (eval_expr env e) in 
   if(b) then let env2 = (eval_stmt env s1) in (eval_stmt env2 (While(e,s1)))
   else env
  
  |For(id,e1,e2,s) -> 
    let rec forHelp id stop s (fev:environment) = 
        if(getInt (getId id fev)<= stop) then 
        let env1 = (eval_stmt fev s) in
        let start = getInt (getId id env1) in 
        let start = start + 1 in
        let env2 = eval_stmt env1 (Assign(id,Int(start)))
        in forHelp id stop s env2
      else
        fev
    in
    let v = getId id env in if(checkType v) then 
    let start = getInt (eval_expr env e1) in let stop = getInt (eval_expr env e2) in
    let env3 = eval_stmt env (Assign(id,Int(start))) in forHelp id stop s env3
    else raise(DeclareError("the start int is not an int"))
  
  |Print(e1) -> let out = eval_expr env e1 in
      match out with 
      |Int_Val i ->print_output_int i;print_output_newline(); env 
      |Bool_Val i ->print_output_bool i; print_output_newline(); env
  