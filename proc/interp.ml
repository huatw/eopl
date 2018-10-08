open Ast
open Ds

let init_env =
  ExtendEnv("i", NumVal 1,
   ExtendEnv("v", NumVal 5,
    ExtendEnv("x", NumVal 10,
      EmptyEnv)))

let rec apply_proc f a =
  let (x, e, env) = procVal_of_proc f in
  eval_expr (extend_env env x a) e
  (* match f with
    ProcVal (x,b,env) -> eval_expr (extend_env env x a) b
  | _ -> failwith "apply_proc: Not a procVal" *)
and
  eval_expr (en: env) (e: expr): exp_val =
  match e with
  | Int n -> NumVal n
  | Var id ->
    (match apply_env en id with
    | None -> failwith @@ "Variable " ^ id ^ " undefined"
    | Some ev -> ev)
  | Let (x, e1, e2) ->
    let v1 = eval_expr en e1 in
    eval_expr (extend_env en x v1) e2
  | Add (e1, e2) ->
    let v1 = eval_expr en e1 in
    let v2 = eval_expr en e2  in
    NumVal ((numVal_to_num v1) + (numVal_to_num v2))
  | Sub (e1, e2) ->
    let v1 = eval_expr en e1 in
    let v2 = eval_expr en e2 in
    NumVal ((numVal_to_num v1) - (numVal_to_num v2))
  | IsZero e ->
    let v1 = eval_expr en e in
    BoolVal (numVal_to_num v1 = 0)
  |  ITE(e1, e2, e3) ->
    let v1 = eval_expr en e1
    in    if boolVal_to_bool v1
    then eval_expr en e2
    else eval_expr en e3
  | Proc (x, e) -> ProcVal (x, e, en)
  | App (e1, e2) ->
    let v1 = eval_expr en e1 in
    let v2 = eval_expr en e2 in
    apply_proc v1 v2
  | _ -> failwith("Not implemented")
and
  eval_prog (AProg e) = eval_expr init_env e


let parse s =
  s |> Lexing.from_string |> Parser.prog Lexer.read

let lexer s =
  s |> Lexing.from_string |> Lexer.read

let interp (e: string): exp_val =
  e |> parse |> eval_prog

let ex1 = "
let x = 7
in let y = 2
   in let y = let x = x-1
              in x-y
      in (x-8)- y"

