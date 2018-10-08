open Ast
open Ds

let rec eval (en: env) (e: expr): exp_val =
  match e with
  | Int n -> NumVal n
  | Var x -> lookup en x
  | Let (x, e1, e2) ->
    let v1 = eval en e1 in
    let extended_en = extend_env en x v1 in
    eval extended_en e2
  | IsZero e1 ->
    let v1 = eval en e1 in
    let n1 = numVal_to_num v1 in
    BoolVal (n1 = 0)
  | ITE (e1, e2, e3) ->
    let v1 = eval en e1 in
    let b1 = boolVal_to_bool v1 in
    if b1 then eval en e2 else eval en e3
  | Sub (e1, e2) ->
    let v1 = eval en e1 in
    let v2 = eval en e2 in
    NumVal ((numVal_to_num v1) - (numVal_to_num v2))
  | Add (e1, e2) ->
    let v1 = eval en e1 in
    let v2 = eval en e2 in
    NumVal ((numVal_to_num v1) + (numVal_to_num v2))
  | Div (e1, e2) ->
    let v1 = eval en e1 in
    let v2 = eval en e2 in
    NumVal ((numVal_to_num v1) / (numVal_to_num v2))
  | Mul (e1, e2) ->
    let v1 = eval en e1 in
    let v2 = eval en e2 in
    NumVal ((numVal_to_num v1) * (numVal_to_num v2))
  | Abs e ->
    let v = eval en e in
    NumVal (abs (numVal_to_num v))
  | Cons (e1, e2) ->
    let v1 = eval en e1 in
    let v2 = eval en e2 in
    ListVal (v1 :: (listVal_to_list v2))
  | Hd e ->
    let v = eval en e in
    List.hd (listVal_to_list v)
  | Tl e ->
    let lst = listVal_to_list (eval en e) in
    ListVal (List.tl lst)
  | Null e ->
    let v = eval en e in
    BoolVal (List.length (listVal_to_list v) = 0)
  | EmptyList ->
    ListVal []


(* Parse a string into an ast *)
let parse s =
  s |> Lexing.from_string |> Parser.prog Lexer.read

(* Interpret an expression *)
let interp (e: string): exp_val =
  e |> parse |> eval (empty_env ())
