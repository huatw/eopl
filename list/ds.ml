(* expressed values & environments *)

type exp_val =
  | NumVal of int
  | BoolVal of bool
  | ListVal of exp_val list
and
  env = (string * exp_val) list

let empty_env (x: unit): env = []

let extend_env (env: env) (id: string) (e: exp_val): env = (id, e) :: env

let rec lookup (env: env) (id: string): exp_val =
  match env with
  | [] -> failwith "Id not found"
  | (key, value) :: t ->
    if id = key
    then value
    else lookup t id

let numVal_to_num = function
  | NumVal n -> n
  | _ -> failwith "Expected a number!"

let boolVal_to_bool = function
  | BoolVal b -> b
  | _ -> failwith "Expected a boolean!"

let listVal_to_list = function
  | ListVal l -> l
  | _ -> failwith "Expected a list!"

let rec string_of_expval = function
  | NumVal n -> "NumVal " ^ string_of_int n
  | BoolVal b -> "BoolVal " ^ string_of_bool b
  | ListVal l -> "ListVal " ^ string_of_listval l
and
  string_of_env e = String.concat ";" (List.map (fun (x, y) -> "(" ^ x ^ "," ^ string_of_expval y ^ ")") e)
and
  string_of_listval e = String.concat ";" (List.map (fun (x) -> "(" ^ string_of_expval x ^ ")") e)
