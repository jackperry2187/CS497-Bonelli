open Ast
open Ds
open Errors

(** [eval_expr e] evaluates expression [e] *)
let rec eval_expr : expr -> int result =
  fun e ->
  match e with
  | Int n      -> return n
  | Add(e1,e2) ->
    eval_expr e1 >>= fun n ->
    eval_expr e2 >>= fun m ->
    return (n+m)   
  | Sub(e1,e2) ->
    eval_expr e1 >>= fun n ->
    eval_expr e2 >>= fun m ->
    return (n-m)   
  | Mul(e1,e2) ->
    eval_expr e1 >>= fun n ->
    eval_expr e2 >>= fun m ->
    return (n*m)   
  | Div(e1,e2) ->
    eval_expr e1 >>= fun n ->
    eval_expr e2 >>= fun m ->
    if m=0
    then error "Division by zero"
    else return (n/m)
  | Abs(e) ->
    eval_expr e >>= fun n ->
    return (abs n)
  | _ -> failwith "Not implemented yet!"


(** [parse s] parses string [s] into an ast *)
let parse (s:string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


(** [interp s] parses [s] and then evaluates it *)
let interp (e:string) : int result =
  e |> parse |> eval_expr

(** [parseWithErrors s] parses string [s] into an ast 
    AND prints useful errors if any are present *)
let parseWithErrors (s:string): expr =
  match firstPass s with
    | Succ expr -> expr
    | LexErr msg -> failwith msg
    | ParseErr s -> failwith (secondPass s)

(** [interpWithErrors s] parses [s] with useful parsing errors
    if they are present, or evaluates if they aren't *)
let interpWithErrors (s: string) : int result = 
  s |> parseWithErrors |> eval_expr



