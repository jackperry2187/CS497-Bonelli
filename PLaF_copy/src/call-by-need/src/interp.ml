open Ast
open Ds

let g_store = Store.empty_store 20 (NumVal 0)

let rec addIds fs evs =
  match fs,evs with
  | [],[] -> []
  | (id,_)::t1, v::t2 -> (id,v):: addIds t1 t2
  | _,_ -> failwith "error: lists have different sizes"
            
let rec apply_clos : string*Ast.expr*env -> exp_val -> exp_val ea_result =
  fun (id,e,en) ev ->
  return en >>+
  extend_env id ev >>+
  eval_expr e
and
  value_of_operand =
  fun op ->
  match op with
  | Var id -> apply_env id
  | _ -> lookup_env  >>= fun en ->
    return (RefVal (Store.new_ref g_store (Thunk(op, en))))
and
  eval_expr : expr -> exp_val ea_result = fun e ->
  match e with
  | Int(n) -> return @@ NumVal n
  | Var(id) ->
    apply_env id >>=
    int_of_refVal >>= fun l ->
    Store.deref g_store l >>= fun ev ->
    (match ev with
     | Thunk(e,en) -> return en >>+ eval_expr e >>= fun ev ->
       Store.set_ref g_store l ev >>= fun _ ->
       return ev
    | _ -> return ev)
  (* | Letrec(id,par,e,target) ->
   *   extend_env_rec id par e >>+
   *   eval_expr target *)
  | Letrec(id,par,e,target) ->
    let l = Store.new_ref g_store UnitVal
    in 
    extend_env id (RefVal l) >>+
    (lookup_env >>= fun en ->
    Store.set_ref g_store l (ProcVal(par,e,en)) >>= fun _ ->
    eval_expr target)
  | Add(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return @@ NumVal (n1+n2)
  | Sub(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return @@ NumVal (n1-n2)
  | Mul(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return @@ NumVal (n1*n2)
  | Div(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    if n2==0
    then error "Division by zero"
    else return @@ NumVal (n1/n2)
  | Let(id,e1,e2) ->
    eval_expr e1 >>= fun w ->
    let l = Store.new_ref g_store w
    in extend_env id (RefVal l) >>+
    eval_expr e2
  | ITE(e1,e2,e3) ->
    eval_expr e1 >>=
    bool_of_boolVal >>= fun b ->
    if b 
    then eval_expr e2
    else eval_expr e3
  | IsZero(e) ->
    eval_expr e >>=
    int_of_numVal >>= fun n ->
    return @@ BoolVal (n = 0)
  | Pair(e1,e2) ->
    eval_expr e1 >>= fun ev1 ->
    eval_expr e2 >>= fun ev2 ->
    return @@ PairVal(ev1,ev2)
  | Fst(e) ->
    eval_expr e >>=
    pair_of_pairVal >>= fun p ->
    return @@ fst p 
  | Snd(e) ->
    eval_expr e >>=
    pair_of_pairVal >>= fun p ->
    return @@ snd p
  | Proc(id,e)  ->
    lookup_env >>= fun en ->
    return (ProcVal(id,e,en))
  | App(e1,e2)  ->
    eval_expr e1 >>= 
    clos_of_procVal >>= fun clos ->
    value_of_operand e2 >>= 
    apply_clos clos
  | Set(id,e) ->
    eval_expr e >>= fun ev ->
    apply_env id >>=
    int_of_refVal >>= fun l ->
    Store.set_ref g_store l ev >>= fun _ ->
    return UnitVal
  | BeginEnd([]) ->
    return UnitVal
  | BeginEnd(es) ->
    sequence (List.map eval_expr es) >>= fun l ->
    return (List.hd (List.rev l))
  | Record(fs) ->
    sequence (List.map (fun (_id,e) -> eval_expr e) fs) >>= fun evs ->
    return (RecordVal (addIds fs evs))
  | Proj(e,id) ->
    eval_expr e >>=
    fields_of_recordVal >>= fun fs ->
    (match List.assoc_opt id fs with
    | None -> error "not found"
    | Some ev -> return ev)
  | Unit -> return UnitVal
  | Debug(_e) ->
    string_of_env >>= fun str_env ->
    let str_store = Store.string_of_store string_of_expval g_store 
    in (print_endline (str_env^"\n"^str_store);
    error "Reached breakpoint")
  | _ -> failwith ("Not implemented: "^string_of_expr e)


             
(* Parse a string into an ast *)

let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let lexer s =
  let lexbuf = Lexing.from_string s
  in Lexer.read lexbuf 


(* Interpret an expression *)
let interp (s:string) : exp_val result =
  let c = s |> parse |> eval_expr
  in run c



