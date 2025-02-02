(* consider rewriting to use the set library *)

(* treat list like set so that we don't get dupes *)
let insert p q = List.fold_left (fun a n -> if (List.mem n a) then a else a @ [n]) p q

let get_desc (x:Parsetree.structure_item) = match x with
 {pstr_desc=x;_}-> x;;

let get_exp_desc (x:Parsetree.expression) = match x with
 {pexp_desc=x} -> x
 |_ -> raise (Failure "malformed expression")

let get_pattern_desc(x:Parsetree.pattern) = match x with
 {ppat_desc=x} -> x
 |_ -> raise (Failure "malformed pattern")

(* gets string identifiers from a long identifier *)
let rec get_names_from_lident t = match t with
  Longident.Lident(x) -> [x]
  (* more robust naming convention for invocations using ., helps us determine illegal module usage*)
  |Longident.Ldot(t,s) -> [(List.fold_left (fun a n -> a ^ n ^ ".") "" (get_names_from_lident t)) ^ s]
  |Longident.Lapply(a,b) -> (get_names_from_lident a)@(get_names_from_lident b)

(* get module identifiers (they have a different structure) *)
let get_module_ident (x:Parsetree.open_declaration) = match x with
| {Parsetree.popen_expr =
  {Parsetree.pmod_desc = Parsetree.Pmod_ident ({Asttypes.txt = x})}} -> get_names_from_lident x

(* get all identifiers that the pattern is binding *)
let rec get_names_from_pattern (p:Parsetree.pattern) = 
  let desc = get_pattern_desc p in
  match desc with
  Ppat_any -> []
  |Ppat_var({Asttypes.txt=x;_}) -> [x]
  |Ppat_alias(pat,{Asttypes.txt=x;_}) -> x::(get_names_from_pattern pat)
  |Ppat_constant(_) -> []
  |Ppat_interval(_) -> []
  |Ppat_tuple(p_lst) -> List.fold_left (fun a x -> (get_names_from_pattern x) @ a) [] p_lst
  |Ppat_construct(_, o) -> 
    (match o with
     None -> []
     |Some(_ ,p) -> (get_names_from_pattern p))
  |Ppat_variant(_,None) -> []
  |Ppat_variant(_,Some(x)) -> get_names_from_pattern x
  |Ppat_record(lst,_) -> 
    List.fold_left (fun a ({Asttypes.txt=x;_},p) -> 
                      (get_names_from_lident x)@(get_names_from_pattern p) @ a) [] lst
  |Ppat_array(lst) -> List.fold_left (fun a x -> get_names_from_pattern x) [] lst
  |Ppat_or(a,b) -> (get_names_from_pattern a)@(get_names_from_pattern b)
  |_ -> []
 
(* goes through an expression type and gets variables names *)
let rec get_functions_from_expression (exp:Parsetree.expression) =
  (* get function calls and bindings from list of cases *)
  let parse_cases cs = 
    List.fold_left (fun (a, b) {Parsetree.pc_rhs=x} ->
                      let bindings, calls = get_functions_from_expression x in
                      (insert a bindings, insert b calls)) ([],[]) cs
  in match get_exp_desc exp with
  (* empty list because only care about identifiers from application *)
  |Pexp_ident(_) -> ([], [])
  |Pexp_let(rf,vb_lst,e) -> 
    let bindings, calls = 
      List.fold_left (fun (a, b) {Parsetree.pvb_pat=bindee; Parsetree.pvb_expr=expr; _} ->
                        (* get the variables being bound, and whether or not they are recursive *)
                        let binding = (rf = Asttypes.Recursive, get_names_from_pattern bindee) in
                        (* get any bindings that occur inside the expression being bound,
                            as well as any function calls inside that expression *)
                        let sub_bindings, sub_calls = get_functions_from_expression expr in
                        (insert a (binding::sub_bindings), insert b sub_calls)) ([], []) vb_lst
    in let bindings', calls' = get_functions_from_expression e
    in (insert bindings bindings', insert calls calls')
  |Pexp_function(_, _, Pfunction_cases(case_lst,_,_)) -> parse_cases case_lst
  |Pexp_function(_, _, Pfunction_body(e)) -> get_functions_from_expression e
  |Pexp_apply(e,lst) -> 
    (* get function being applied *)
    let bindings, calls = (match get_exp_desc e with
                           |Pexp_ident({Asttypes.txt=i; _}) -> ([], get_names_from_lident i)
                           |_ -> get_functions_from_expression e)
    (* get info from expressions it's being applied to *)
    in let bindings', calls' = List.fold_left (fun (a,b) (_,x) -> 
                                                let (bindings'',calls'') = get_functions_from_expression x in 
                                                (insert a bindings'', insert b calls'')) ([], []) lst
    in (insert bindings bindings', insert calls calls') 
  |Pexp_match(e, cs) -> 
    (* there could theoretically be function calls in a pattern, maybe expand this later *)
    let bindings, calls = get_functions_from_expression e in
    let bindings', calls' = parse_cases cs in
    (insert bindings bindings', insert calls calls')
  |Pexp_tuple(es) ->
    List.fold_left (fun (a, b) e -> let bindings, calls = get_functions_from_expression e in
                                    (insert a bindings, insert b calls)) ([], []) es
  |Pexp_record(cs, _) ->
    List.fold_left (fun (a, b) (i, e) -> let bindings, calls = get_functions_from_expression e in
                                          (insert a bindings, insert b calls)) ([], []) cs
  |Pexp_setfield(_, _, _) -> raise (Failure "Illegal use of set (in a mutable field context)")
  |Pexp_array(_) -> raise (Failure "Illegal use of array construct")
  |Pexp_ifthenelse(guard, t_branch, e_opt) ->
    let bindings, calls = get_functions_from_expression guard in
    let bindings', calls' = get_functions_from_expression t_branch in
    let bindings'', calls'' = 
      (match e_opt with
       |None -> ([], [])
       |Some(e_branch) -> get_functions_from_expression e_branch)
    in (insert bindings (insert bindings' bindings''), insert calls (insert calls' calls''))
  |Pexp_sequence(_, _) -> raise (Failure "Illegal use of ; (sequence) construct")
  |Pexp_while(_, _) -> raise (Failure "Illegal use of while loop construct")
  |Pexp_for(_, _, _, _, _) -> raise (Failure "Illegal use of for loop construct")
  |_ -> ([],[])


(* (<list of module identifier > * <list of call graph>) *)
(* call graph : <list of binding> * <list of function call> *)
(* binding : (<is recursive?> * <list of identifier>*)
(* module identifier : string *)
(* identifier : string *)
(* is recursive? : bool *)
(* function calls : string, delimited with '.' *)
(* gets info from every item in the structure (modules, toplevel declarations, evalutations) *)
(* i think that this function will always return the function calls in the order they appear *)
let get_modules_and_graphs (i:Parsetree.structure_item) ((a, b) : string list * ((bool * string list) list * string list) list) = 
  let desc = get_desc i in
  match desc with
  |Parsetree.Pstr_open (e) -> ((a @ get_module_ident e), b)
  |Parsetree.Pstr_eval(e, _) -> (a, (b @ [get_functions_from_expression e]))
  |Parsetree.Pstr_value(rf,vb_lst) -> 
    let res = List.fold_left (fun (a, b) {Parsetree.pvb_pat=bindee; Parsetree.pvb_expr=expr; _} ->
                                let binding = (rf = Asttypes.Recursive, get_names_from_pattern bindee) in
                                let sub_bindings, calls = get_functions_from_expression expr in
                                (insert a (binding::sub_bindings), insert b calls)) ([], []) vb_lst 
    in (a, (b @ [res]))
  |_ -> raise (Failure "unknown parsetree type uh oh")

(* test utility *)
let modules_and_graphs_from_string (s:string) = 
  let tree = Parse.implementation (Lexing.from_string s) in
  List.fold_left (fun a item -> get_modules_and_graphs item a) ([], []) tree
