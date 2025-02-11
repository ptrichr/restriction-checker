(** utilities for synopsis *)

type _identifier = string
type _call = string
type _def_type = 
  Recursive | Non_recursive
type _binding = _def_type * _identifier list
type _definition = _binding list * _call list
type _synopsis = {
    modules: _identifier list ;
    definitions: _definition list ;
   }

let insert p q = List.fold_left (fun a n -> if (List.mem n a) then a else a @ [n]) p q

let get_desc ~item:({pstr_desc=d; _}:Parsetree.structure_item) = d

let get_exp_desc ~expression:({pexp_desc=d; _}:Parsetree.expression) = d

let get_pattern_desc ~pattern:({ppat_desc=d; _}:Parsetree.pattern) = d

let rec get_names_from_lident (id:Longident.t) = 
  match id with
  |Longident.Lident(x) -> [x]
  |Longident.Ldot(_, _) -> [String.concat "." (List.rev (Longident.flatten id))]
  |Longident.Lapply(a,b) -> (get_names_from_lident a)@(get_names_from_lident b)

let get_module_ident (dec:Parsetree.open_declaration) = 
  match dec with
  |{popen_expr =
      {pmod_desc = Pmod_ident({Asttypes.txt = lid; _}); _}; _} -> get_names_from_lident lid
  |_ -> raise (Failure "Could not get module identifier")

let rec get_names_from_pattern (p:Parsetree.pattern) = 
  match get_pattern_desc ~pattern:p with
  |Ppat_any -> []
  |Ppat_var({Asttypes.txt=x;_}) -> [x]
  |Ppat_alias(pat,{Asttypes.txt=x;_}) -> insert (get_names_from_pattern pat) [x]
  |Ppat_constant(_) -> []
  |Ppat_interval(_) -> []
  |Ppat_tuple(p_lst) -> List.fold_left (fun a x -> insert a (get_names_from_pattern x)) [] p_lst
  |Ppat_construct(_, o) -> 
    (match o with
     |None -> []
     |Some(_ ,p) -> get_names_from_pattern p)
  |Ppat_variant(_,None) -> []
  |Ppat_variant(_,Some(x)) -> get_names_from_pattern x
  |Ppat_record(lst,_) -> 
    List.fold_left (fun a ({Asttypes.txt=x;_},p) -> 
                      insert (insert a (get_names_from_lident x)) (get_names_from_pattern p)) [] lst
  |Ppat_array(lst) -> List.fold_left (fun a x -> insert a (get_names_from_pattern x)) [] lst
  |Ppat_or(a,b) -> insert (get_names_from_pattern a) (get_names_from_pattern b)
  |_ -> []
 
let rec get_bindings_calls (exp:Parsetree.expression) =
  let parse_cases cs = 
    List.fold_left (fun (a, b) {Parsetree.pc_rhs=x; _} ->
                      let bindings, calls = get_bindings_calls x in
                      (insert a bindings, insert b calls)) ([],[]) cs
  in match get_exp_desc ~expression:exp with
  |Pexp_ident(_) -> ([], [])
  |Pexp_let(rf,vb_lst,e) -> 
    let bindings, calls = deconstruct_binding_list rf vb_lst
    in let bindings', calls' = get_bindings_calls e
    in (insert bindings bindings', insert calls calls')
  |Pexp_function(_, _, Pfunction_cases(case_lst,_,_)) -> parse_cases case_lst
  |Pexp_function(_, _, Pfunction_body(e)) -> get_bindings_calls e
  |Pexp_apply(e,lst) -> 
    (* gets the bindings and calls from the function applied *)
    let bindings, calls = 
      (match get_exp_desc ~expression:e with
       |Pexp_ident({Asttypes.txt=i; _}) -> ([], get_names_from_lident i)
       |_ -> get_bindings_calls e)
    (* get the bindings and calls from the arguments *)
    in let bindings', calls' = List.fold_left (fun (a,b) (_,x) -> 
                                                let (bindings'', calls'') = get_bindings_calls x in 
                                                (insert a bindings'', insert b calls'')) ([], []) lst
    in (insert bindings bindings', insert calls calls') 
  |Pexp_match(e, cs) -> 
    let bindings, calls = get_bindings_calls e in
    let bindings', calls' = parse_cases cs in
    (insert bindings bindings', insert calls calls')
  |Pexp_tuple(es) ->
    List.fold_left (fun (a, b) e -> let bindings, calls = get_bindings_calls e in
                                    (insert a bindings, insert b calls)) ([], []) es
  |Pexp_record(cs, _) ->
    List.fold_left (fun (a, b) (_, ex) -> let bindings, calls = get_bindings_calls ex in
                                          (insert a bindings, insert b calls)) ([], []) cs
  |Pexp_setfield(_, _, _) -> raise (Failure "Illegal use of mutable field")
  |Pexp_array(_) -> raise (Failure "Illegal use of array construct")
  |Pexp_ifthenelse(guard, t_branch, e_opt) ->
    let bindings, calls = get_bindings_calls guard in
    let bindings', calls' = get_bindings_calls t_branch in
    let bindings'', calls'' = 
      (match e_opt with
       |None -> ([], [])
       |Some(e_branch) -> get_bindings_calls e_branch)
    in (insert bindings (insert bindings' bindings''), insert calls (insert calls' calls''))
  |Pexp_sequence(_, _) -> raise (Failure "Illegal use of ; (sequence) construct")
  |Pexp_while(_, _) -> raise (Failure "Illegal use of while loop construct")
  |Pexp_for(_, _, _, _, _) -> raise (Failure "Illegal use of for loop construct")
  |_ -> ([],[])

and deconstruct_binding_list rf vb_lst = 
  let deconstruct_binding ~acc:(a, b) ~binding:{Parsetree.pvb_pat=bindee; Parsetree.pvb_expr=expr; _} =
    let binding = ((if rf = Asttypes.Recursive then Recursive else Non_recursive), get_names_from_pattern bindee)
    in let sub_bindings, calls = get_bindings_calls expr
    in (insert a (binding::sub_bindings), insert b calls)
  in List.fold_left (fun a n -> deconstruct_binding ~acc:a ~binding:n) ([], []) vb_lst

let get_synopsis (item:Parsetree.structure_item) ~acc:{modules=m; definitions=d} = 
  match get_desc ~item:item with
  |Pstr_open (e) -> {modules = insert m (get_module_ident e); definitions = d}
  |Pstr_eval(e, _) -> {modules = m; definitions = d @ [get_bindings_calls e]}
  |Pstr_value(rf,vb_lst) -> {modules = m; definitions = d @ [deconstruct_binding_list rf vb_lst]}
  |Pstr_type(_) -> {modules = m; definitions = d}
  |_ -> raise (Failure "Unknown parsetree type?")