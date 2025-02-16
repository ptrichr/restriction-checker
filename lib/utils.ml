(** utilities for synopsis *)

module StringSet = Set.Make(String)

type _identifier = string
type _call = string
type _binding = bool * _identifier list
type _definition = _binding list * _call list
type _synopsis = {
    modules: _identifier list ;
    (* maybe we can just have an imperative bool? *)
    definitions: _definition list ;
   }

(* set union of multiple lists of strings *)
let union sets = StringSet.(to_list (List.fold_left (union) (StringSet.empty) (List.map (of_list) sets)))

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
  |_ -> raise (Failure "Weird module identifier. This probably isn't allowed")

let add_module_binding mods ({pmb_name={Asttypes.txt=opt; _}; _}:Parsetree.module_binding) =
  match opt with
  |None -> mods
  |Some(name) -> name::mods

let rec get_names_from_pattern (p:Parsetree.pattern) = 
  match get_pattern_desc ~pattern:p with
  |Ppat_any -> []
  |Ppat_var({Asttypes.txt=x;_}) -> [x]
  |Ppat_alias(pat,{Asttypes.txt=x;_}) -> union [get_names_from_pattern pat; [x]]
  |Ppat_constant(_) -> []
  |Ppat_interval(_) -> []
  |Ppat_tuple(p_lst) -> List.fold_left (fun a x -> union [a; get_names_from_pattern x]) [] p_lst
  |Ppat_construct(_, o) -> 
    (match o with
     |None -> []
     |Some(_ ,p) -> get_names_from_pattern p)
  |Ppat_variant(_,None) -> []
  |Ppat_variant(_,Some(x)) -> get_names_from_pattern x
  |Ppat_record(lst,_) -> 
    List.fold_left (fun a ({Asttypes.txt=x;_},p) -> 
                      union [a; get_names_from_lident x; get_names_from_pattern p]) [] lst
  |Ppat_array(lst) -> List.fold_left (fun a x -> union [a; get_names_from_pattern x]) [] lst
  |Ppat_or(a,b) -> union [get_names_from_pattern a; get_names_from_pattern b]
  |_ -> []

(* are bindings guaranteed to be unique? if no, which ones do we care about? the most recent? *)
let rec get_bindings_calls (exp:Parsetree.expression) =
  let parse_cases cs = 
    List.fold_left (fun (a, b) {Parsetree.pc_rhs=x; _} ->
                      let bindings, calls = get_bindings_calls x in
                      (a @ bindings, union [b; calls])) ([], []) cs
  in match get_exp_desc ~expression:exp with
  |Pexp_ident(_) -> ([], [])
  |Pexp_let(rf,vb_lst,e) -> 
    let bindings, calls = deconstruct_binding_list rf vb_lst
    in let bindings', calls' = get_bindings_calls e
    in (bindings @ bindings', union [calls; calls'])
  |Pexp_function(_, _, Pfunction_cases(case_lst,_,_)) -> parse_cases case_lst
  |Pexp_function(_, _, Pfunction_body(e)) -> get_bindings_calls e
  |Pexp_apply(e,lst) -> 
    (* gets the bindings and calls from the function applied *)
    let bindings, calls = 
      (match get_exp_desc ~expression:e with
       |Pexp_ident({Asttypes.txt=i; _}) -> ([], get_names_from_lident i)
       |_ -> get_bindings_calls e)
    (* get the bindings and calls from the arguments *)
    in List.fold_left (fun (a,b) (_,x) -> 
        let (bindings', calls') = get_bindings_calls x in 
        (a @ bindings', union [b; calls'])) (bindings, calls) lst
  |Pexp_match(e, cs) -> 
    let bindings, calls = get_bindings_calls e in
    let bindings', calls' = parse_cases cs in
    (bindings @ bindings', union [calls; calls'])
  |Pexp_tuple(es) ->
    List.fold_left (fun (a, b) e -> let bindings, calls = get_bindings_calls e in
                                    (a @ bindings, union [b; calls])) ([], []) es
  |Pexp_record(cs, _) ->
    List.fold_left (fun (a, b) (_, ex) -> let bindings, calls = get_bindings_calls ex in
                                          (a @ bindings, union [b; calls])) ([], []) cs
  |Pexp_setfield(_, _, _) -> raise (Failure "Illegal use of mutable field")
  |Pexp_array(_) -> raise (Failure "Illegal use of array construct")
  |Pexp_ifthenelse(guard, t_branch, e_opt) ->
    let bindings, calls = get_bindings_calls guard in
    let bindings', calls' = get_bindings_calls t_branch in
    let bindings'', calls'' = 
      (match e_opt with
       |None -> ([], [])
       |Some(e_branch) -> get_bindings_calls e_branch)
    in (bindings @ bindings' @ bindings'', union [calls; calls'; calls''])
  |Pexp_sequence(_, _) -> raise (Failure "Illegal use of ; (sequence) construct")
  |Pexp_while(_, _) -> raise (Failure "Illegal use of while loop construct")
  |Pexp_for(_, _, _, _, _) -> raise (Failure "Illegal use of for loop construct")
  |_ -> ([],[])

and deconstruct_binding_list rf vb_lst = 
  let deconstruct_binding ~acc:(a, b) ~binding:{Parsetree.pvb_pat=bindee; Parsetree.pvb_expr=expr; _} =
    let binding = (rf = Asttypes.Recursive, get_names_from_pattern bindee)
    in let sub_bindings, calls = get_bindings_calls expr
    in (binding::(sub_bindings @ a), union [b; calls])
  in List.fold_left (fun a n -> deconstruct_binding ~acc:a ~binding:n) ([], []) vb_lst

let get_synopsis (item:Parsetree.structure_item) ~acc:{modules=m; definitions=d} = 
  let default = {modules = m; definitions = d} in
  match get_desc ~item:item with
  |Pstr_open (e) -> {modules = union [m; get_module_ident e]; definitions = d}
  |Pstr_eval(e, _) -> {modules = m; definitions = d @ [get_bindings_calls e]}
  |Pstr_value(rf,vb_lst) -> {modules = m; definitions = d @ [deconstruct_binding_list rf vb_lst]}
  (* everything after this shouldn't be used by students *)
  |Pstr_module(b) -> {modules = add_module_binding m b; definitions = d}
  |Pstr_recmodule(lst) -> {modules = List.fold_left (add_module_binding) m lst; definitions = d}
  |_ -> default