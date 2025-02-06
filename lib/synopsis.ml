(** top-level synopsis fuctions, used for enforcing restrictions *)

let read_string str = 
  let tree = Parse.implementation (Lexing.from_string str) in
  List.fold_left (fun a item -> Utils.get_synopsis item ~acc:a) {modules = []; definitions = []} tree

let read_file src = 
  let inchan = open_in src in
  let parsetree = Parse.implementation (Lexing.from_channel inchan) in
  List.fold_left (fun a item -> Utils.get_synopsis item ~acc:a) {modules = []; definitions = []} parsetree

let contains_illegal_expr constr items mode =
  let res = List.fold_left (fun a i -> (if mode then (&&) else (||)) a (List.mem i constr)) mode items in
  if mode then not res else res