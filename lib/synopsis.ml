open Utils

(** (testing util) generate synopsis from string
    @param str ocaml expression represented as a string 
    @return synopsis of ocaml expression *)
let read_string (str:string) = 
  let tree = Parse.implementation (Lexing.from_string str) in
  List.fold_left (fun a item -> get_synopsis item a) ([], []) tree

(** generate synopsis from input channel
    @param src filepath to read from
    @return synopsis of file read *)
let read_file (src:string) = 
  let inchan = open_in src in
  let parsetree = Parse.implementation (Lexing.from_channel inchan) in
  List.fold_left (fun a item -> get_synopsis item a) ([], []) parsetree

(** determines if a synopsis contains illegal expressions, set by a list of constraints.
    @param constr defines usage constraints
    @param items items to check against constraints
    @param mode defines whether or not constr is a blacklist or whitelist (true is whitelist)e
    @return whether or not the items breach constraints *)
let contains_illegal_expr (constr:string list) (items:string list) (mode:bool) : bool =
  let res = List.fold_left (fun a i -> (if mode then (&&) else (||)) a (List.mem i constr)) mode items in
  if mode then not res else res