type _identifier = string
type _module = string
type _call = string

(** defines a binding: is it recursive? what identifiers does it bind? *)
type _binding = bool * string list

(** a type that decribes aspects of an ocaml program and its parsetree.
    private type ensures we can only generate a synopsis via this library *)
type _synopsis = {
    modules: _module list ;                             (** a list of modules opened/used *)
    definitions: (_binding list * _call list) list ;    (** a list of bindings and function calls *)
   }

(** set insert *)
val insert: 'a list -> 'a list -> 'a list
val get_desc: Parsetree.structure_item -> Parsetree.structure_item_desc
val get_exp_desc: Parsetree.expression -> Parsetree.expression_desc
val get_pattern_desc: Parsetree.pattern -> Parsetree.pattern_desc
val get_names_from_lident: Longident.t -> _identifier list
val get_module_ident: Parsetree.open_declaration -> _identifier list

(**  *)
val get_names_from_pattern: Parsetree.pattern -> _identifier list

(** *)
val get_bindings_calls: Parsetree.expression -> _binding list * _call list

(**  *)
val deconstruct_binding: 
    Asttypes.rec_flag -> _binding list * _call list -> Parsetree.value_binding -> _binding list * _call list

(**  *)
val get_synopsis: Parsetree.structure_item -> _synopsis -> _synopsis