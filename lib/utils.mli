(** utilities for synopsis *)

type _identifier = string
type _module = string
type _call = string

(** defines a binding: is it recursive? what identifiers does it bind? *)
type _binding = bool * string list

(** defines a definition: what does it bind? what functions are called in its body? *)
type _definition = _binding list * _call list

(** a type that decribes certain aspects of a program's parsetree. *)
type _synopsis = {
    modules: _module list ;         (** a list of identifiers of modules opened/used *)
    definitions: _definition list ; (** a list of top-level _definitions *)
   }

(** set insert... not much else to say *)
val insert: 'a list -> 'a list -> 'a list
val get_desc: item:Parsetree.structure_item -> Parsetree.structure_item_desc
val get_exp_desc: expression:Parsetree.expression -> Parsetree.expression_desc
val get_pattern_desc: pattern:Parsetree.pattern -> Parsetree.pattern_desc
val get_names_from_lident: Longident.t -> _identifier list
val get_module_ident: Parsetree.open_declaration -> _identifier list

(** parses a Parsetree.pattern, gathering all identifiers being bound
    @param p pattern that is binding identifiers
    @return list of identifiers bound by pattern
  *)
val get_names_from_pattern: Parsetree.pattern -> _identifier list

(** parses a Parsetree.expression into a _definition, containing
    the bindings and function applications that occur
    @param exp expression being parsed
    @return _definition representing bindings and function calls
            happening inside the expression 
  *)
val get_bindings_calls: Parsetree.expression -> _definition

(** turns a Parsetree.value_binding list into a _definition for the items bound
    by the expression being parsed 
    @param rf whether or not the binding is recursive
    @param vb_lst the list of value bindings to convert
    @return _definition representing the value bindings
  *)
val deconstruct_binding_list: Asttypes.rec_flag -> Parsetree.value_binding list -> _definition

(** generates updates a synopsis with information via the structure item argument
    @param the item to deconstruct into a synopsis
    @param acc the _synopsis to accumulate information into
    @return new updated synopsis record
  *)
val get_synopsis: Parsetree.structure_item -> acc:_synopsis -> _synopsis