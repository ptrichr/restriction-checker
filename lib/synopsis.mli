(** generate a synopsis from a string
    @param str ocaml expression represented as a string 
    @return synopsis of ocaml expression *)
val read_string: string -> Utils._synopsis

(** generate a synopsis from a file
    @param src filepath to read from
    @return synopsis of file read *)
val read_file: string -> Utils._synopsis

(** determines if a synopsis contains illegal expressions, set by a list of constraints.
    @param constr defines usage constraints
    @param items items to check against constraints
    @param mode defines whether or not constr is a blacklist or whitelist (true is whitelist)
    @return whether or not the items breach constraints *)
val contains_illegal_expr: string list -> string list -> bool -> bool