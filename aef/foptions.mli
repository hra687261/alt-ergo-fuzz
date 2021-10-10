
val get_query_max_depth: unit -> int
val get_axiom_max_depth: unit -> int
val get_func_max_depth: unit -> int

val get_nb_us_vars: unit -> int
val get_nb_q_vars: unit -> int

val get_u_qvrs: unit -> bool
val get_u_adts: unit -> bool
val get_u_li: unit -> bool
val get_u_ite: unit -> bool
val get_u_fa: unit -> bool
val get_u_btv: unit -> bool


val set_query_max_depth: int -> unit
val set_axiom_max_depth: int -> unit
val set_func_max_depth: int -> unit

val set_nb_us_vars: int -> unit
val set_nb_q_vars: int -> unit

val set_u_qvrs: bool -> unit
val set_u_adts: bool -> unit
val set_u_li: bool -> unit
val set_u_ite: bool -> unit
val set_u_fa: bool -> unit
val set_u_btv: bool -> unit
