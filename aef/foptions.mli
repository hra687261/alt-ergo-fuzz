
type interval = {lb: int; ub: int}

(** Query (goal) max depth *)
val qmd_i : interval

(** Axiom max depth *)
val amd_i : interval

(** Function max depth *)
val fmd_i : interval

(** Number of uninterpreted variables by type *)
val nuv_i : interval

(** Number of quantified variables by type *)
val nqv_i : interval

(** Number of type declarations *)
val ntd_i : interval

(** Number of statements *)
val nst_i : interval


val get_qmd: unit -> int
val get_amd: unit -> int
val get_fmd: unit -> int
val get_nuv: unit -> int
val get_nqv: unit -> int
val get_ntd: unit -> int
val get_nst: unit -> int


val get_u_qvrs: unit -> bool
val get_u_adts: unit -> bool
val get_u_li: unit -> bool
val get_u_ite: unit -> bool
val get_u_fa: unit -> bool
val get_u_btv: unit -> bool


val set_qmd: int -> unit
val set_amd: int -> unit
val set_fmd: int -> unit
val set_nuv: int -> unit
val set_nqv: int -> unit
val set_ntd: int -> unit
val set_nst: int -> unit


val set_u_qvrs: bool -> unit
val set_u_adts: bool -> unit
val set_u_li: bool -> unit
val set_u_ite: bool -> unit
val set_u_fa: bool -> unit
val set_u_btv: bool -> unit
