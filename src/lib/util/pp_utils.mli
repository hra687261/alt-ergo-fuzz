
module F = Format

type fmt = F.formatter

type box_type

val hbox: int -> box_type
val vbox: int -> box_type
val hvbox: int -> box_type
val hovbox: int -> box_type
val box: int -> box_type

val pp_sc_cut: fmt -> unit -> unit
val pp_sc_spc: fmt -> unit -> unit
val pp_c_spc: fmt -> unit -> unit
(* val pp_sc_brk: fmt -> unit -> unit *)

val set_dbox: box_type -> unit
val get_dbox: unit -> box_type

val set_dsep: (fmt -> unit -> unit) -> unit
val get_dsep: unit -> (fmt -> unit -> unit)


val set_dpref: string -> unit
val get_dpref: unit -> string


val add_p:
  ?p:string -> (fmt -> 'a -> unit) -> fmt -> 'a -> unit

val enbox_pp:
  ?bt:box_type -> (fmt -> 'a -> unit) -> fmt -> 'a -> unit


val pp_option:
  ?p:string -> (fmt -> 'a -> unit) ->
  fmt -> 'a option -> unit

val pp_doublet:
  ?p:string ->
  ?pp_sep1:(fmt -> unit -> unit) ->
  (fmt -> 'a -> unit) ->
  (fmt -> 'b -> unit) ->
  fmt -> 'a * 'b -> unit

val pp_triplet:
  ?p:string ->
  ?pp_sep1:(fmt -> unit -> unit) ->
  ?pp_sep2:(fmt -> unit -> unit) ->
  (fmt -> 'a -> unit) ->
  (fmt -> 'b -> unit) ->
  (fmt -> 'c -> unit) ->
  fmt -> 'a * 'b * 'c -> unit

val pp_quadruplet:
  ?p:string ->
  ?pp_sep1:(fmt -> unit -> unit) ->
  ?pp_sep2:(fmt -> unit -> unit) ->
  ?pp_sep3:(fmt -> unit -> unit) ->
  (fmt -> 'a -> unit) ->
  (fmt -> 'b -> unit) ->
  (fmt -> 'c -> unit) ->
  (fmt -> 'd -> unit) ->
  fmt -> 'a * 'b * 'c * 'd -> unit

val pp_quintuplet:
  ?p:string ->
  ?pp_sep1:(fmt -> unit -> unit) ->
  ?pp_sep2:(fmt -> unit -> unit) ->
  ?pp_sep3:(fmt -> unit -> unit) ->
  ?pp_sep4:(fmt -> unit -> unit) ->
  (fmt -> 'a -> unit) ->
  (fmt -> 'b -> unit) ->
  (fmt -> 'c -> unit) ->
  (fmt -> 'd -> unit) ->
  (fmt -> 'e -> unit) ->
  fmt -> 'a * 'b * 'c * 'd * 'e -> unit


val pp_list:
  ?p:string -> ?pp_sep:(fmt -> unit -> unit) ->
  (fmt -> 'a -> unit) -> fmt -> 'a list -> unit

val pp_array:
  ?p:string -> ?pp_sep:(fmt -> unit -> unit) ->
  (fmt -> 'a -> unit) -> fmt -> 'a array -> unit

val pp_queue:
  ?p:string -> ?pp_sep:(fmt -> unit -> unit) ->
  (fmt -> 'a -> unit) -> fmt -> 'a Queue.t -> unit

val pp_stack:
  ?p:string -> ?pp_sep:(fmt -> unit -> unit) ->
  (fmt -> 'a -> unit) -> fmt -> 'a Stack.t -> unit

val pp_set:
  ?p:string -> ?pp_sep:(fmt -> unit -> unit) ->
  (module Set.S with type elt = 'a and type t = 't) ->
  (fmt -> 'a -> unit) -> fmt -> 't -> unit

module type MS = sig
  type key
  type 'a t
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val cardinal : 'a t -> int
end

module MapPrinter(M: MS): sig
  val pp:
    ?p:string ->
    ?pp_kv_sep:(fmt -> unit -> unit) ->
    ?pp_sep:(fmt -> unit -> unit) ->
    (fmt -> M.key -> unit) ->
    (fmt -> 'a -> unit) ->
    fmt -> 'a M.t -> unit
end

module type HTS = sig
  type key
  type 'a t
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val length : 'a t -> int
end

module HTPrinter(M: HTS): sig
  val pp:
    ?p:string ->
    ?pp_kv_sep:(fmt -> unit -> unit) ->
    ?pp_sep:(fmt -> unit -> unit) ->
    (fmt -> M.key -> unit) ->
    (fmt -> 'a -> unit) ->
    fmt -> 'a M.t -> unit
end
