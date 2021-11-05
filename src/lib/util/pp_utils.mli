module F = Format

type fmt = F.formatter

val pp_sc_cut: F.formatter -> unit -> unit
val pp_sc_spc: F.formatter -> unit -> unit
val pp_c_spc: F.formatter -> unit -> unit
val pp_sc_brk: F.formatter -> unit -> unit

val add_p: ?p:string -> (fmt -> 'a -> unit) -> fmt -> 'a -> unit

val pp_list:
  ?p:string ->
  ?pp_sep:(F.formatter -> unit -> unit) ->
  (F.formatter -> 'a -> unit) -> F.formatter -> 'a list -> unit

val pp_array:
  ?p:string ->
  ?pp_sep:(F.formatter -> unit -> unit) ->
  (F.formatter -> 'a -> unit) -> F.formatter -> 'a array -> unit

val pp_queue:
  ?p:string ->
  ?pp_sep:(F.formatter -> unit -> unit) ->
  (F.formatter -> 'a -> unit) -> F.formatter -> 'a Queue.t -> unit

val pp_stack:
  ?p:string ->
  ?pp_sep:(F.formatter -> unit -> unit) ->
  (F.formatter -> 'a -> unit) -> F.formatter -> 'a Stack.t -> unit

val pp_option:
  ?p:string ->
  (F.formatter -> 'a -> unit) -> F.formatter -> 'a option -> unit

val pp_doublet:
  ?boxed:bool -> ?p:string ->
  ?pp_sep1:(F.formatter -> unit -> unit) ->
  (F.formatter -> 'a -> unit) ->
  (F.formatter -> 'b -> unit) -> F.formatter -> 'a * 'b -> unit

val pp_triplet:
  ?p:string ->
  ?pp_sep1:(F.formatter -> unit -> unit) ->
  ?pp_sep2:(F.formatter -> unit -> unit) ->
  (F.formatter -> 'a -> unit) ->
  (F.formatter -> 'b -> unit) ->
  (F.formatter -> 'c -> unit) -> F.formatter -> 'a * 'b * 'c -> unit

val pp_quadruplet:
  ?p:string ->
  ?pp_sep1:(F.formatter -> unit -> unit) ->
  ?pp_sep2:(F.formatter -> unit -> unit) ->
  ?pp_sep3:(F.formatter -> unit -> unit) ->
  (F.formatter -> 'a -> unit) ->
  (F.formatter -> 'b -> unit) ->
  (F.formatter -> 'c -> unit) ->
  (F.formatter -> 'd -> unit) -> F.formatter -> 'a * 'b * 'c * 'd -> unit

val pp_quintuplet:
  ?p:string ->
  ?pp_sep1:(F.formatter -> unit -> unit) ->
  ?pp_sep2:(F.formatter -> unit -> unit) ->
  ?pp_sep3:(F.formatter -> unit -> unit) ->
  ?pp_sep4:(F.formatter -> unit -> unit) ->
  (F.formatter -> 'a -> unit) ->
  (F.formatter -> 'b -> unit) ->
  (F.formatter -> 'c -> unit) ->
  (F.formatter -> 'd -> unit) ->
  (F.formatter -> 'e -> unit) ->
  F.formatter -> 'a * 'b * 'c * 'd * 'e -> unit

val pp_set:
  ?p:string ->
  ?pp_sep:(F.formatter -> unit -> unit) ->
  (module Set.S with type elt = 'a and type t = 't) ->
  (F.formatter -> 'a -> unit) -> F.formatter -> 't -> unit

module type MS =
sig
  type key
  type +'a t
  val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val cardinal: 'a t -> int
end

module type HS = sig
  type key
  type 'a t
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val length : 'a t -> int
end

module MapPrinter(M: MS) : sig
  val pp:
    ?boxed:bool -> ?p:string ->
    ?pp_kv_sep:(fmt -> unit -> unit) ->
    ?pp_sep:(fmt -> unit -> unit) ->
    (fmt -> M.key -> unit) ->
    (fmt -> 'a -> unit) -> fmt -> 'a M.t -> unit
end

module HTPrinter(M: HS) : sig
  val pp:
    ?boxed:bool -> ?p:string ->
    ?pp_kv_sep:(fmt -> unit -> unit) ->
    ?pp_sep:(fmt -> unit -> unit) ->
    (fmt -> M.key -> unit) ->
    (fmt -> 'a -> unit) ->
    fmt -> 'a M.t -> unit
end