type fmt = Format.formatter

val set_indentsize:
  int -> unit

val get_indentsize:
  unit -> int

val addpref:
  (fmt -> 'a -> unit) ->
  (?p:string -> fmt -> 'a -> unit)

val addlb:
  (?p:string -> fmt -> 'a -> unit) ->
  (?p:string -> fmt -> 'a -> unit)

val pr_int:
  ?p:string -> fmt -> int -> unit
val pr_float:
  ?p:string -> fmt -> float -> unit
val pr_bool:
  ?p:string -> fmt -> bool -> unit

val print_list:
  (?p:string -> fmt -> 'a -> unit) ->
  (fmt -> 'a list -> unit)
val print_list_lb:
  ?ind:bool ->
  (?p:string -> fmt -> 'a -> unit) ->
  (?p:string -> fmt -> 'a list -> unit)

val print_array:
  (?p:string -> fmt -> 'a -> unit) ->
  (fmt -> 'a array -> unit)
val print_array_lb: ?slb:bool ->
  (?p:string -> fmt -> 'a -> unit) ->
  (?p:string -> fmt -> 'a array -> unit)


val print_queue:
  (?p:string -> fmt -> 'a -> unit) ->
  (fmt -> 'a Queue.t -> unit)
val print_queue_lb:
  (?p:string -> fmt -> 'a -> unit) ->
  (?p:string -> fmt -> 'a Queue.t -> unit)


val print_stack:
  (?p:string -> fmt -> 'a -> unit) ->
  (fmt -> 'a Stack.t -> unit)
val print_stack_lb:
  (?p:string -> fmt -> 'a -> unit) ->
  (?p:string -> fmt -> 'a Stack.t -> unit)


val print_opt:
  (?p:string -> fmt -> 'a -> unit) ->
  (fmt -> 'a option -> unit)
val print_opt_lb: ?slb:bool ->
  (?p:string -> fmt -> 'a -> unit) ->
  (?p:string -> fmt -> 'a option -> unit)


val print_doublet:
  (?p:string -> fmt -> 'a -> unit) *
  (?p:string -> fmt -> 'b -> unit) ->
  (fmt -> 'a * 'b -> unit)
val print_doublet_lb:
  (?p:string -> fmt -> 'a -> unit) *
  (?p:string -> fmt -> 'b -> unit) ->
  (?p:string -> fmt -> 'a * 'b -> unit)


val print_triplet:
  (?p:string -> fmt -> 'a -> unit) *
  (?p:string -> fmt -> 'b -> unit) *
  (?p:string -> fmt -> 'c -> unit) ->
  (fmt -> 'a * 'b * 'c -> unit)
val print_triplet_lb:
  (?p:string -> fmt -> 'a -> unit) *
  (?p:string -> fmt -> 'b -> unit) *
  (?p:string -> fmt -> 'c -> unit) ->
  (?p:string -> fmt -> 'a * 'b * 'c -> unit)

val print_quadruplet:
  (?p:string -> fmt -> 'a -> unit) *
  (?p:string -> fmt -> 'b -> unit) *
  (?p:string -> fmt -> 'c -> unit) *
  (?p:string -> fmt -> 'd -> unit) ->
  (fmt -> 'a * 'b * 'c * 'd -> unit)
val print_quadruplet_lb:
  (?p:string -> fmt -> 'a -> unit) *
  (?p:string -> fmt -> 'b -> unit) *
  (?p:string -> fmt -> 'c -> unit) *
  (?p:string -> fmt -> 'd -> unit) ->
  (?p:string -> fmt -> 'a * 'b * 'c * 'd -> unit)

val print_quintuplet:
  (?p:string -> fmt -> 'a -> unit) *
  (?p:string -> fmt -> 'b -> unit) *
  (?p:string -> fmt -> 'c -> unit) *
  (?p:string -> fmt -> 'd -> unit) *
  (?p:string -> fmt -> 'e -> unit) ->
  (fmt -> 'a * 'b * 'c * 'd * 'e-> unit)
val print_quintuplet_lb:
  (?p:string -> fmt -> 'a -> unit) *
  (?p:string -> fmt -> 'b -> unit) *
  (?p:string -> fmt -> 'c -> unit) *
  (?p:string -> fmt -> 'd -> unit) *
  (?p:string -> fmt -> 'e -> unit) ->
  (?p:string -> fmt -> 'a * 'b * 'c * 'd * 'e -> unit)

val print_set:
  (module Set.S with type elt = 'a and type t = 't) ->
  (?p:string -> fmt -> 'a -> unit) ->
  (fmt -> 't -> unit)

val print_set_lb: ?slb:bool ->
  (module Set.S with type elt = 'a and type t = 't) ->
  (?p:string -> fmt -> 'a -> unit) ->
  (?p:string -> fmt -> 't -> unit)


module type MS = sig
  type key
  type +'a t
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val is_empty : 'a t -> bool
end

module MapPrinter:
  functor (M: MS) ->
  sig
    val pr :
      (?p:string -> fmt -> M.key -> unit) ->
      (?p:string -> fmt -> 'a -> unit) ->
      (fmt -> 'a M.t -> unit)
    val pr_lb :
      ?ind:bool ->
      (?p:string -> fmt -> M.key -> unit) ->
      (?p:string -> fmt -> 'a -> unit) ->
      (?p:string -> fmt -> 'a M.t -> unit)
  end

module type HS = sig
  type key
  type 'a t
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val length : 'a t -> int
end

module HTPrinter:
  functor (M: HS) ->
  sig
    val pr :
      (?p:string -> fmt -> M.key -> unit) ->
      (?p:string -> fmt -> 'a -> unit) ->
      (fmt -> 'a M.t -> unit)
    val pr_lb :
      ?ind:bool ->
      (?p:string -> fmt -> M.key -> unit) ->
      (?p:string -> fmt -> 'a -> unit) ->
      (?p:string -> fmt -> 'a M.t -> unit)
  end
