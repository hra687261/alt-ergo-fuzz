
type 'a literal = LTerm of Expr.t | LSem of 'a Xliteral.view

type instances = (Expr.t list * Expr.gformula * Explanation.t) list

type 'a input =
  'a Xliteral.view * Expr.t option * Explanation.t * Th_util.lit_origin

type 'a fact = 'a literal * Explanation.t * Th_util.lit_origin

type 'a facts = {
  equas     : 'a fact Queue.t;
  diseqs  : 'a fact Queue.t;
  ineqs   : 'a fact Queue.t;
  mutable touched : 'a Util.MI.t;
}

type 'a result = {
  assume : 'a fact list;
  remove: Expr.t list;
}

let print_literal : (Format.formatter -> 'a -> unit) ->
  ?p:string -> Format.formatter -> 'a literal -> unit = 
  fun pr ?(p = "  ") fmt lit ->
  match lit with 
  | LTerm e -> 
    Format.fprintf fmt "%s%a" p Expr.print_bis e 
  | LSem l -> 
    Format.fprintf fmt "%a"
      (Xliteral.print_view ~lbl:p pr) l

module type RELATION = sig
  type t

  val empty : Expr.Set.t list -> t

  val assume : t ->
    Uf.t -> (Shostak.Combine.r input) list -> t * Shostak.Combine.r result
  val query  : t -> Uf.t -> Shostak.Combine.r input -> Th_util.answer

  val case_split :
    t -> Uf.t ->
    for_model:bool ->
    (Shostak.Combine.r Xliteral.view * bool * Th_util.lit_origin) list
  (** case_split env returns a list of equalities *)

  val add : t -> Uf.t -> Shostak.Combine.r -> Expr.t ->
    t * (Shostak.Combine.r Xliteral.view * Explanation.t) list
  (** add a representant to take into account *)

  val instantiate :
    do_syntactic_matching:bool ->
    Matching_types.info Expr.Map.t * Expr.t list Expr.Map.t Symbols.Map.t ->
    t -> Uf.t -> (Expr.t -> Expr.t -> bool) ->
    t * instances

  val print_model :
    Format.formatter -> t -> (Expr.t * Shostak.Combine.r) list -> unit

  val new_terms : t -> Expr.Set.t

  val assume_th_elt : t -> Expr.th_elt -> Explanation.t -> t

  val pr_vrb : ?p:string -> Format.formatter -> t -> unit

end
