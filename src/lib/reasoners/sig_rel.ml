
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

let print_fact : (Format.formatter -> 'a -> unit) ->
  ?p:string -> Format.formatter -> 'a fact -> unit =
  fun pr ?(p = "  ") fmt (lit, ex, lit_o) ->
  let p1 = p^"  " in
  let p2 = p1^"  " in

  Format.fprintf fmt "%s{@." p;
  Format.fprintf fmt "%sliteral = \n%a;@."
    p1 (print_literal pr ~p:p2) lit;
  Format.fprintf fmt "%sexplanation = %a;@."
    p1 Explanation.print ex;
  Format.fprintf fmt "%slit_origin = %a;@."
    p1 Th_util.print_lit_origin lit_o;
  Format.fprintf fmt "%s}" p

let print_facts : (Format.formatter -> 'a -> unit) ->
  ?p:string -> Format.formatter -> 'a facts -> unit =
  fun pr ?(p = "  ") fmt {equas; diseqs; ineqs; touched} ->
  let p1 = p^"  " in

  Format.fprintf fmt "%s{@." p;
  Format.fprintf fmt "%sequas = %a@." p1
    (Pp_utils.print_queue (print_fact pr)) equas;
  Format.fprintf fmt "%sdiseqs = %a@." p1
    (Pp_utils.print_queue (print_fact pr)) diseqs;
  Format.fprintf fmt "%sineqs = %a@." p1
    (Pp_utils.print_queue (print_fact pr)) ineqs;
  let module PMI = Pp_utils.MapPrinter(Util.MI) in
  Format.fprintf fmt "%stouched = %a@." p1
    (PMI.pr Pp_utils.pr_int (Pp_utils.addpref pr)) touched;
  Format.fprintf fmt "%s}" p


module type RELATION = sig
  type t

  val pr_vrb : ?p:string -> Format.formatter -> t -> unit

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

end
