
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

module Pp = Pp_utils
module F = Format

let pp_literal pp_v ppf = function
  | LTerm e ->
    Format.fprintf ppf "%a" Expr.pp_bis e
  | LSem l ->
    Format.fprintf ppf "%a" (Xliteral.print_view pp_v) l

let pp_fact pp_v ppf (lit, ex, th) =

  let pp_l = pp_literal pp_v in
  let pp_ex = Explanation.pp_bis in
  let pp_th = Th_util.pp_lit_origin in

  let l_p = "literal = " in
  let ex_p = "explanation = " in
  let lo_p = "lit_origin = " in

  let pp_l = Pp.add_p pp_l ~p:l_p in
  let pp_ex = Pp.add_p pp_ex ~p:ex_p in
  let pp_lo = Pp.add_p pp_th ~p:lo_p in

  F.fprintf ppf "{";

  F.fprintf ppf "@,@[<hov 2>%a; @]" pp_l lit;
  F.fprintf ppf "@,@[<hov 2>%a; @]" pp_ex ex;
  F.fprintf ppf "@,@[<hov 2>%a@]" pp_lo th;

  F.fprintf ppf "}"

let pp_facts pp_v ppf {equas; diseqs; ineqs; touched} =

  let pp_f = pp_fact pp_v in
  let pp_i1 = F.pp_print_int in
  let module PMI = Pp_utils.MapPrinter(Util.MI) in

  let e_p = "equas = " in
  let d_p = "diseqs = " in
  let i_p = "ineqs = " in
  let t_p = "touched = " in

  let pp_e = Pp.pp_queue pp_f ~p:e_p in
  let pp_d = Pp.pp_queue pp_f ~p:d_p in
  let pp_i2 = Pp.pp_queue pp_f ~p:i_p in
  let pp_t = PMI.pp pp_i1 pp_v ~p:t_p in

  F.fprintf ppf "{";

  F.fprintf ppf "@,@[<hov 2>%a; @]" pp_e equas;
  F.fprintf ppf "@,@[<hov 2>%a; @]" pp_d diseqs;
  F.fprintf ppf "@,@[<hov 2>%a; @]" pp_i2 ineqs;
  F.fprintf ppf "@,@[<hov 2>%a@]" pp_t touched;

  F.fprintf ppf "}"

module type RELATION = sig
  type t

  val pp_vrb : Format.formatter -> t -> unit

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
