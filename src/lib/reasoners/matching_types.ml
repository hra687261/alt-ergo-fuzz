(******************************************************************************)
(*                                                                            *)
(*     The Alt-Ergo theorem prover                                            *)
(*     Copyright (C) 2006-2013                                                *)
(*                                                                            *)
(*     Sylvain Conchon                                                        *)
(*     Evelyne Contejean                                                      *)
(*                                                                            *)
(*     Francois Bobot                                                         *)
(*     Mohamed Iguernelala                                                    *)
(*     Stephane Lescuyer                                                      *)
(*     Alain Mebsout                                                          *)
(*                                                                            *)
(*     CNRS - INRIA - Universite Paris Sud                                    *)
(*                                                                            *)
(*     This file is distributed under the terms of the Apache Software        *)
(*     License version 2.0                                                    *)
(*                                                                            *)
(*  ------------------------------------------------------------------------  *)
(*                                                                            *)
(*     Alt-Ergo: The SMT Solver For Software Verification                     *)
(*     Copyright (C) 2013-2018 --- OCamlPro SAS                               *)
(*                                                                            *)
(*     This file is distributed under the terms of the Apache Software        *)
(*     License version 2.0                                                    *)
(*                                                                            *)
(******************************************************************************)

type gsubst = {
  sbs : Expr.t Symbols.Map.t;
  sty : Ty.subst;
  gen : int ;     (* l'age d'une substitution est l'age du plus vieux
                     		     terme qu'elle contient *)
  goal : bool;    (* vrai si la substitution contient un terme ayant un lien
                     		     avec le but de la PO *)
  s_term_orig : Expr.t list;
  s_lem_orig : Expr.t;
}

type trigger_info = {
  trigger : Expr.trigger;
  trigger_age : int ;  (* age d'un trigger *)
  trigger_orig : Expr.t ; (* lemme d'origine *)
  trigger_formula : Expr.t ; (* formule associee au trigger *)
  trigger_dep : Explanation.t ;
  trigger_increm_guard : Expr.t
  (* guard associated to push in incremental mode *)
}

type term_info = {
  term_age : int ;        (* age du terme *)
  term_from_goal : bool ;   (* vrai si le terme provient du but de la PO *)
  term_from_formula : Expr.t option; (* lemme d'origine du terme *)
  term_from_terms : Expr.t list;
}

type info = {
  age : int ; (* age du terme *)
  lem_orig : Expr.t list ; (* lemme d'ou provient eventuellement le terme *)
  t_orig : Expr.t list;
  but : bool  (* le terme a-t-il un lien avec le but final de la PO *)
}

module Pp = Pp_utils
module F = Format
module E = Expr
module Ex = Explanation

let pp_info ppf {age; lem_orig; t_orig; but} =

  let pp_e = E.pp_bis in
  let pp_i = F.pp_print_int in
  let pp_b = F.pp_print_bool in

  let a_p = "age = " in
  let lo_p = "lem_orig = " in
  let to_p = "t_orig = " in
  let b_p = "but = " in

  let pp_a = Pp.add_p pp_i ~p:a_p in
  let pp_lo = Pp.pp_list pp_e ~p:lo_p in
  let pp_to = Pp.pp_list pp_e ~p:to_p in
  let pp_b = Pp.add_p pp_b ~p:b_p in

  F.fprintf ppf "{";

  F.fprintf ppf "@[%a; @,@]" pp_a age;
  F.fprintf ppf "@[%a; @,@]" pp_lo lem_orig;
  F.fprintf ppf "@[%a; @,@]" pp_to t_orig;
  F.fprintf ppf "@[%a@,@]" pp_b but;

  F.fprintf ppf "}"

let pp_trigger_info ppf {
    trigger; trigger_age; trigger_orig;
    trigger_formula; trigger_dep; trigger_increm_guard
  } =

  let pp_e = E.pp_bis in
  let pp_t = E.pp_trg in
  let pp_i = F.pp_print_int in
  let pp_ex = Ex.pp_bis in


  let t_p = "trigger = " in
  let ta_p = "trigger_age = " in
  let to_p = "trigger_orig = " in

  let tf_p = "trigger_formula = " in
  let td_p = "trigger_dep = " in
  let tig_p = "trigger_increm_guard = " in


  let pp_t = Pp.add_p pp_t ~p:t_p in
  let pp_ta = Pp.add_p pp_i ~p:ta_p in
  let pp_to = Pp.add_p pp_e ~p:to_p in

  let pp_tf = Pp.add_p pp_e ~p:tf_p in
  let pp_td = Pp.add_p pp_ex ~p:td_p in
  let pp_tig = Pp.add_p pp_e  ~p:tig_p in


  F.fprintf ppf "{";

  F.fprintf ppf "@,@[<hov 2>%a; @]" pp_t trigger;
  F.fprintf ppf "@,@[<hov 2>%a; @]" pp_ta trigger_age;
  F.fprintf ppf "@,@[<hov 2>%a; @]" pp_to trigger_orig;

  F.fprintf ppf "@,@[<hov 2>%a; @]" pp_tf trigger_formula;
  F.fprintf ppf "@,@[<hov 2>%a; @]" pp_td trigger_dep;
  F.fprintf ppf "@,@[<hov 2>%a@]" pp_tig trigger_increm_guard;

  F.fprintf ppf "}"

let pp_gsubst ppf {
    sbs; sty; gen;
    goal; s_term_orig; s_lem_orig
  } =

  let pp_e = E.pp_bis in
  let pp_i = F.pp_print_int in
  let pp_b = F.pp_print_bool in
  let pp_ty = Ty.print in
  let pp_sy = Symbols.print in

  let module TyMP = Pp.MapPrinter(Ty.M) in
  let module SyMP = Pp.MapPrinter(Symbols.Map) in

  let sb_p = "sbs = " in
  let st_p = "sty = " in
  let ge_p = "gen = " in

  let go_p = "goal = " in
  let sto_p = "s_term_orig = " in
  let slo_p = "s_lem_orig = " in


  let pp_sb = SyMP.pp pp_sy pp_e ~p:sb_p in
  let pp_st = TyMP.pp pp_i pp_ty ~p:st_p in
  let pp_ge = Pp.add_p pp_i ~p:ge_p in

  let pp_go = Pp.add_p pp_b ~p:go_p in
  let pp_sto = Pp.pp_list pp_e ~p:sto_p in
  let pp_slo = Pp.add_p pp_e  ~p:slo_p in


  F.fprintf ppf "{";

  F.fprintf ppf "@,@[<hov 2>%a; @]" pp_sb sbs;
  F.fprintf ppf "@,@[<hov 2>%a; @]" pp_st sty;
  F.fprintf ppf "@,@[<hov 2>%a; @]" pp_ge gen;

  F.fprintf ppf "@,@[<hov 2>%a; @]" pp_go goal;
  F.fprintf ppf "@,@[<hov 2>%a; @]" pp_sto s_term_orig;
  F.fprintf ppf "@,@[<hov 2>%a@]" pp_slo s_lem_orig;

  F.fprintf ppf "}"



