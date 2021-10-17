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

let f = Format.fprintf

let print_e = Pp.addpref Expr.print_bis

let print_info :
  ?p:string -> Format.formatter -> info -> unit =
  fun ?(p = "") fmt {age; lem_orig; t_orig; but} ->
  (
    let p1 = p^"  " in
    let p2 = p1^"  " in

    let print_el = Pp.print_list_lb print_e in

    f fmt "%s{" p;

    f fmt "\n%sage=" p1;
    f fmt " %d;" age;

    f fmt "\n%slem_orig=" p1;
    f fmt " %a;"
      (print_el ~p:p2) lem_orig;

    f fmt "\n%st_orig=" p1;
    f fmt " %a;"
      (print_el ~p:p2) t_orig;

    f fmt "\n%sbut=" p1;
    f fmt " %b;" but;

    f fmt "\n%s}" p
  )

let print_trigger_info :
  ?p:string -> Format.formatter -> trigger_info -> unit =
  fun ?(p = "") fmt { trigger; trigger_age; trigger_orig; trigger_formula;
                      trigger_dep; trigger_increm_guard} ->
    let p1 = p^"  " in
    let p2 = p1^"  " in

    f fmt "%s{" p;

    f fmt "\n%strigger=" p1;
    f fmt " %a" (Expr.print_trg ~p:p2) trigger;

    f fmt "\n%strigger_age=" p1;
    f fmt "%d" trigger_age;

    f fmt "\n%strigger_orig=" p1;
    f fmt " %a"
      (Expr.print_vrb ~firstcall:true ~p:p2) trigger_orig;

    f fmt "\n%strigger_formula=" p1;
    f fmt " %a"
      (Expr.print_vrb ~firstcall:true ~p:p2) trigger_formula;

    f fmt "\n%strigger_dep=" p1;
    f fmt "\n%s%a"
      p2 Explanation.print trigger_dep;

    f fmt "\n%strigger_increm_guard=" p1;
    f fmt " %a"
      (Expr.print_vrb ~firstcall:true ~p:p2) trigger_increm_guard;

    f fmt "\n%s}" p

let print_gsubst :
  ?p:string -> Format.formatter -> gsubst -> unit =
  fun ?(p = "") fmt  {sbs; sty; gen; goal; s_term_orig; s_lem_orig} ->
  (
    let p1 = p^"  " in
    let p2 = p1^"  " in

    let pr_ty = Pp.addpref Ty.print in
    let pr_sy = Pp.addpref Symbols.print in

    let module TyMP = Pp.MapPrinter(Ty.M) in
    let module SyMP = Pp.MapPrinter(Symbols.Map) in

    f fmt "%s{" p;

    f fmt "\n%ssbs=" p1;
    f fmt " %a"
      (SyMP.pr_lb pr_sy print_e ~p:p2) sbs;

    f fmt "\n%ssty=" p1;
    f fmt " %a"
      (TyMP.pr_lb Pp.pr_int pr_ty ~p:p2) sty;

    f fmt "\n%sgen=" p1;
    f fmt "%d" gen;

    f fmt "\n%sgoal=" p1;
    f fmt "%b" goal;

    f fmt "\n%ss_term_orig=" p1;
    f fmt " %a"
      (Pp.print_list_lb ~p:p2 (Expr.print_vrb ~firstcall:true)) s_term_orig;

    f fmt "\n%ss_lem_orig=" p1;
    f fmt " %a"
      (Expr.print_vrb ~firstcall:true ~p:p2) s_lem_orig;

    f fmt "\n%s}" p
  )
