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

open Typed

(* Sat entry *)

type sat_decl_aux =
  | Assume of string * Expr.t * bool
  | PredDef of Expr.t * string (*name of the predicate*)
  | RwtDef of (Expr.t rwt_rule) list
  | Query of string *  Expr.t * goal_sort
  | ThAssume of Expr.th_elt
  | Push of int
  | Pop of int

type sat_tdecl = {
  st_loc : Loc.t;
  st_decl : sat_decl_aux
}

let print_aux fmt = function
  | Assume (name, e, b) ->
    Format.fprintf fmt "assume %s(%b): @[<hov>%a@]" name b Expr.print e
  | PredDef (e, name) ->
    Format.fprintf fmt "pred-def %s: @[<hov>%a@]" name Expr.print e
  | RwtDef l ->
    Format.fprintf fmt "rwrts: @[<v>%a@]"
      (Util.print_list_pp
         ~sep:Format.pp_print_space
         ~pp:(print_rwt Expr.print)
      ) l
  | Query (name, e, sort) ->
    Format.fprintf fmt "query %s(%a): @[<hov>%a@]"
      name print_goal_sort sort Expr.print e
  | ThAssume t ->
    Format.fprintf fmt "th assume %a" Expr.print_th_elt t
  | Push n -> Format.fprintf fmt "Push %d" n
  | Pop n ->  Format.fprintf fmt "Pop %d" n

let print fmt decl = print_aux fmt decl.st_decl

let f = Format.fprintf

let print_aux_bis fmt = function
  | Assume (name, e, b) ->
    f fmt "assume %s(%b): \n%a" 
      name b
      (Expr.print_vrb ~p:"") e
  | PredDef (e, name) ->
    f fmt "pred-def %s: \n%a"
      name
      (Expr.print_vrb ~p:"") e
  | RwtDef l ->
    f fmt "rwrts: \n%a"
      (Util.print_list_pp
         ~sep:Format.pp_print_space
         ~pp:(print_rwt Expr.print)
      ) l
  | Query (name, e, sort) ->
    f fmt "query %s(%a): \n%a"
      name print_goal_sort
      sort
      (Expr.print_vrb ~p:"") e
  | ThAssume t ->
    f fmt
      "th assume %a" Expr.print_th_elt t
  | Push n ->
    f fmt "Push %d" n
  | Pop n ->
    f fmt "Pop %d" n

let print_verbose fmt decl = print_aux_bis fmt decl.st_decl