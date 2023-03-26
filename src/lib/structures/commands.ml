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

(* Sat entry *)

type sat_decl_aux =
  | Assume of string * Expr.t * bool
  | PredDef of Expr.t * string (*name of the predicate*)
  | RwtDef of (Expr.t Typed.rwt_rule) list
  | Query of string *  Expr.t * Ty.goal_sort
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
         ~pp:(Typed.print_rwt Expr.print)
      ) l
  | Query (name, e, sort) ->
    Format.fprintf fmt "query %s(%a): @[<hov>%a@]"
      name Ty.print_goal_sort sort Expr.print e
  | ThAssume t ->
    Format.fprintf fmt "th assume %a" Expr.print_th_elt t
  | Push n -> Format.fprintf fmt "Push %d" n
  | Pop n ->  Format.fprintf fmt "Pop %d" n

let print fmt decl = print_aux fmt decl.st_decl

module Pp = Pp_utils
module F = Format

let pp_vrb ppf decl =
  let pp_vrb_aux ppf = function
    | Assume (name, e, b) ->
      F.fprintf ppf "Assume %s(%b): @[<hov 2>%a@]"
        name b Expr.pp_vrb e
    | PredDef (e, name) ->
      F.fprintf ppf "PredDef %s: @[<hov 2>%a@]"
        name Expr.pp_vrb e
    | RwtDef l ->
      F.fprintf ppf "RwtDef: @[<hov 2>%a@]"
        (Util.print_list_pp
           ~sep:Format.pp_print_space
           ~pp:(Typed.print_rwt Expr.print)
        ) l
    | Query (name, e, sort) ->
      F.fprintf ppf "Query %s(%a): @[<hov 2>%a@]"
        name Ty.print_goal_sort sort Expr.pp_vrb e
    | ThAssume t ->
      F.fprintf ppf "ThAssume @[<hov 2>%a@]"
        Expr.print_th_elt t
    | Push n ->
      F.fprintf ppf "Push %d" n
    | Pop n ->
      F.fprintf ppf "Pop %d" n
  in
  pp_vrb_aux ppf decl.st_decl
