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

type answer = (Explanation.t * Expr.Set.t list) option

type theory =
  | Th_arith
  | Th_sum
  | Th_adt
  | Th_arrays
  | Th_UF

type lit_origin =
  | Subst
  | CS of theory * Numbers.Q.t
  | NCS of theory * Numbers.Q.t
  | Other

let pp_lit_origin ppf lo =
  let aux = function
    | Th_arith -> "Th_arith"
    | Th_sum -> "Th_sum"
    | Th_adt -> "Th_adt"
    | Th_arrays -> "Th_arrays"
    | Th_UF -> "Th_UF"
  in
  match lo with
  | Subst ->
    Format.fprintf ppf "Subst"
  | CS (th, q) ->
    Format.fprintf ppf "CS (%s, %a)"
      (aux th) Numbers.Q.print q
  | NCS (th, q) ->
    Format.fprintf ppf "NCS (%s, %a)"
      (aux th) Numbers.Q.print q
  | Other ->
    Format.fprintf ppf "Other"
