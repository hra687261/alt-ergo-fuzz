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

let print_lit_origin fmt lo =
  let f = Format.fprintf in
  let aux fmt th =
    match th with
    | Th_arith ->
      f fmt "Th_arith"
    | Th_sum ->
      f fmt "Th_sum"
    | Th_adt ->
      f fmt "Th_adt"
    | Th_arrays ->
      f fmt "Th_arrays"
    | Th_UF ->
      f fmt "Th_UF"
  in
  match lo with
  | Subst ->
    f fmt "Subst"
  | CS (th, q) ->
    f fmt "CS (%a, %a)"
      aux th Numbers.Q.print q
  | NCS (th, q) ->
    f fmt "NCS (%a, %a)"
      aux th Numbers.Q.print q
  | Other ->
    f fmt "Other"
