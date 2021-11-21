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

open Format

module E = Expr

type rootdep = { name : string; f : Expr.t; loc : Loc.t}

type exp =
  | Literal of Satml_types.Atom.atom
  | Fresh of int
  | Bj of E.t
  | Dep of E.t
  | RootDep of rootdep

module S =
  Set.Make
    (struct
      type t = exp
      let compare a b = match a,b with
        | Fresh i1, Fresh i2 -> i1 - i2
        | Literal a  , Literal b   -> Satml_types.Atom.cmp_atom a b
        | Dep e1  , Dep e2   -> E.compare e1 e2
        | RootDep r1, RootDep r2 -> E.compare r1.f r2.f
        | Bj e1   , Bj e2    -> E.compare e1 e2

        | Literal _, _ -> -1
        | _, Literal _ -> 1

        | Fresh _, _ -> -1
        | _, Fresh _ -> 1

        | Dep _, _ -> 1
        | _, Dep _ -> -1
        | RootDep _, _ -> 1
        | _, RootDep _ -> -1

    end)

let is_empty t = S.is_empty t

type t = S.t

exception Inconsistent of t * Expr.Set.t list

let empty = S.empty

let union s1 s2 =
  if s1 == s2 then s1 else S.union s1 s2

let singleton e = S.singleton e

let mem e s = S.mem e s

let remove e s =
  if S.mem e s then S.remove e s
  else raise Not_found

let iter_atoms f s = S.iter f s

let fold_atoms f s acc = S.fold f s acc

(* TODO : XXX : We have to choose the smallest ??? *)
let merge s1 _s2 = s1

let fresh_exp =
  let r = ref (-1) in
  fun () ->
    incr r;
    Fresh !r

let exists_fresh t =
  S.exists (function
      | Fresh _ -> true
      | _ -> false
    ) t

let remove_fresh fe s =
  if S.mem fe s then Some (S.remove fe s)
  else None

let add_fresh fe s = S.add fe s

let print fmt ex =
  if Options.get_debug_explanations () then begin
    fprintf fmt "{";
    S.iter (function
        | Literal a -> fprintf fmt "{Literal:%a}, " Satml_types.Atom.pr_atom a
        | Fresh i -> Format.fprintf fmt "{Fresh:%i}" i;
        | Dep f -> Format.fprintf fmt "{Dep:%a}" E.print f
        | RootDep r -> Format.fprintf fmt "{RootDep:%s}" r.name
        | Bj f -> Format.fprintf fmt "{BJ:%a}" E.print f
      ) ex;
    fprintf fmt "}"
  end

let get_unsat_core dep =
  fold_atoms
    (fun a acc ->
       match a with
       | RootDep r -> r :: acc
       | Dep _ -> acc
       | Bj _ | Fresh _ | Literal _ -> assert false
    ) dep []

let print_unsat_core ?(tab=false) fmt dep =
  iter_atoms
    (function
      | RootDep r ->
        if tab then Format.fprintf fmt "  %s@." r.name (* tab is too big *)
        else Format.fprintf fmt "%s@." r.name
      | Dep _ -> ()
      | Bj _ | Fresh _ | Literal _ -> assert false
    ) dep

let formulas_of s =
  S.fold (fun e acc ->
      match e with
      | Dep f | Bj f -> E.Set.add f acc
      | RootDep _ | Fresh _ -> acc
      | Literal _ -> assert false (*TODO*)
    ) s E.Set.empty

let bj_formulas_of s =
  S.fold (fun e acc ->
      match e with
      | Bj f -> E.Set.add f acc
      | Dep _ | RootDep _ | Fresh _ -> acc
      | Literal _ -> assert false (*TODO*)
    ) s E.Set.empty

let rec literals_of_acc lit fs f acc = match E.form_view f with
  | E.Not_a_form -> assert false
  | E.Literal _ ->
    if lit then f :: acc else acc
  | E.Iff(f1, f2) ->
    let g = E.elim_iff f1 f2 (E.id f) ~with_conj:true in
    literals_of_acc lit fs g acc
  | E.Xor(f1, f2) ->
    let g = E.neg @@ E.elim_iff f1 f2 (E.id f) ~with_conj:false in
    literals_of_acc lit fs g acc
  | E.Unit (f1,f2) ->
    let acc = literals_of_acc false fs f1 acc in
    literals_of_acc false fs f2 acc
  | E.Clause (f1, f2, _) ->
    let acc = literals_of_acc true fs f1 acc in
    literals_of_acc true fs f2 acc
  | E.Lemma _ ->
    acc
  | E.Skolem { E.main = f; _ } ->
    literals_of_acc true fs f acc
  | E.Let { E.in_e; let_e; _ } ->
    literals_of_acc true fs in_e @@ literals_of_acc true fs let_e acc

let literals_of ex =
  let fs  = formulas_of ex in
  E.Set.fold (literals_of_acc true fs) fs []

module MI = Util.MI

let literals_ids_of ex =
  List.fold_left (fun acc f ->
      let i = E.id f in
      let m = try MI.find i acc with Not_found -> 0 in
      MI.add i (m + 1) acc
    ) MI.empty (literals_of ex)


let make_deps sf =
  E.Set.fold (fun l acc -> S.add (Bj l) acc) sf S.empty

let has_no_bj s =
  try S.iter (function Bj _ -> raise Exit | _ -> ())s; true
  with Exit -> false

let compare = S.compare

let subset = S.subset

module Pp = Pp_utils
module F = Format

let pp_bis ppf =
  let pp_aux ppf = function
    | Literal a ->
      F.fprintf ppf "{Literal:%a}" Satml_types.Atom.pr_atom a
    | Fresh i ->
      F.fprintf ppf "{Fresh:%i}" i;
    | Dep d ->
      F.fprintf ppf "{Dep:%a}" E.pp_bis d
    | RootDep r ->
      F.fprintf ppf "{RootDep:%s}" r.name
    | Bj d ->
      F.fprintf ppf "{BJ:%a}" E.pp_bis d
  in
  let pp = Pp.pp_set (module S) pp_aux in
  F.fprintf ppf "@[<hov 2>{%a}@]" pp

let pp_root_dep ppf {name; f; loc} =

  let pp_s = F.pp_print_string in
  let pp_e = E.pp_bis in
  let pp_l = Loc.report in

  let n_p = "name = " in
  let f_p = "f = " in
  let l_p = "loc = " in

  let pp_n = Pp.add_p pp_s ~p:n_p in
  let pp_f = Pp.add_p pp_e ~p:f_p in
  let pp_l = Pp.add_p pp_l ~p:l_p in

  F.fprintf ppf "{";

  F.fprintf ppf "@ @[<hov 2>%a;@]" pp_n name;
  F.fprintf ppf "@ @[<hov 2>%a;@]" pp_f f;
  F.fprintf ppf "@ @[<hov 2>%a@]" pp_l loc;

  F.fprintf ppf "}"

let pp_exp_vrb ppf = function
  | Literal atom ->
    F.fprintf ppf "@[<hov 2>Literal (%a)@]"
      Satml_types.Atom.pp_atom_vrb atom
  | Fresh n ->
    F.fprintf ppf "@[<hov 2>Fresh (%a)@]"
      F.pp_print_int n
  | Bj e ->
    F.fprintf ppf "@[<hov 2>Bj (%a)@]"
      E.pp_bis e
  | Dep e ->
    F.fprintf ppf "@[<hov 2>Dep (%a)@]"
      E.pp_bis e
  | RootDep rd ->
    F.fprintf ppf "@[<hov 2>RootDep (%a)@]"
      pp_root_dep rd
