(******************************************************************************)
(*                                                                            *)
(*     Alt-Ergo: The SMT Solver For Software Verification                     *)
(*     Copyright (C) 2013-2018 --- OCamlPro SAS                               *)
(*                                                                            *)
(*     This file is distributed under the terms of the license indicated      *)
(*     in the file 'License.OCamlPro'. If 'License.OCamlPro' is not           *)
(*     present, please contact us to clarify licensing.                       *)
(*                                                                            *)
(******************************************************************************)

exception Timeout
exception Unsolvable

exception Cmp of int

module MI = Map.Make(struct type t = int
    let compare (x: int) y = Stdlib.compare x y end)

module SI = Set.Make(struct type t = int
    let compare (x: int) y = Stdlib.compare x y end)

module SS = Set.Make(String)


(** Different values for -case-split-policy option:
    -after-theory-assume (default value): after assuming facts in
    theory by the SAT
    -before-matching: just before performing a matching round
    -after-matching: just after performing a matching round **)
type case_split_policy =
  | AfterTheoryAssume (* default *)
  | BeforeMatching
  | AfterMatching


type inst_kind = Normal | Forward | Backward

type sat_solver =
  | Tableaux
  | Tableaux_CDCL
  | CDCL
  | CDCL_Tableaux

type theories_extensions =
  | Sum
  | Adt
  | Arrays
  | Records
  | Bitv
  | LIA
  | LRA
  | NRA
  | NIA
  | FPA

type axiom_kind = Default | Propagator

let th_ext_of_string ext =
  match ext with
  | "Sum" -> Some Sum
  | "Adt" -> Some Adt
  | "Arrays" -> Some Arrays
  | "Records" -> Some Records
  | "Bitv" -> Some Bitv
  | "LIA" -> Some LIA
  | "LRA" -> Some LRA
  | "NRA" -> Some NRA
  | "NIA" -> Some NIA
  | "FPA" -> Some FPA
  |  _ -> None

let string_of_th_ext ext =
  match ext with
  | Sum -> "Sum"
  | Adt -> "Adt"
  | Arrays -> "Arrays"
  | Records -> "Records"
  | Bitv -> "Bitv"
  | LIA -> "LIA"
  | LRA -> "LRA"
  | NRA -> "NRA"
  | NIA -> "NIA"
  | FPA -> "FPA"

let string_of_axiom_kind = function
  | Default -> "Default"
  | Propagator -> "Propagator"

let [@inline always] compare_algebraic s1 s2 f_same_constrs_with_args =
  let r1 = Obj.repr s1 in
  let r2 = Obj.repr s2 in
  match Obj.is_int r1, Obj.is_int r2 with
  | true, true -> Stdlib.compare s1 s2 (* both constructors without args *)
  | true, false -> -1
  | false, true -> 1
  | false, false ->
    let cmp_tags = Obj.tag r1 - Obj.tag r2 in
    if cmp_tags <> 0 then cmp_tags else f_same_constrs_with_args (s1, s2)

let [@inline always] cmp_lists l1 l2 cmp_elts =
  try
    List.iter2
      (fun a b ->
         let c = cmp_elts a b in
         if c <> 0 then raise (Cmp c)
      )l1 l2;
    0
  with
  | Cmp n -> n
  | Invalid_argument _ -> List.length l1 - List.length l2

type matching_env =
  {
    nb_triggers : int;
    triggers_var : bool;
    no_ematching: bool;
    greedy : bool;
    use_cs : bool;
    backward : inst_kind
  }

module Pp = Pp_utils
module F = Format

let pp_inst_kind ppf = function
  | Normal -> F.fprintf ppf "Normal"
  | Forward -> F.fprintf ppf "Forward"
  | Backward -> F.fprintf ppf "Backward"

let pp_menv ppf {
    nb_triggers; triggers_var; no_ematching;
    greedy; use_cs; backward
  } =

  let pp_b1 = F.pp_print_bool in
  let pp_i = F.pp_print_int in
  let pp_ik = pp_inst_kind in

  let nt_p = "nb_triggers = " in
  let tv_p = "triggers_var = " in
  let ne_p = "no_ematching = " in

  let g_p = "greedy = " in
  let uc_p = "use_cs = " in
  let b2_p = "backward = " in


  let pp_nt = Pp.add_p pp_i ~p:nt_p in
  let pp_tv = Pp.add_p pp_b1 ~p:tv_p in
  let pp_ne = Pp.add_p pp_b1 ~p:ne_p in

  let pp_g = Pp.add_p pp_b1 ~p:g_p in
  let pp_uc = Pp.add_p pp_b1 ~p:uc_p in
  let pp_b2 = Pp.add_p pp_ik ~p:b2_p in


  F.fprintf ppf "{";

  F.fprintf ppf "@,@[<hov 2>%a; @]" pp_nt nb_triggers;
  F.fprintf ppf "@,@[<hov 2>%a; @]" pp_tv triggers_var;
  F.fprintf ppf "@,@[<hov 2>%a; @]" pp_ne no_ematching;

  F.fprintf ppf "@,@[<hov 2>%a; @]" pp_g greedy;
  F.fprintf ppf "@,@[<hov 2>%a; @]" pp_uc use_cs;
  F.fprintf ppf "@,@[<hov 2>%a@]" pp_b2 backward;

  F.fprintf ppf "}"

let loop
    ~(f : int -> 'a -> 'b -> 'b)
    ~(max : int)
    ~(elt : 'a)
    ~(init : 'b) : 'b
  =
  let rec loop_aux cpt acc =
    if cpt >= max then acc
    else
      loop_aux (cpt+1) (f cpt elt acc)
  in
  loop_aux 0 init

let print_list ~sep ~pp fmt l =
  match l with
    [] -> ()
  | e :: l ->
    Format.fprintf fmt "%a" pp e;
    List.iter (fun e -> Format.fprintf fmt "%s %a" sep pp e) l


let rec print_list_pp ~sep ~pp fmt = function
  | [] -> ()
  | [x] -> pp fmt x
  | x :: l ->
    Format.fprintf fmt "%a %a" pp x sep ();
    print_list_pp ~sep ~pp fmt l

