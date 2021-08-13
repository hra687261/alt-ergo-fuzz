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

[@@@ocaml.warning "-33"]
open Options

type constant =
  | ConstBitv of string
  | ConstInt of string
  | ConstReal of Num.num
  | ConstTrue
  | ConstFalse
  | ConstVoid

type pp_infix =
  | PPand | PPor | PPxor | PPimplies | PPiff
  | PPlt | PPle | PPgt | PPge | PPeq | PPneq
  | PPadd | PPsub | PPmul | PPdiv | PPmod
  | PPpow_int | PPpow_real

type pp_prefix =
  | PPneg | PPnot

type ppure_type =
  | PPTint
  | PPTbool
  | PPTreal
  | PPTunit
  | PPTbitv of int
  | PPTvarid of string * Loc.t
  | PPTexternal of ppure_type list * string * Loc.t

type pattern =
  { pat_loc : Loc.t; pat_desc : string * string list }

type lexpr =
  { pp_loc : Loc.t; pp_desc : pp_desc }

and pp_desc =
  | PPvar of string
  | PPapp of string * lexpr list
  | PPmapsTo of string * lexpr
  | PPinInterval of lexpr * bool * lexpr * lexpr * bool
  (* bool = true <-> interval is_open *)

  | PPdistinct of lexpr list
  | PPconst of constant
  | PPinfix of lexpr * pp_infix * lexpr
  | PPprefix of pp_prefix * lexpr
  | PPget of lexpr * lexpr
  | PPset of lexpr * lexpr * lexpr
  | PPdot of lexpr * string
  | PPrecord of (string * lexpr) list
  | PPwith of lexpr * (string * lexpr) list
  | PPextract of lexpr * lexpr * lexpr
  | PPconcat of lexpr * lexpr
  | PPif of lexpr * lexpr * lexpr
  | PPforall of
      (string * ppure_type) list * (lexpr list * bool) list * lexpr list * lexpr
  | PPexists of
      (string * ppure_type) list * (lexpr list * bool) list * lexpr list * lexpr
  | PPforall_named of
      (string * string * ppure_type) list * (lexpr list * bool) list *
      lexpr list * lexpr
  | PPexists_named of
      (string * string * ppure_type) list * (lexpr list * bool) list *
      lexpr list * lexpr
  | PPnamed of string * lexpr
  | PPlet of (string * lexpr) list * lexpr
  | PPcheck of lexpr
  | PPcut of lexpr
  | PPcast of lexpr * ppure_type
  | PPmatch of lexpr * (pattern * lexpr) list
  | PPisConstr of lexpr * string
  | PPproject of bool * lexpr * string

(* Declarations. *)

type plogic_type =
  | PPredicate of ppure_type list
  | PFunction of ppure_type list * ppure_type

type body_type_decl =
  | Record of string * (string * ppure_type) list  (* lbl : t *)
  | Enum of string list
  | Algebraic of (string * (string * ppure_type) list) list
  | Abstract

type type_decl = Loc.t * string list * string * body_type_decl

type decl =
  | Theory of Loc.t * string * string * decl list
  | Axiom of Loc.t * string * Util.axiom_kind * lexpr
  | Rewriting of Loc.t * string * lexpr list
  | Goal of Loc.t * string * lexpr
  | Logic of Loc.t * Symbols.name_kind * (string * string) list * plogic_type
  | Predicate_def of
      Loc.t * (string * string) *
      (Loc.t * string * ppure_type) list * lexpr
  | Function_def of
      Loc.t * (string * string) *
      (Loc.t * string * ppure_type) list * ppure_type * lexpr
  | TypeDecl of type_decl list
  | Push of Loc.t * int
  | Pop of Loc.t * int

type file = decl list

let f = Format.fprintf

let print_constant fmt cst =
  match cst with 
  | ConstBitv str ->
    f fmt "ConstBitv (%s)" str
  | ConstInt str ->
    f fmt "ConstInt (%s)" str
  | ConstReal n ->
    f fmt "ConstReal (%s)" (Num.string_of_num n)
  | ConstTrue ->
    f fmt "ConstTrue"
  | ConstFalse ->
    f fmt "ConstFalse"
  | ConstVoid ->
    f fmt "ConstVoid"

let print_ppi fmt ppi =
  match ppi with
  | PPand -> f fmt "PPand"
  | PPor -> f fmt "PPor"
  | PPxor -> f fmt "PPxor"
  | PPimplies -> f fmt "PPimplies"
  | PPiff -> f fmt "PPiff"
  | PPlt -> f fmt "PPlt"
  | PPle -> f fmt "PPle"
  | PPgt -> f fmt "PPgt"
  | PPge -> f fmt "PPge"
  | PPeq -> f fmt "PPeq"
  | PPneq -> f fmt "PPneq"
  | PPadd -> f fmt "PPadd"
  | PPsub -> f fmt "PPsub"
  | PPmul -> f fmt "PPmul"
  | PPdiv -> f fmt "PPdiv"
  | PPmod -> f fmt "PPmod"
  | PPpow_int -> f fmt "PPpow_int"
  | PPpow_real -> f fmt "PPpow_real"

let print_prefix fmt prf =
  match prf with
  | PPneg -> f fmt "PPneg"
  | PPnot -> f fmt "PPnot"

let rec print_ppt fmt ppt =
  match ppt with 
  | PPTint -> f fmt "PPTint"
  | PPTbool -> f fmt "PPTbool"
  | PPTreal -> f fmt "PPTreal"
  | PPTunit -> f fmt "PPTunit"
  | PPTbitv i -> f fmt "PPTbitv (%d)" i
  | PPTvarid (str, _) -> 
    f fmt "PPTvarid (%s)" str
  | PPTexternal (ptl, str, _) -> 
    f fmt "PPTexternal (%a, %s)" print_ppt_l ptl str

and print_ppt_l fmt ptl = 
  f fmt "[";
  begin match ptl with 
    | h :: t ->
      f fmt "%a" print_ppt h;
      List.iter (
        f fmt "; %a" print_ppt
      ) t
    | [] -> ()
  end;
  f fmt "]"

let print_plogic_type fmt pt = 
  match pt with 
  | PPredicate ptl ->
    f fmt "PPredicate %a"
      print_ppt_l ptl
  | PFunction (ptl, pt) ->
    f fmt "PFunction (%a, %a)"
      print_ppt_l ptl print_ppt pt

let print_sl fmt ptl = 
  f fmt "[";
  begin match ptl with 
    | h :: t ->
      f fmt "%s" h;
      List.iter (
        f fmt "; %s"
      ) t
    | [] -> ()
  end;
  f fmt "]"

let print_pattern fmt {pat_desc = str, sl; _} = 
  f fmt "{pat_desc = %s, %a}" 
    str print_sl sl

let rec print_le fmt {pp_desc; _} = 
  f fmt "{pp_desc = %a}" 
    print_pp_desc pp_desc

and print_lel fmt lel = 
  f fmt "[";
  begin match lel with 
    | h :: t ->
      f fmt "%a" print_le h;
      List.iter (
        f fmt "; %a" print_le
      ) t
    | [] -> ()
  end;
  f fmt "]"

and print_sll fmt sll = 
  f fmt "[";
  begin match sll with 
    | (s, le) :: t ->
      f fmt "(%s, %a)" s print_le le;
      List.iter (
        fun (s, le) ->
          f fmt "; (%s, %a)" s print_le le;
      ) t
    | [] -> ()
  end;
  f fmt "]"

and print_spl fmt spl = 
  f fmt "[";
  begin match spl with 
    | (s, ppt) :: t ->
      f fmt "(%s, %a)" s print_ppt ppt;
      List.iter (
        fun (s, ppt) ->
          f fmt "; (%s, %a)" s print_ppt ppt;
      ) t
    | [] -> ()
  end;
  f fmt "]"

and print_sspl fmt sspl = 
  f fmt "[";
  begin match sspl with 
    | (s1, s2, ppt) :: t ->
      f fmt "(%s, %s, %a)" 
        s1 s2 print_ppt ppt;
      List.iter (
        fun (s1, s2, ppt) ->
          f fmt "; (%s, %s, %a)" 
            s1 s2 print_ppt ppt;
      ) t
    | [] -> ()
  end;
  f fmt "]"

and print_lelbl fmt lelbl = 
  f fmt "[";
  begin match lelbl with 
    | (lel, b) :: t ->
      f fmt "(%a, %b)" 
        print_lel lel b;
      List.iter (
        fun (lel, b) ->
          f fmt "; (%a, %b)" 
            print_lel lel b;
      ) t
    | [] -> ()
  end;
  f fmt "]"

and print_pll fmt pll = 
  f fmt "[";
  begin match pll with 
    | (p, le) :: t ->
      f fmt "(%a, %a)" 
        print_pattern p print_le le;
      List.iter (
        fun (p, le) ->
          f fmt "; (%a, %a)" 
            print_pattern p print_le le
      ) t
    | [] -> ()
  end;
  f fmt "]"

and print_pp_desc fmt pd =
  ignore fmt; 
  match pd with 
  | PPvar str -> 
    f fmt "PPvar %s" str

  | PPapp (str, lel) -> 
    f fmt "PPapp (%s, %a)" str print_lel lel

  | PPmapsTo (str, le) -> 
    f fmt "PPmapsTo (%s, %a)" str print_le le

  | PPinInterval (le1, b1, le2, le3, b2) -> 
    f fmt "PPinInterval (%a, %b, %a, %a, %b)"
      print_le le1 b1 print_le le2 print_le le3 b2

  | PPdistinct lel -> 
    f fmt "PPdistinct %a" print_lel lel

  | PPconst cst -> 
    f fmt "PPconst %a" print_constant cst

  | PPinfix (le1, pp, le2) -> 
    f fmt "PPinfix (%a, %a, %a)"
      print_le le1 print_ppi pp print_le le2

  | PPprefix (pp, le) -> 
    f fmt "PPprefix (%a, %a)" print_prefix pp print_le le

  | PPget (le1, le2) -> 
    f fmt "PPget (%a, %a)" print_le le1 print_le le2

  | PPset (le1, le2, le3) -> 
    f fmt "PPset (%a, %a, %a)"
      print_le le1 print_le le2 print_le le3

  | PPdot (le, str) -> 
    f fmt "PPdot (%a, %s)"
      print_le le str

  | PPrecord sll -> 
    f fmt "PPrecord %a" print_sll sll

  | PPwith (le, sll) -> 
    f fmt "PPwith (%a, %a)" print_le le print_sll sll

  | PPextract (le1, le2, le3) -> 
    f fmt "PPextract (%a, %a, %a)"
      print_le le1 print_le le2 print_le le3

  | PPconcat (le1, le2) -> 
    f fmt "PPconcat (%a, %a)"
      print_le le1 print_le le2

  | PPif (le1, le2, le3) -> 
    f fmt "PPif (%a, %a, %a)"
      print_le le1 print_le le2 print_le le3

  | PPforall (spl, lelbl, lel, le) -> 
    f fmt "PPforall (%a, %a, %a, %a)"
      print_spl spl print_lelbl lelbl 
      print_lel lel print_le le

  | PPexists (spl, lelbl, lel, le) -> 
    f fmt "PPexists (%a, %a, %a, %a)"
      print_spl spl print_lelbl lelbl 
      print_lel lel print_le le

  | PPforall_named (sspl, lelbl, lel, le) -> 
    f fmt "PPforall_named (%a, %a, %a, %a)"
      print_sspl sspl print_lelbl lelbl 
      print_lel lel print_le le

  | PPexists_named (sspl, lelbl, lel, le) -> 
    f fmt "PPexists_named (%a, %a, %a, %a)"
      print_sspl sspl print_lelbl lelbl 
      print_lel lel print_le le

  | PPnamed (s, le) ->
    f fmt "PPnamed (%s, %a)" s print_le le

  | PPlet (sll, le) ->
    f fmt "PPlet (%a, %a)"
      print_sll sll print_le le

  | PPcheck le ->
    f fmt "PPcheck %a" print_le le

  | PPcut le ->
    f fmt "PPcut %a" print_le le

  | PPcast (le, p) ->
    f fmt "PPcast (%a, %a)"
      print_le le print_ppt p

  | PPmatch (le, pll) ->
    f fmt "PPmatch (%a, %a)"
      print_le le print_pll pll

  | PPisConstr (le, s) ->
    f fmt "PPisConstr (%a, %s)"
      print_le le s

  | PPproject (b, le, s) ->
    f fmt "PPproject (%b, %a, %s)"
      b print_le le s

let print_axiom_kind fmt ak = 
  match ak with 
  | Util.Default -> f fmt "Util.Default"
  | Util.Propagator -> f fmt "Util.Propagator"

let print_name_kind fmt nk = 
  match nk with 
  | Symbols.Ac -> f fmt "Symbols.Ac"
  | Symbols.Other -> f fmt "Symbols.Other"

let print_ssl fmt ssl = 
  f fmt "[";
  begin match ssl with 
    | (s1, s2) :: t ->
      f fmt "(%s, %s)" s1 s2;
      List.iter (
        fun (s1, s2) ->
          f fmt "; (%s, %s)" s1 s2
      ) t
    | [] -> ()
  end;
  f fmt "]"

let print_lspl fmt lspl =
  f fmt "[";
  begin match lspl with 
    | (_, s, ppt) :: t ->
      f fmt "(%s, %a)" s print_ppt ppt;
      List.iter (
        fun (_, s, ppt) ->
          f fmt "; (%s, %a)" s print_ppt ppt;
      ) t
    | [] -> ()
  end;
  f fmt "]"

let print_spll fmt spll =
  f fmt "[";
  begin match spll with 
    | (s, spl) :: t ->
      f fmt "(%s, %a)" s print_spl spl;
      List.iter (
        fun (s, spl) ->
          f fmt "(%s, %a)" s print_spl spl;
      ) t
    | [] -> ()
  end;
  f fmt "]"

let print_btd fmt btd = 
  match btd with 
  | Record (s, spl) ->
    f fmt "Record (%s, %a)" s print_spl spl

  | Enum sl ->
    f fmt "Enum %a" print_sl sl

  | Algebraic spll ->
    f fmt "Algebraic %a" print_spll spll

  | Abstract ->
    f fmt "Abstract"

let rec print_decl fmt d =
  match d with 
  | Theory (_, str1, str2, dl) -> 
    f fmt "Theory (%s, %s, %a)" 
      str1 str2 print_decl_list dl

  | Axiom (_, str1, ak, le) -> 
    f fmt "Axiom (%s, %a, %a)" 
      str1 print_axiom_kind ak print_le le

  | Rewriting (_, str1, lel) -> 
    f fmt "Rewriting (%s, %a)" 
      str1 print_lel lel

  | Goal (_, str1, le) -> 
    f fmt "Goal (%s, %a)" 
      str1 print_le le

  | Logic (_, nk, ssl, plt) ->
    f fmt "Logic (%a, %a, %a)" 
      print_name_kind nk print_ssl ssl 
      print_plogic_type plt

  | Predicate_def (_, (s1, s2), lspl, le) ->
    f fmt "Predicate_def ((%s, %s), %a, %a)" 
      s1 s2 print_lspl lspl 
      print_le le

  | Function_def (_, (s1, s2), lspl, pt, le) ->
    f fmt "Function_def ((%s, %s), %a, %a, %a)" 
      s1 s2 print_lspl lspl print_ppt pt 
      print_le le

  | TypeDecl tdl -> 
    f fmt "TypeDecl %a" (
      fun fmt l -> 
        f fmt "[";
        begin match l with 
          | (_, sl, s, btd) :: t ->
            f fmt "(%a, %s, %a)" 
              print_sl sl s print_btd btd;
            List.iter (
              fun (_, sl, s, btd) -> 
                f fmt "; (%a, %s, %a)" 
                  print_sl sl s print_btd btd 
            ) t
          | [] -> ()
        end;
        f fmt "]" 
    ) tdl

  | Push (_, i) -> 
    f fmt "Push %d" i

  | Pop (_, i) -> 
    f fmt "Pop %d" i

and print_decl_list fmt dl =
  f fmt "[";
  begin match dl with 
    | h :: t ->
      f fmt "%a" print_decl h;
      List.iter (
        f fmt "; %a" print_decl
      ) t
    | [] -> ()
  end;
  f fmt "]"
