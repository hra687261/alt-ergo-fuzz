open AltErgoLib
open Ast 

module Cr = Crowbar 
module Sy = Symbols 

let thmid = ref 0

let i_usymv, r_usymv, b_usymv = mk_vars "iuv" Tint US, 
  mk_vars "ruv" Treal US, mk_vars "buv" Tbool US

let i_uqv, r_uqv, b_uqv = mk_vars "iuqv" Tint UQ, 
  mk_vars "ruqv" Treal UQ, mk_vars "buqv" Tbool UQ

let i_eqv, r_eqv, b_eqv = mk_vars "ieqv" Tint EQ, 
  mk_vars "reqv" Treal EQ, mk_vars "beqv" Tbool EQ
  
let get_usymf num rty args = 
  let name = 
    ( match rty with 
      | Tint -> "iuf_"
      | Treal -> "ruf_"
      | Tbool -> "buf_") 
    ^ string_of_int num 
  in
    Fun {name; rty; args} 

let cst_gen ty = 
  match ty with 
  | Tint -> 
    Cr.map 
      [Cr.int] 
      (fun x -> Cst (CstI x))
  | Treal -> 
    Cr.map 
      [Cr.float] 
      (fun x -> 
        Cst (CstR (
          if Float.is_nan x then 0. else x)))
  | Tbool -> 
    Cr.map 
      [Cr.bool] 
      (fun x -> Cst (CstB x))
  
let binop_gen ty fuel bop gen = 
  Cr.map 
    [ gen ty (fuel - 1);
      gen ty (fuel - 1)] 
    ( fun x y -> 
        mk_binop bop x y)  

let unop_gen ty fuel uop gen = 
  Cr.map [gen ty (fuel - 1)] (fun x -> mk_unop uop x)  

let usymv_gen ty = 
  Cr.map 
    [Cr.range nb_usym_vars] 
    ( fun pos -> 
        List.nth (
        match ty with
        | Tint -> i_usymv
        | Treal -> r_usymv
        | Tbool -> b_usymv) pos)

let usymf_gen ty gen fuel = 
  [ Cr.map 
      [ gen Tint (fuel-1)] 
      ( fun arg1 -> 
          get_usymf 1 ty [arg1]);
    Cr.map 
      [ gen Tint (fuel-1); 
        gen Treal (fuel-1)] 
      ( fun arg1 arg2 -> 
          get_usymf 2 ty [arg1; arg2]);
    Cr.map 
      [ gen Tint (fuel-1); 
        gen Treal (fuel-1); 
        cst_gen Tbool
        (* gen Tbool (fuel-1) *)]
      ( fun arg1 arg2 arg3 -> 
          get_usymf 3 ty [arg1; arg2; arg3]);]

let qv_gen ty = 
  Cr.map 
    [Cr.bool; Cr.range nb_q_vars] 
    ( fun b pos -> 
        match b with 
        | true -> (
          List.nth (
            match ty with
            | Tint -> i_uqv
            | Treal -> r_uqv
            | Tbool -> b_uqv) pos)
        | false ->
          List.nth (
            match ty with
            | Tint -> i_eqv
            | Treal -> r_eqv
            | Tbool -> b_eqv) pos)

let ast_gen =
  let rec ag_aux ty fuel = 
    match fuel <= 0 with 
    | true -> 
      Cr.choose @@  
        [cst_gen ty; usymv_gen ty; qv_gen ty]
    | false -> 
      Cr.choose @@
        (* 
        cst_gen ty ::
        usymv_gen ty :: 
        qv_gen ty ::*)
        usymf_gen ty ag_aux fuel @
        ( match ty with 
          | Tint -> 
            List.map 
              (fun bop -> binop_gen ty fuel bop ag_aux)
              [ IAdd; ISub; IMul; IDiv; IMod; IPow]
          | Treal ->
            List.map 
              (fun bop -> binop_gen ty fuel bop ag_aux)
              [ RAdd; RSub; RMul; RDiv; RPow]
          | Tbool ->
            List.map 
              (fun bop -> binop_gen ty fuel bop ag_aux)
              [ And; Or; Xor; Imp; Iff] @   
            List.map 
              (fun bop -> binop_gen Tint fuel bop ag_aux)
              [ Lt; Le; Gt; Ge; Eq; Neq] @   
            List.map 
              (fun bop -> binop_gen Treal fuel bop ag_aux)
              [ Lt; Le; Gt; Ge; Eq; Neq])
  in 
    Cr.map 
      [ag_aux Tbool max_fuel]
      quantify

module SAT = Fun_sat.Make(Theory.Main_Default)
module FE = Frontend.Make(SAT)

let mk_cmd_query name expr gsty = 
  Commands.{ 
    st_loc = Loc.dummy;
    st_decl = 
      Commands.Query (name, expr, gsty)}

let proc astlist = 
  let cmds = 
    List.map 
      ( fun x -> 
          let expr = ast_to_expr (quantify x) in 
          let name = "thm" ^ string_of_int (incr thmid; !thmid) in 
          let gsty = Typed.Thm in 
            Commands.{ 
              st_loc = Loc.dummy;
              st_decl = 
                Commands.Query (name, expr, gsty)})
      astlist
  in
  (*
  List.iter (Format.printf "\n####  %a\n" print) astlist;
  List.iter (Format.printf "\n>>>>  %a\n" Commands.print) cmds;
  *)
  try         
    let _, consistent, ex = 
      List.fold_left 
        ( fun acc d ->
            FE.process_decl 
              ( fun _ _ -> ()) (*FE.print_status*)
              (FE.init_all_used_context ()) 
              (Stack.create ()) 
              acc d)
        (SAT.empty (), true, Explanation.empty) 
        cmds
    in
      ignore ex; 
      ignore consistent;
      true
    with 
    | exp ->

      List.iter (Format.printf "\n####  %a\n" print) astlist;
      List.iter (Format.printf "\n>>>>  %a\n" Commands.print) cmds;
  

      let tmp = Stdlib.Marshal.to_string astlist [] in
      let time = Unix.gettimeofday () in
      let file = 
        "test/fuzzing/crash_output/op_"^string_of_float time^".txt"
      in
      Format.printf "\nWriting to file : %s\n@." file;
      let oc = open_out file in 
      Printf.fprintf oc "%s" tmp;
      flush stdout;
      close_out oc; 
      raise exp 

let () = Options.set_is_gui false

let test_id = ref 0 

let () =
  Cr.add_test ~name:"ae" [ast_gen] 
  @@ fun m -> 
  Cr.check (proc [m])