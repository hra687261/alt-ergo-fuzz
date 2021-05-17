open AltErgoLib
open Ast 

module Cr = Crowbar 
module Sy = Symbols 

let fdefs : (int * typ) list ref = ref []

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

let usymv_gen ty = 
  Cr.map 
    [Cr.range nb_usym_vars] 
    ( fun pos -> 
        ( match ty with
          | Tint -> mk_var (mk_vname "iusv" pos)
          | Treal -> mk_var (mk_vname "rusv" pos)
          | Tbool -> mk_var (mk_vname "busv" pos)) ty US)

let usymf_genl ty gen fuel = 
  [ Cr.map 
      [ gen Tint (fuel-1)] 
      ( fun arg1 -> 
          get_ufunc_ast 0 ty [arg1]);
    Cr.map 
      [ gen Tint (fuel-1); 
        gen Treal (fuel-1)] 
      ( fun arg1 arg2 -> 
          get_ufunc_ast 1 ty [arg1; arg2]);
    Cr.map 
      [ gen Tint (fuel-1); 
        gen Treal (fuel-1); 
        cst_gen Tbool
        (* gen Tbool (fuel-1) *)]
      ( fun arg1 arg2 arg3 -> 
          get_ufunc_ast 2 ty [arg1; arg2; arg3]);]

let qv_gen ty = 
  Cr.map 
    [Cr.bool; Cr.range nb_q_vars] 
    ( fun b pos -> 
        match b with 
        | true -> 
          ( match ty with
            | Tint -> mk_var (mk_vname "iuqv" pos)
            | Treal -> mk_var (mk_vname "ruqv" pos)
            | Tbool -> mk_var (mk_vname "buqv" pos)) ty UQ
        | false -> 
          ( match ty with
            | Tint -> mk_var (mk_vname "ieqv" pos)
            | Treal -> mk_var (mk_vname "reqv" pos)
            | Tbool -> mk_var (mk_vname "beqv" pos)) ty EQ)

let get_arg_gens ty args =
  List.map 
    ( fun x -> Cr.const @@ Var x)
    ( List.filter 
        (fun x -> ty = x.vty) 
        args)

let get_fcall_gens gen rtyp fuel = 
  let fl = 
    (List.filter (fun (_,rt) -> rt = rtyp) !fdefs)
  in
  List.map ( 
    fun (num, rtyp) -> ( 
        let fname, _ = 
          match rtyp with 
          | Tint -> List.nth i_udfs num
          | Treal -> List.nth r_udfs num
          | Tbool -> List.nth b_udfs num
        in 
        match num with 
        | 0 -> 
          Cr.map
            [gen Tint (fuel-1)]
            ( fun x1 -> 
                FunCall {fname; rtyp; args = [x1]})
        | 1 -> 
          Cr.map
            [ gen Tint (fuel-1); 
              gen Treal (fuel-1)]
            ( fun x1 x2 -> 
                FunCall {fname; rtyp; args = [x1; x2]})
        | 2 -> 
          Cr.map
            [ gen Tint (fuel-1); 
              gen Treal (fuel-1);
              cst_gen Tbool
              (* gen Tbool (fuel-1) *)]
            ( fun x1 x2 x3 -> 
                FunCall {fname; rtyp; args = [x1; x2; x3]})
        | _ -> assert false))
    fl

let ast_gen ?(qvars = true) ?(args = []) max_depth ty =
  let rec ag_aux ty fuel = 
    match fuel <= 0 with 
    | true -> 
      Cr.choose ( 
        [cst_gen ty; usymv_gen ty]
        @ (if qvars then [qv_gen ty] else []) 
        @ get_arg_gens ty args)

    | false -> 
      Cr.choose (
        cst_gen ty ::
        usymv_gen ty :: 
        usymf_genl ty ag_aux fuel @
        ( if qvars && fuel < max_depth 
          then [qv_gen ty] 
          else []) @

        get_arg_gens ty args @
        get_fcall_gens ag_aux ty fuel @

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
              [ Lt; Le; Gt; Ge; Eq; Neq]))
  in 
  ag_aux ty max_depth

let goal_gen () =
  Cr.with_printer print_cmd (
    Cr.map 
      [ast_gen query_max_depth Tbool]
      ( fun ast ->
          Goal {
            name = "goal_" ^ (incr gid; string_of_int !gid);
            body = quantify ast}))

let axiom_gen () =
  Cr.with_printer print_cmd (
    Cr.map 
      [ast_gen axiom_max_depth Tbool]
      ( fun ast ->
          Axiom {
            name = "ax_" ^ (incr axid; string_of_int !axid);
            body = quantify ast}))

(* num determines the types of the arguments
 * 0 : int
 * 1 : int -> real
 * 2 : int -> real -> bool
 **)
let funcdef_gen (num, rtyp) () =
  let aux_vmk name vty = 
    mk_tvar name vty ARG
  in 
  let name, _ = 
    match rtyp with 
    | Tint -> List.nth i_udfs num
    | Treal -> List.nth r_udfs num
    | Tbool -> List.nth b_udfs num
  in
  let atyp = 
    match num with 
    | 0 -> [aux_vmk "ia" Tint]
    | 1 -> [aux_vmk "ia" Tint; aux_vmk "ra" Treal]
    | 2 -> [aux_vmk "ia" Tint; aux_vmk "ra" Treal; aux_vmk "ba" Tbool]
    | _ -> assert false 
  in
  let qvars = false in 
  Cr.with_printer print_cmd (
    Cr.map [ast_gen ~qvars ~args:atyp func_max_depth rtyp] ( 
      fun body -> FuncDef {name; body = quantify body; atyp; rtyp}))

module SAT = Fun_sat.Make(Theory.Main_Default)
module FE = Frontend.Make(SAT)

let reinit_env () = 
  SAT.reset_refs ();
  Expr.clear_hc ();
  Shostak.Combine.empty_cache ();
  Gc.major ()

let solve cmds =
  let _, consistent, _ = 
    List.fold_left 
      ( fun acc cmd ->
          let command = cmd_to_commad cmd in 
          FE.process_decl 
            (fun _ _ -> ()) (*FE.print_status*)
            (FE.init_all_used_context ()) 
            (Stack.create ()) 
            acc command)
      (SAT.empty (), true, Explanation.empty) 
      cmds
  in 
  Format.printf "%s@."
    ( if consistent 
      then "unknown" 
      else "unsat")

let crash_cpt = ref 0 

let proc cmds = 
  try
    solve cmds;
    reinit_env ();
    true
  with
  | exp ->
    (*let exp_raw_bt = Printexc.get_raw_backtrace () in*)
    let exp_str = Printexc.to_string exp in 
    let exp_bt_str = Printexc.get_backtrace () in 

    let tmp = Stdlib.Marshal.to_string (exp_str, exp_bt_str, cmds) [] in
    let time = Unix.gettimeofday () in
    let file_name = 
      "test/fuzzing/crash_output/op_"^string_of_float time^".txt" 
    in

    let oc = open_out file_name in
    output_string oc tmp;
    close_out oc;

    Format.printf "\nException: %s\n%s@." exp_str exp_bt_str;
    Format.printf "\nCaused by: \n%a@." 
      ( fun fmt cmdl ->
          List.iter ( 
            fun cmd ->
              Format.fprintf fmt "\n### %a@." print_cmd cmd;
              let command = cmd_to_commad cmd in 
              Format.fprintf fmt ">>> %a@." Commands.print command
          ) cmdl
      ) cmds;
    Format.printf "Marshalled and written to the file : %s@." file_name;

    reinit_env ();
    (*Printexc.raise_with_backtrace exp exp_raw_bt*)
    false

let () =


  Options.set_disable_weaks true;
  Options.set_is_gui false; 
  (*Memtrace.trace_if_requested ();*)
  let fdi = (2, Tint) in
  let fdr = (2, Treal) in
  let fdb = (2, Tbool) in
  fdefs := fdi :: fdr :: fdb :: [];  
  begin 
    Cr.add_test ~name:"ae" 
      [ funcdef_gen fdi ();
        funcdef_gen fdr ();
        funcdef_gen fdb ();
        axiom_gen ();
        goal_gen ()] 
    @@ 
    fun fi fr fb x y -> Cr.check (proc [fi; fr; fb; x; y])
  end

  (*
  begin 
    Cr.add_test ~name:"ae" 
      [ axiom_gen ();
        goal_gen ()] 
    @@ 
    fun x y -> Cr.check (proc [x; y])
  end
  *)