open AltErgoLib
open Ast 

module Cr = Crowbar 
module Sy = Symbols 

let query_max_depth = 3
let axiom_max_depth = 3
let func_max_depth = 3

let nb_usym_vars = 3
let nb_usym_funcs = 3
let nb_funs = 3
let nb_q_vars = 3

let v_id, thmid, axid, gid = ref 0, ref 0, ref 0, ref 0

let mk_usym_var pref ty vk num = 
  let vname = pref ^ string_of_int num in 
    Var {vname; ty; vk; id = (incr v_id; !v_id)}

let get_usymf num rty args = 
  let fname = 
    ( match rty with 
      | Tint -> "iuf_"
      | Treal -> "ruf_"
      | Tbool -> "buf_") 
    ^ string_of_int num 
  in
    FunCall {fname; rty; args} 

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
          | Tint -> mk_usym_var "iusv" 
          | Treal -> mk_usym_var "rusv" 
          | Tbool -> mk_usym_var "busv") ty US pos)

let usymf_genl ty gen fuel = 
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
        | true -> 
          ( match ty with
            | Tint -> mk_usym_var "iuqv"
            | Treal -> mk_usym_var "ruqv" 
            | Tbool -> mk_usym_var "buqv") ty UQ pos
        | false -> 
          ( match ty with
            | Tint -> mk_usym_var "ieqv"
            | Treal -> mk_usym_var "reqv" 
            | Tbool -> mk_usym_var "beqv") ty EQ pos)

let get_arg_gens ty args =
  List.map 
    ( fun (vname, vty) -> 
        Cr.const @@
        Var {vname; ty=vty; vk=ARG; id = (incr v_id; !v_id)})
    ( List.filter 
        (fun ( _, t) -> ty = t) 
        args)

let get_fcall_gens gen fdefs rty fuel = 
  List.map 
    ( fun f -> 
        ( match List.length f.aty with 
          | 1 -> 
            Cr.map
              [gen Tint (fuel-1)]
              ( fun x1 -> 
                FunCall {fname = f.name; rty; args = [x1]})
          | 2 -> 
            Cr.map
              [ gen Tint (fuel-1); 
                gen Treal (fuel-1)]
              ( fun x1 x2 -> 
                FunCall {fname = f.name; rty; args = [x1; x2]})
          | 3 -> 
            Cr.map
              [ gen Tint (fuel-1); 
                gen Treal (fuel-1);
                gen Tbool (fuel-1)]
              ( fun x1 x2 x3 -> 
                FunCall {fname = f.name; rty; args = [x1; x2; x3]})
          | _ -> assert false))
  @@
  List.filter 
    ( fun f -> f.rty = rty) 
    fdefs

let ast_gen ?(args = []) ?(funcs = []) ty  =
  ignore args;
  let rec ag_aux (funcs: fdef list) ty fuel = 
    match fuel <= 0 with 
    | true -> 
      Cr.choose @@  
        [cst_gen ty; usymv_gen ty; qv_gen ty] 
        (*@ get_arg_gens ty args*)

    | false -> 
      Cr.choose @@
        cst_gen ty ::
        usymv_gen ty :: 
        usymf_genl ty (ag_aux funcs) fuel @
        
        (*get_arg_gens ty args @
        get_fcall_gens (ag_aux funcs) funcs ty fuel @*)

        ( match ty with 
          | Tint -> 
            List.map 
              (fun bop -> binop_gen ty fuel bop (ag_aux funcs))
              [ IAdd; ISub; IMul; IDiv; IMod; IPow]
          | Treal ->
            List.map 
              (fun bop -> binop_gen ty fuel bop (ag_aux funcs))
              [ RAdd; RSub; RMul; RDiv; RPow]
          | Tbool ->
            List.map 
              (fun bop -> binop_gen ty fuel bop (ag_aux funcs))
              [ And; Or; Xor; Imp; Iff] @   
            List.map 
              (fun bop -> binop_gen Tint fuel bop (ag_aux funcs))
              [ Lt; Le; Gt; Ge; Eq; Neq] @   
            List.map 
              (fun bop -> binop_gen Treal fuel bop (ag_aux funcs))
              [ Lt; Le; Gt; Ge; Eq; Neq])
  in 
    ag_aux funcs ty query_max_depth

let goal_gen () =
  Cr.map 
    [ast_gen Tbool]
    ( fun ast ->
        Goal {
          name = "goal_" ^ (incr gid; string_of_int !gid);
          body = quantify ast})

let axiom_gen () =
  Cr.map 
  [ast_gen Tbool]
  ( fun ast ->
      Axiom {
        name = "ax_" ^ (incr axid; string_of_int !axid);
        body = quantify ast})
  
let fdef_gen rty n = 
  let args = 
    match n with 
    | 0 -> [ "x1", Tint;]
    | 1 -> [ "x1", Tint; "x2", Treal;]
    | 2 -> [ "x1", Tint; "x2", Treal; "x3", Tbool;]
    | _ -> assert false
  in
    Cr.map 
    [ast_gen ~args rty]
    ( fun ast ->
        FuncDef {
          name = (
            match rty with 
            | Tint -> "ifun_" 
            | Treal -> "rfun_" 
            | Tbool -> "bfun_" ) ^ string_of_int n;
          body = quantify ast; 
          aty = args; rty})

module SAT = Fun_sat.Make(Theory.Main_Default)
module FE = Frontend.Make(SAT)

let reinit_env () = 
  SAT.reset_refs ();
  Expr.clear_hc ();
  Shostak.Combine.empty_cache ()
  
let proc cmdlist = 
  try
    let cmds = 
      List.map 
        cmd_to_commad
        cmdlist
    in

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
      reinit_env ();
      true
    with 
    | exp ->
      
      List.iter (Format.printf "\n####  %a\n" print_cmd) cmdlist;
      Printexc.print_backtrace stdout;

      let tmp = Stdlib.Marshal.to_string (exp, cmdlist) [] in
      let time = Unix.gettimeofday () in
      let file = 
        "test/fuzzing/crash_output/op_"^string_of_float time^".txt"
      in
      let oc = open_out file in 
      
      Format.printf "\nWriting to file : %s\n@." file;
      Printf.fprintf oc "%s" tmp;
      flush stdout;
      close_out oc; 
      reinit_env ();
      raise exp 

let () =
  (*Memtrace.trace_if_requested ();*)
  Options.set_disable_weaks true;
  Options.set_is_gui false;
  Cr.add_test ~name:"ae" [goal_gen ()] 
  @@ fun x -> Cr.check (proc [x]) 
  (*
  (*Out_of_memory bug reproduction in afl mode*)
  Cr.add_test ~name:"ae" [axiom_gen (); goal_gen ()] 
  @@ fun x y -> Cr.check (proc [x; y])
  *)