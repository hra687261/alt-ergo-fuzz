open AltErgoLib
open Ast 

module Cr = Crowbar 
module Sy = Symbols 


(* TODO: 
    Test generation of funcdefs, 
    Test calling of funcdefs in goals*)

let fdefs : fdef list ref = ref []

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

let get_fcall_gens gen (fdefs: fdef list) rtyp fuel = 
  List.map 
    ( fun f -> 
        ( match List.length f.atyp with 
          | 1 -> 
            Cr.map
              [gen Tint (fuel-1)]
              ( fun x1 -> 
                FunCall {fname = f.name; rtyp; args = [x1]})
          | 2 -> 
            Cr.map
              [ gen Tint (fuel-1); 
                gen Treal (fuel-1)]
              ( fun x1 x2 -> 
                FunCall {fname = f.name; rtyp; args = [x1; x2]})
          | 3 -> 
            Cr.map
              [ gen Tint (fuel-1); 
                gen Treal (fuel-1);
                gen Tbool (fuel-1)]
              ( fun x1 x2 x3 -> 
                FunCall {fname = f.name; rtyp; args = [x1; x2; x3]})
          | _ -> assert false))
  @@
  List.filter 
    (fun f -> f.rtyp = rtyp) 
    fdefs

let ast_gen ?(qvars = true) ?(args = []) ?(funcs = []) max_depth ty =
  let rec ag_aux funcs ty fuel = 
    match fuel <= 0 with 
    | true -> 
      Cr.choose @@  
        [cst_gen ty; usymv_gen ty]
        @ (if qvars then [qv_gen ty] else []) 
        @ get_arg_gens ty args

    | false -> 
      Cr.choose @@
        cst_gen ty ::
        usymv_gen ty :: 
        usymf_genl ty (ag_aux funcs) fuel @
        (if qvars && fuel < max_depth 
          then [qv_gen ty] 
          else []) @

        get_arg_gens ty args @
        get_fcall_gens (ag_aux funcs) funcs ty fuel @

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
    ag_aux funcs ty max_depth

let goal_gen ?(funcs = []) () =
  Cr.with_printer print_cmd (
    Cr.map 
      [ast_gen ~funcs query_max_depth Tbool]
      ( fun ast ->
          Goal {
            name = "goal_" ^ (incr gid; string_of_int !gid);
            body = quantify ast}))

let axiom_gen ?(funcs = []) () =
  Cr.with_printer print_cmd (
    Cr.map 
      [ast_gen ~funcs axiom_max_depth Tbool]
      ( fun ast ->
          Axiom {
            name = "ax_" ^ (incr axid; string_of_int !axid);
            body = quantify ast}))

let funcdef_gen ?(funcs = []) () =
  let aux name vty = 
    mk_tvar name vty ARG
  in 
  let aux2 rtyp num =
    match rtyp with 
    | Tint -> List.nth i_udfs num
    | Treal -> List.nth r_udfs num
    | Tbool -> List.nth b_udfs num
  in   
  let genl rtyp =
    [ (let atyp = [aux "ia" Tint] in 
        Cr.map 
          [ast_gen ~qvars:false ~args:atyp ~funcs func_max_depth rtyp]
          (fun body ->
            let name, _ = aux2 rtyp 0 in 
            let fd = {name; body; atyp; rtyp} in 
              fdefs := fd :: !fdefs;
              FuncDef fd));
      
      (let atyp = [aux "ia" Tint; aux "ra" Treal] in
        Cr.map 
          [ast_gen ~qvars:false ~args:atyp ~funcs func_max_depth rtyp]
          (fun body ->
            let name, _ = aux2 rtyp 1 in 
            let fd = {name; body = quantify body; atyp; rtyp} in 
              fdefs := fd :: !fdefs;
              FuncDef fd));

      (let atyp = [aux "ia" Tint; aux "ra" Treal; aux "ba" Tbool] in 
        Cr.map 
          [ast_gen ~qvars:false ~args:atyp ~funcs func_max_depth rtyp]
          (fun body ->
            let name, _ = aux2 rtyp 2 in 
            let fd = {name; body; atyp; rtyp} in 
              fdefs := fd :: !fdefs;
              FuncDef fd))]
  in 
  let typs = [Tint; Treal; Tbool] in 
  Cr.with_printer print_cmd (
    Cr.choose 
      ( List.fold_left 
          (fun acc t ->
            genl t @ acc)
          [] typs))


module SAT = Fun_sat.Make(Theory.Main_Default)
module FE = Frontend.Make(SAT)

let reinit_env () = 
  SAT.reset_refs ();
  Expr.clear_hc ();
  Shostak.Combine.empty_cache ()
  
let cmdll = ref []

let proc cmds = 
  cmdll := !cmdll @ [cmds];
  try
    let commands = 
      List.map 
        cmd_to_commad
        cmds
    in

    let _, consistent, _ = 
      List.fold_left 
        (fun acc d ->
          FE.process_decl 
            (fun _ _ -> ()) (*FE.print_status*)
            (FE.init_all_used_context ()) 
            (Stack.create ()) 
            acc d)
        (SAT.empty (), true, Explanation.empty) 
        commands
    in
      Format.printf "%s@." (
        if consistent 
        then "unknown" 
        else "unsat");
      reinit_env ();
      true
  with
  | _ ->
    let exp_str = Printexc.get_backtrace () in 
    Format.printf "\nException: %s\n@." exp_str;
    
    Format.printf "cmds :@.";
    List.iter (Format.printf "### %a\n@." print_cmd) cmds;
    
    
    let commands = 
      List.map 
        cmd_to_commad
        cmds
    in
    Format.printf "ae commands :@.";
    List.iter (
      fun x ->
        Format.printf ">>> %a@." Commands.print x) 
      commands;


    let tmp = Stdlib.Marshal.to_string (!cmdll, exp_str, cmds) [] in
    let time = Unix.gettimeofday () in
    let file = 
      "test/fuzzing/crash_output/op_"^string_of_float time^".txt"
    in
    let oc = open_out file in
    
    Format.printf "Writing to file : %s@." file;
    Printf.fprintf oc "%s" tmp;
    close_out oc;
    reinit_env ();
    false

let () =
  Options.set_disable_weaks true;
  Options.set_is_gui false; 

  Cr.add_test ~name:"ae" 
    [ axiom_gen ();
      goal_gen ()] 
    @@ 
    fun x y -> Cr.check (proc [x; y])
