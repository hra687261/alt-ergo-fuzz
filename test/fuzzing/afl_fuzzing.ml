open AltErgoLib
open Ast 

module Cr = Crowbar 
module Sy = Symbols 

module FCS = Set.Make(String)

type gares = {gast : ast; args : VS.t; funcalls: FCS.t}

type gcres = {gcmd : cmd; args : VS.t; funcalls: FCS.t}

let fdefs : (int * typ) list ref = ref []

let cst_gen ty = 
  match ty with 
  | Tint -> 
    Cr.map 
      [Cr.int] 
      (fun x -> 
         let gast = Cst (CstI x) in 
         {gast; args = VS.empty; funcalls = FCS.empty})
  | Treal -> 
    Cr.map 
      [Cr.float] 
      (fun x -> 
         let gast = 
           Cst (CstR (
               if Float.is_nan x then 0. else x)) in 
         {gast; args = VS.empty; funcalls = FCS.empty})
  | Tbool -> 
    Cr.map 
      [Cr.bool] 
      (fun x -> 
         let gast =
           Cst (CstB x) in 
         {gast; args = VS.empty; funcalls = FCS.empty})

let binop_gen ty fuel bop gen = 
  Cr.map 
    [ gen ty (fuel - 1);
      gen ty (fuel - 1)] 
    ( fun x y -> 
        let gast = 
          mk_binop bop x.gast y.gast in 
        { gast; 
          args = VS.union x.args y.args; 
          funcalls = FCS.union x.funcalls y.funcalls})   

let usymv_gen ty = 
  Cr.map 
    [Cr.range nb_us_vars] 
    (fun pos -> 
       let gast =
         get_uvar_ast pos ty 
       in 
       {gast; args = VS.empty; funcalls = FCS.empty})


let usymf_genl ty gen fuel = 
  [ Cr.map 
      [ gen Tint (fuel-1)] 
      ( fun arg1 -> 
          let gast = 
            get_ufunc_ast 0 ty [arg1.gast]
          in 
          { gast = gast; 
            args = arg1.args; 
            funcalls = arg1.funcalls});
    Cr.map 
      [ gen Tint (fuel-1); 
        gen Treal (fuel-1)] 
      ( fun arg1 arg2 -> 
          let gast =
            get_ufunc_ast 1 ty [arg1.gast; arg2.gast]
          in 
          { gast; 
            args = 
              VS.union arg1.args arg2.args; 
            funcalls = 
              FCS.union arg1.funcalls arg2.funcalls});
    Cr.map 
      [ gen Tint (fuel-1); 
        gen Treal (fuel-1); 
        cst_gen Tbool
        (* gen Tbool (fuel-1) *)]
      ( fun arg1 arg2 arg3 -> 
          let gast =
            get_ufunc_ast 2 ty [arg1.gast; arg2.gast; arg3.gast]
          in 
          { gast; 
            args = 
              VS.union 
                (VS.union arg1.args arg2.args) 
                arg3.args; 
            funcalls = 
              FCS.union 
                (FCS.union arg1.funcalls arg2.funcalls) 
                arg3.funcalls});]
let qv_gen ty = 
  Cr.map 
    [Cr.bool; Cr.range nb_q_vars] 
    ( fun b pos -> 
        let gast =
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
              | Tbool -> mk_var (mk_vname "beqv" pos)) ty EQ
        in  
        {gast; args = VS.empty; funcalls = FCS.empty})

let get_arg_gens ty args =
  List.map 
    ( fun x -> 
        Cr.const (
          { gast = Var x; 
            args = VS.add x VS.empty; 
            funcalls = FCS.empty}))
    ( List.filter 
        (fun x -> ty = x.vty) 
        args)

let get_fcall_gens gen rtyp fuel = 
  let fl = 
    (List.filter (fun (_,rt) -> rt = rtyp) !fdefs)
  in
  List.map ( 
    fun (num, rtyp) -> ( 
        match num with 
        | 0 -> 
          Cr.map
            [gen Tint (fuel-1)]
            ( fun x1 -> 
                let fc =
                  get_udfunc_ast num rtyp [x1.gast] 
                in 
                let gast = FunCall fc in 
                { gast; 
                  args = x1.args; 
                  funcalls = 
                    FCS.add 
                      fc.fname
                      x1.funcalls})
        | 1 -> 
          Cr.map
            [ gen Tint (fuel-1); 
              gen Treal (fuel-1)]
            ( fun x1 x2 -> 
                let fc =
                  get_udfunc_ast num rtyp [x1.gast; x2.gast]
                in 
                let gast = FunCall fc in 
                { gast; 
                  args = VS.union x1.args x2.args; 
                  funcalls = 
                    FCS.add 
                      fc.fname
                      (FCS.union x1.funcalls x2.funcalls)})
        | 2 -> 
          Cr.map
            [ gen Tint (fuel-1); 
              gen Treal (fuel-1);
              cst_gen Tbool
              (* gen Tbool (fuel-1) *)]
            ( fun x1 x2 x3 -> 
                let fc =
                  get_udfunc_ast num rtyp 
                    [x1.gast; x2.gast; x3.gast] 
                in 
                let gast = FunCall fc in 
                { gast; 
                  args = 
                    VS.union 
                      (VS.union x1.args x2.args) 
                      x3.args; 
                  funcalls = 
                    FCS.add 
                      fc.fname 
                      ( FCS.union 
                          (FCS.union x1.funcalls x2.funcalls) 
                          x3.funcalls)})
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
  Cr.map 
    [ast_gen query_max_depth Tbool]
    ( fun x ->
        let gcmd =
          Goal {
            name = "goal_" ^ (incr gid; string_of_int !gid);
            body = quantify x.gast}
        in 
        {gcmd; args = x.args; funcalls = x.funcalls}
    )

let axiom_gen () =
  Cr.map 
    [ast_gen axiom_max_depth Tbool]
    ( fun x ->
        let gcmd =
          Axiom {
            name = "ax_" ^ (incr axid; string_of_int !axid);
            body = quantify x.gast}
        in 
        {gcmd; args = x.args; funcalls = x.funcalls}
    )

(* num determines the types of the arguments
 * 0 : int
 * 1 : int -> real
 * 2 : int -> real -> bool
 **)
let funcdef_gen (num, rtyp) () =
  let aux_vmk name vty = 
    mk_tvar name vty ARG
  in 
  let name = get_udfunc_name num rtyp in
  let atyp = 
    match num with 
    | 0 -> [aux_vmk "ia" Tint]
    | 1 -> [aux_vmk "ia" Tint; aux_vmk "ra" Treal]
    | 2 -> [aux_vmk "ia" Tint; aux_vmk "ra" Treal; aux_vmk "ba" Tbool]
    | _ -> assert false 
  in
  let qvars = false in 
  Cr.map [ast_gen ~qvars ~args:atyp func_max_depth rtyp] ( 
    fun x -> 
      let gcmd =
        FuncDef 
          { name; 
            body = x.gast; 
            atyp = 
              List.filter (
                fun v -> VS.mem v x.args
              ) atyp; 
            rtyp}
      in 
      {gcmd; args = x.args; funcalls = x.funcalls}
  )

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
  let udif = (2, Tint) in
  let udrf = (2, Treal) in
  let udbf = (2, Tbool) in
  fdefs := udif :: udrf :: udbf :: [];  
  begin 
    Cr.add_test ~name:"ae" 
      [ funcdef_gen udif ();
        funcdef_gen udrf ();
        funcdef_gen udbf ();
        axiom_gen ();
        goal_gen ()] 
    @@ 
    fun fi fr fb x y -> 
    Cr.check ( 
      proc (
        let gcmdl =
          [fi; fr; fb; x; y]
        in
        let fs, rl =
          let x, y = 
            List.fold_left (
              fun (acc, cl) x -> 
                FCS.union acc x.funcalls, x.gcmd :: cl
            ) (FCS.empty, []) gcmdl
          in
          x, List.rev y
        in
        List.filter 
          ( fun x -> 
              match x with 
              | FuncDef { name; _} -> FCS.mem name fs 
              | _ -> true)  
          rl
      ) 
    )  end

  (*
  begin 
    Cr.add_test ~name:"ae" 
      [ axiom_gen ();
        goal_gen ()] 
    @@ 
    fun x y -> Cr.check (proc [x; y])
  end
  *)