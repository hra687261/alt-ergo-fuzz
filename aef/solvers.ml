module type T = Translater.T
module AEL = AltErgoLib

(* CVC5 *)

let c5_ic : in_channel option ref = ref None  

let call_cvc5 ?(timeout = 10000) stmtcs =
  let data = 
    Format.asprintf "%a" Smtlib2_tr.print_stmts stmtcs
  in
  let ic = 
    Unix.open_process_in ( 
      Format.sprintf 
        "echo \"\n%s\n\" | cvc5 --incremental --lang smt2 --tlimit=%d 2>&1" 
        data timeout
    )
  in
  c5_ic := Some ic

let get_cvc5_response () = 
  let rec get_lines (ic: in_channel) =
    try
      let l = input_line ic in
      l :: get_lines ic
    with End_of_file -> []
  in
  match !c5_ic with 
  | Some ic ->
    let lines = get_lines ic in
    let _ = Unix.close_process_in ic in

    List.map (
      fun x ->  
        match x with 
        | "sat" -> Utils.Sat
        | "unsat" -> Utils.Unsat
        | "unknown" -> Utils.Unknown
        | "cvc5 interrupted by timeout." -> 
          Format.printf "\nCVC5.process_stmts <1> %s@." x;
          raise Utils.Timeout
        | _ -> 
          Format.printf "\nCVC5.process_stmts <2> %s@." x;
          raise (Utils.Other x)
    ) lines
  | None -> assert false 

(* Alt-Ergo *)

let set_debug debug = 
  ignore debug
  (*
  AEL.Options.set_verbose debug;
  AEL.Options.set_debug debug;
  AEL.Options.set_debug_ac debug;
  AEL.Options.set_debug_adt debug;
  AEL.Options.set_debug_arith debug;
  AEL.Options.set_debug_arrays debug;
  AEL.Options.set_debug_bitv debug;
  AEL.Options.set_debug_cc debug;
  AEL.Options.set_debug_combine debug;
  AEL.Options.set_debug_constr debug;
  AEL.Options.set_debug_explanations debug;
  AEL.Options.set_debug_fm debug;
  (*AEL.Options.set_debug_fpa : int -> unit*)
  AEL.Options.set_debug_gc debug;
  AEL.Options.set_debug_interpretation debug;
  AEL.Options.set_debug_ite debug;
  (*AEL.Options.set_debug_matching : int -> unit*)
  AEL.Options.set_debug_sat debug;
  AEL.Options.set_debug_split debug;
  AEL.Options.set_debug_sum debug;
  AEL.Options.set_debug_triggers debug;
  AEL.Options.set_debug_types debug;
  AEL.Options.set_debug_typing debug;
  AEL.Options.set_debug_uf debug;
  AEL.Options.set_debug_unsat_core debug;
  AEL.Options.set_debug_use debug;
  AEL.Options.set_debug_warnings debug
  *)

let solve_with_ae 
    (module SAT: AEL.Sat_solver_sig.S)
    (module Tr: T with type t = AEL.Commands.sat_tdecl) = 

  let module FE = AEL.Frontend.Make(SAT) in
  let solve ctx tstmts goal_name =
    let ctx = FE.choose_used_context ctx ~goal_name in
    let st = Stack.create () in
    let resp, _ =
      List.fold_left (
        fun (resp, (env, consistent, ex)) tstmt ->
          let env, consistent, ex =
            FE.process_decl
              (fun _ _ -> ()) (*FE.print_status*)
              ctx st
              (env, consistent, ex) tstmt
          in
          match resp, tstmt.st_decl with
          | None, AEL.Commands.Query _ ->
            if consistent
            then Some Utils.Unknown, (env, consistent, ex)
            else Some Utils.Unsat, (env, consistent, ex)
          | None, _ ->
            resp, (env, consistent, ex)
          | _ ->
            Format.fprintf Format.err_formatter
              "\nSolve is expected to get one goal at a time@.";
            assert false
      ) (None, (SAT.empty (), true, AEL.Explanation.empty)) 
        tstmts
    in
    Option.get resp
  in
  let ctx =
    FE.init_all_used_context ()
  in 

  fun stmtcs ->
    let resps, _ =
      List.fold_left (
        fun (resps , tstmts) Ast.{stmt; _} ->
          let tstmt = Tr.translate_stmt stmt in 
          match stmt with
          | Ast.Goal {name; _} ->
            let resp =
              solve ctx (List.rev_append tstmts [tstmt]) name
            in
            resp :: resps, []
          | _ ->
            resps, tstmt :: tstmts
      ) ([], []) stmtcs
    in
    List.rev resps

(* Alt-Ergo Tableaux *)

let run_with_ae_t = 
  let module SC = AEL.Fun_sat in
  let module Th = AEL.Theory.Main_Default in
  let module SAT = SC.Make(Th) in
  AEL.Options.set_disable_weaks true;
  AEL.Options.set_use_fpa true;

  fun ?(debug = false) stmtcs ->
    set_debug debug;
    try
      let res =
        solve_with_ae (module SAT) (module Tr_altergo) stmtcs
      in
      SAT.reinit_ctx ();
      Tr_altergo.reset_cnt ();
      res
    with
    | exn ->
      SAT.reinit_ctx ();
      Tr_altergo.reset_cnt ();
      Printexc.raise_with_backtrace 
        exn (Printexc.get_raw_backtrace ())

(* Alt-Ergo CDCL *)

let run_with_ae_c =
  let module SC = AEL.Satml_frontend in
  let module Th = AEL.Theory.Main_Default in
  let module SAT = SC.Make(Th) in
  AEL.Options.set_disable_weaks true;
  AEL.Options.set_use_fpa true;

  fun ?(debug = false) stmtcs -> 
    set_debug debug;
    try
      let res =
        solve_with_ae (module SAT) (module Tr_altergo) stmtcs
      in
      SAT.reinit_ctx ();
      Tr_altergo.reset_cnt ();
      res
    with
    | exn ->
      SAT.reinit_ctx ();
      Tr_altergo.reset_cnt ();
      Printexc.raise_with_backtrace
        exn (Printexc.get_raw_backtrace ())
