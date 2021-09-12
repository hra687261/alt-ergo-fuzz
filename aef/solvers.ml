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

module type SolverType =
sig
  module SAT: AEL.Sat_solver_sig.S
  module FE: AEL.Frontend.S with type sat_env = SAT.t
end

module MakeSolver(Solver: AEL.Sat_solver_sig.S) =
struct 
  module SAT = Solver
  module FE = AEL.Frontend.Make(SAT)
end

module Theory = AEL.Theory.Main_Default

module CDCL_solver =
  MakeSolver(AEL.Satml_frontend.Make(Theory))

module Tableaux_solver =
  MakeSolver(AEL.Fun_sat.Make(Theory))


let solve_with_ae =
  AEL.Options.set_disable_weaks true;
  AEL.Options.set_use_fpa true;

  fun (module Solver: SolverType) ->
    let solve ctx tstmts goal_name =
      let ctx = Solver.FE.choose_used_context ctx ~goal_name in
      let st = Stack.create () in
      let resp, _ =
        List.fold_left (
          fun (resp, (env, consistent, ex)) tstmt ->
            let env, consistent, ex =
              Solver.FE.process_decl
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
        ) (None, (Solver.SAT.empty (), true, AEL.Explanation.empty)) 
          tstmts
      in
      Option.get resp
    in
    let ctx =
      Solver.FE.init_all_used_context ()
    in

    fun stmtcs ->
      try
        let rresps, _ =
          List.fold_left (
            fun (resps , tstmts) Ast.{stmt; _} ->
              let tstmt = Tr_altergo.translate_stmt stmt in 
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
        let resps = List.rev rresps in
        Solver.SAT.reinit_ctx ();
        Tr_altergo.reset_cnt ();
        resps
      with
      | exn ->
        Solver.SAT.reinit_ctx ();
        Tr_altergo.reset_cnt ();
        Printexc.raise_with_backtrace 
          exn (Printexc.get_raw_backtrace ())


(* Alt-Ergo Tableaux *)

let run_with_ae_t = 
  fun stmtcs ->
  AEL.Options.set_sat_solver AEL.Util.Tableaux;
  AEL.Options.set_tableaux_cdcl false;
  AEL.Options.set_cdcl_tableaux_inst false;
  AEL.Options.set_cdcl_tableaux_th false;
  solve_with_ae (module Tableaux_solver) stmtcs

(* Alt-Ergo Tableaux-CDCL *)

let run_with_ae_tc = 
  fun stmtcs ->
  AEL.Options.set_sat_solver AEL.Util.Tableaux_CDCL;
  AEL.Options.set_tableaux_cdcl true;
  AEL.Options.set_cdcl_tableaux_inst false;
  AEL.Options.set_cdcl_tableaux_th false;
  solve_with_ae (module Tableaux_solver) stmtcs

(* Alt-Ergo CDCL *)

let run_with_ae_c =
  fun stmtcs ->
  AEL.Options.set_sat_solver AEL.Util.CDCL;
  AEL.Options.set_tableaux_cdcl false;
  AEL.Options.set_cdcl_tableaux_inst false;
  AEL.Options.set_cdcl_tableaux_th false;
  solve_with_ae (module CDCL_solver) stmtcs

(* Alt-Ergo CDCL-Tableaux *)

let run_with_ae_ct =
  fun stmtcs ->
  AEL.Options.set_sat_solver AEL.Util.CDCL_Tableaux;
  AEL.Options.set_tableaux_cdcl false;
  AEL.Options.set_cdcl_tableaux_inst true;
  AEL.Options.set_cdcl_tableaux_th true;
  solve_with_ae (module CDCL_solver) stmtcs
