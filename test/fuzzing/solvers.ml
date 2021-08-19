module type T = Translater.T
module AEL = AltErgoLib

(* CVC5 *)

let c5_ic : in_channel option ref = ref None  

let call_cvc5 ?(timeout = 10000) stmtcs =
  let filename = 
    Format.sprintf "tmp_%f.smt2"
      (Unix.gettimeofday ()) in

  let oc = open_out filename in
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "%a" Smtlib2_tr.print_stmts stmtcs;
  close_out oc;

  let ic = 
    Unix.open_process_in 
      ( Format.sprintf 
          "cvc5 --incremental --tlimit=%d %s 2>&1; rm %s" 
          timeout filename filename)
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

let solve_with_ae 
    (module SAT: AEL.Sat_solver_sig.S)
    (module Tr: T with type t = AEL.Commands.sat_tdecl) stmtcs = 
  let module FE = AEL.Frontend.Make(SAT) in
  let ral, _ =
    List.fold_left
      ( fun (al, (env, consistent, ex)) Ast.{stmt;_} ->

          let tstmt = Tr.translate_stmt stmt in

          let env, consistent, ex =
            FE.process_decl
              (fun _ _ -> ()) (*FE.print_status*)
              (FE.init_all_used_context ())
              (Stack.create ())
              (env, consistent, ex) tstmt
          in
          match stmt with
          | Ast.Goal _ ->
            if consistent
            then Utils.Unknown :: al, (env, consistent, ex)
            else Utils.Unsat :: al, (env, consistent, ex)
          | _ ->
            al, (env, consistent, ex)
      )
      ([], (SAT.empty (), true, AEL.Explanation.empty)) 
      stmtcs
  in List.rev ral

(* Alt-Ergo Tableaux *)

let run_with_ae_t = 
  let module SC = AEL.Fun_sat in
  let module Th = AEL.Theory.Main_Default in
  let module SAT = SC.Make(Th) in
  AEL.Options.set_disable_weaks true;
  AEL.Options.set_use_fpa true;

  fun stmtcs -> 
    try 
      let res =
        solve_with_ae (module SAT) (module Tr_altergo) stmtcs
      in
      SAT.clear_cache ();
      Tr_altergo.reset_cnt ();
      res
    with 
    | exn ->
      SAT.clear_cache ();
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

  fun stmtcs -> 
    try
      let res =
        solve_with_ae (module SAT) (module Tr_altergo) stmtcs
      in
      SAT.clear_cache ();
      Tr_altergo.reset_cnt ();
      res
    with
    | exn ->
      SAT.clear_cache ();
      Tr_altergo.reset_cnt ();
      Printexc.raise_with_backtrace
        exn (Printexc.get_raw_backtrace ())
