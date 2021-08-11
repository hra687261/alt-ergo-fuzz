
module type T = Translater.T

module type ST =
sig 
  include T
  val process_stmts : 
    ?debug:bool -> Ast.stmt_c list -> Utils.answer list
end 

module CVC5: ST = 
struct 
  include Smtlib2_tr

  let process_stmts =
    let rec get_lines (ic: in_channel) =
      try
        let l = input_line ic in
        l :: get_lines ic
      with End_of_file -> []
    in
    fun ?(debug = false) stmtcs ->
      ignore debug;
      let filename = 
        Format.sprintf "tmp_%f.smt2"
          (Unix.gettimeofday ()) in

      let oc = open_out filename in
      let fmt = Format.formatter_of_out_channel oc in
      Format.fprintf fmt "%a" print_stmts stmtcs;
      close_out oc;

      let ic = 
        Unix.open_process_in 
          ( Format.sprintf 
              "cvc5 --incremental --tlimit=%d %s 2>&1; rm %s" 
              10000 filename filename)
      in
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
            raise (Utils.Failure Utils.Timeout)
          | _ -> 
            Format.printf "\nCVC5.process_stmts <2> %s@." x;
            raise (Utils.Failure (Utils.Other x))
      ) lines

end

module AEL = AltErgoLib

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

let set_debug debug = 
  ignore debug
  (*
  AEL.Options.set_verbose b;
  AEL.Options.set_debug b;
  AEL.Options.set_debug_ac b; (**)
  AEL.Options.set_debug_adt b; (**)
  AEL.Options.set_debug_arith b;
  AEL.Options.set_debug_arrays b;
  AEL.Options.set_debug_bitv b;
  AEL.Options.set_debug_cc b;
  AEL.Options.set_debug_combine b;
  AEL.Options.set_debug_constr b;
  AEL.Options.set_debug_explanations b;
  AEL.Options.set_debug_fm b;
  (*AEL.Options.set_debug_fpa : int -> unit*)
  AEL.Options.set_debug_gc b;
  AEL.Options.set_debug_interpretation b;
  AEL.Options.set_debug_ite b; 
  (*AEL.Options.set_debug_matching : int -> unit*)
  AEL.Options.set_debug_sat b;
  AEL.Options.set_debug_split b;
  AEL.Options.set_debug_sum b;
  AEL.Options.set_debug_triggers b;
  AEL.Options.set_debug_types b; 
  AEL.Options.set_debug_typing b; 
  AEL.Options.set_debug_uf b; 
  AEL.Options.set_debug_unsat_core b; 
  AEL.Options.set_debug_use b; 
  AEL.Options.set_debug_warnings b 
  *)

module AE_Tableaux: ST = 
struct 
  include Tr_altergo

  let process_stmts =
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
        SAT.clear_cache ();
        Tr_altergo.reset_cnt ();
        res
      with 
      | exn ->
        SAT.clear_cache ();
        Tr_altergo.reset_cnt ();
        Printexc.raise_with_backtrace 
          exn (Printexc.get_raw_backtrace ())
end 

module AE_CDCL: ST = 
struct 
  include Tr_altergo

  let process_stmts =
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
        SAT.clear_cache ();
        Tr_altergo.reset_cnt ();
        res
      with
      | exn ->
        SAT.clear_cache ();
        Tr_altergo.reset_cnt ();
        Printexc.raise_with_backtrace
          exn (Printexc.get_raw_backtrace ())
end
