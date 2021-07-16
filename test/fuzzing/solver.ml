
module type T = Translater.T

module type ST =
sig 
  include T
  val process_stmts : Ast.stmt_c list -> Utils.answer list
end 

module CVC5: ST = 
struct 
  include Smtlib2_tr
  let process_stmts =
    let procl = ref [] in
    let kill_procs () =
      List.iter (
        fun proc ->
          let id = Unix.process_in_pid proc in
          Unix.kill id Sys.sigkill;
          close_in proc
      ) !procl;
      procl := []
    in
    let rec get_lines (ic: in_channel) =
      try
        let l = input_line ic in
        l :: get_lines ic
      with End_of_file -> []
    in
    at_exit kill_procs;
    fun stmtcs ->
      kill_procs ();
      let filename = "_.smt2" in

      let oc = open_out filename in
      let fmt = Format.formatter_of_out_channel oc in
      Format.fprintf fmt "%a" print_stmts stmtcs;
      close_out oc;

      let ic = 
        Unix.open_process_in 
          ( Format.sprintf 
              "cvc5 --incremental --tlimit=%d %s" 
              5000 filename)
      in
      procl := ic :: !procl;
      let lines = get_lines ic in

      List.map (
        function
        | "sat" -> Utils.Sat
        | "unsat" -> Utils.Unsat
        | "unknown" -> Utils.Unknown
        | _ as x -> Format.printf "\n(((%s)))@." x; assert false
      ) lines

end

module AEL = AltErgoLib



(*
val print_set_lb: 
  (module Set.S with type elt = 'a and type t = 't) ->
  (?p:string -> fmt -> 'a -> unit) ->
  (?p:string -> fmt -> 't -> unit)
*)


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

          if Ast.is_goal stmt 
          then 
            if consistent 
            then Utils.Unknown :: al, (env, consistent, ex)
            else Utils.Unsat :: al, (env, consistent, ex)
          else 
            al, (env, consistent, ex)
      )
      ([], (SAT.empty (), true, AEL.Explanation.empty)) 
      stmtcs
  in 
  List.rev ral 

module AE_Tableaux: ST = 
struct 
  include Tr_altergo

  let process_stmts =
    let module SC = AEL.Fun_sat in
    let module SAT = SC.Make(AEL.Theory.Main_Default) in
    AEL.Options.set_disable_weaks true;
    fun stmtcs -> 
      SAT.clear_cache ();
      let filename = "_.ae" in 

      let oc = open_out filename in 
      let fmt = Format.formatter_of_out_channel oc in
      Format.fprintf fmt "%a" print_stmts stmtcs;
      close_out oc;
      solve_with_ae (module SAT) (module Tr_altergo) stmtcs

end 

module AE_CDCL: ST = 
struct 
  include Tr_altergo

  let process_stmts =
    let module SC = AEL.Satml_frontend in
    let module SAT = SC.Make(AEL.Theory.Main_Default) in
    AEL.Options.set_disable_weaks true;
    fun stmtcs -> 
      SAT.clear_cache ();
      let filename = "_.ae" in 

      let oc = open_out filename in 
      let fmt = Format.formatter_of_out_channel oc in
      Format.fprintf fmt "%a" print_stmts stmtcs;
      close_out oc;
      solve_with_ae (module SAT) (module Tr_altergo) stmtcs

end
