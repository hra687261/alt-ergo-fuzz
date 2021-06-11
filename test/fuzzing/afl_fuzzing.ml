open AltErgoLib
open Ast
open Generator
open Translate_ae
module Cr = Crowbar 

(********************************************************************)
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
          let command = translate_decl cmd in 
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
              let command = translate_decl cmd in 
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
  Cr.add_test ~name:"ae" [gen_decls] 
    (fun decls -> Cr.check (proc decls))



