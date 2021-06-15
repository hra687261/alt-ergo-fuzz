open AltErgoLib
open Ast
open Generator
open Translate_ae
module Cr = Crowbar 

(********************************************************************)
module SAT = Fun_sat.Make(Theory.Main_Default)
module FE = Frontend.Make(SAT)

exception Timeout

let reinit_env () = 
  SAT.reset_refs ();
  Expr.clear_hc ();
  Shostak.Combine.empty_cache ();
  Gc.major ()

let solve cmds =
  reinit_env ();
  Format.printf "\n";
  let _ = 
    List.fold_left ( 
      fun (env, consistent, ex) cmd ->

        let command = translate_decl cmd in 

        let env, consistent, ex = 
          FE.process_decl 
            (fun _ _ -> ()) (*FE.print_status*)
            (FE.init_all_used_context ()) 
            (Stack.create ()) 
            (env, consistent, ex) command
        in

        if is_goal cmd 
        then 
          Format.printf "%s@."
            ( if consistent 
              then "unknown" 
              else "unsat");
        env, consistent, ex
    )
      (SAT.empty (), true, Explanation.empty) 
      cmds
  in ()

let run_with_timeout timeout solve cmds =
  let old_handler = Sys.signal Sys.sigalrm
      (Sys.Signal_handle (fun _ -> raise Timeout)) in
  let finish () =
    ignore (Unix.alarm 0);
    ignore (Sys.signal Sys.sigalrm old_handler) in
  try
    ignore (Unix.alarm timeout);
    ignore (solve cmds);
    finish ()
  with
  | Timeout -> finish (); raise Timeout
  | exn -> finish (); raise exn

let proc cmds = 
  try
    (try
       run_with_timeout 5 solve cmds;
     with Timeout -> Format.printf "timed out@.");
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
    Format.printf 
      "Marshalled and written to the file : %s@." 
      file_name;
      
    false

let () =
  Options.set_disable_weaks true;
  Options.set_is_gui false;
  Cr.add_test ~name:"ae" [gen_decls] 
    (fun decls -> Cr.check (proc decls))



