open AltErgoLib
open Ast
open Generator
open Translate_ae
module Cr = Crowbar 


module SAT = Fun_sat.Make(Theory.Main_Default)
module FE = Frontend.Make(SAT)

exception Timeout

type bug_info = { 
  id: int;
  exp_str: string; 
  exp_bt_str: string; 
  decls: decl list}

let cnt = ref 0 

let mk_bug_info id exp_str exp_bt_str decls =
  {id; exp_str; exp_bt_str; decls}

let sh_printf ?(firstcall = false) ?(filename = "debug.txt") content =
  let str =
    Format.sprintf  
      ( if firstcall 
        then "printf \"%s\" > %s 2>&1"
        else "printf \"%s\" >> %s 2>&1"
      ) content filename
  in
  let command = 
    Lwt_process.shell str
  in 
  ignore (
    Lwt_process.exec 
      ~stdin:`Dev_null 
      ~stdout:`Keep 
      ~stderr:`Keep 
      command)

let reinit_env () = 
  incr cnt;
  SAT.reset_refs ();
  Expr.clear_hc ();
  Shostak.Combine.empty_cache ();
  Gc.major ()

let solve decls =
  reinit_env ();
  List.fold_left 
    ( fun (env, consistent, ex) decl ->

        let command = translate_decl decl in 

        let env, consistent, ex = 
          FE.process_decl 
            (fun _ _ -> ()) (*FE.print_status*)
            (FE.init_all_used_context ()) 
            (Stack.create ()) 
            (env, consistent, ex) command
        in

        if is_goal decl 
        then 
          sh_printf
            ( if consistent 
              then "unknown\n" 
              else "unsat\n");
        env, consistent, ex
    )
    (SAT.empty (), true, Explanation.empty) 
    decls

let run_with_timeout timeout solve decls =
  let old_handler = Sys.signal Sys.sigalrm
      (Sys.Signal_handle (fun _ -> raise Timeout)) in
  let finish () =
    ignore (Unix.alarm 0);
    ignore (Sys.signal Sys.sigalrm old_handler) in
  try
    ignore (Unix.alarm timeout);
    ignore (solve decls);
    finish ()
  with
  | Timeout -> finish (); raise Timeout
  | exn -> finish (); raise exn

let proc decls = 
  try
    sh_printf "\n";
    run_with_timeout 5 solve decls; 
    true
  with
  | Timeout -> 
    sh_printf "timed out\n"; 
    true
  | exp ->
    let id = !cnt in 
    let exp_str = Printexc.to_string exp in 
    let exp_bt_str = Printexc.get_backtrace () in 
    let bi = mk_bug_info id exp_str exp_bt_str decls in 

    let tmp = Stdlib.Marshal.to_string bi [] in
    let file_name = 
      Format.sprintf
        "test/fuzzing/crash_output/op_%d_%f.txt"
        !cnt (Unix.gettimeofday ())
    in

    let oc = open_out file_name in
    output_string oc tmp;
    close_out oc;

    sh_printf (
      Format.sprintf "\nException: %s\n%s@." 
        exp_str exp_bt_str
    );
    sh_printf (
      Format.asprintf "\nCaused by: \n%a@." 
        ( fun fmt decll ->
            List.iter ( 
              fun decl ->
                Format.fprintf fmt "\n### %a@." print_decl decl;
                let tdecl = translate_decl decl in 
                Format.fprintf fmt ">>> %a@." Commands.print tdecl
            ) decll
        ) decls
    );
    sh_printf (
      Format.sprintf 
        "Marshalled and written to the file : %s@." 
        file_name
    );
    false

let () =
  Options.set_disable_weaks true;
  Options.set_is_gui false;
  sh_printf ~firstcall:true ""; 
  Cr.add_test ~name:"ae" [gen_decls] 
    (fun decls -> Cr.check (proc decls))



