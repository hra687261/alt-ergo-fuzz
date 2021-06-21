
module AEL = AltErgoLib
module Tae = Translate_ae

module SAT = AEL.Fun_sat.Make(AEL.Theory.Main_Default)
module FE = AEL.Frontend.Make(SAT)

exception Timeout

type bug_info = { 
  id: int;
  exp_str: string; 
  exp_bt_str: string; 
  decls: Ast.decl list}

let mk_bug_info id exp_str exp_bt_str decls =
  {id; exp_str; exp_bt_str; decls}

let cnt = ref 0 

let timeout_limit = ref 5

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
  AEL.Expr.clear_hc ();
  AEL.Shostak.Combine.empty_cache ();
  Gc.major ()

let solve decls =
  reinit_env ();
  List.fold_left 
    ( fun (env, consistent, ex) decl ->

        let tdecl = Tae.translate_decl decl in 

        let env, consistent, ex = 
          FE.process_decl 
            (fun _ _ -> ()) (*FE.print_status*)
            (FE.init_all_used_context ()) 
            (Stack.create ()) 
            (env, consistent, ex) tdecl
        in

        if Ast.is_goal decl 
        then 
          sh_printf
            ( if consistent 
              then "unknown\n" 
              else "unsat\n");
        env, consistent, ex
    )
    (SAT.empty (), true, AEL.Explanation.empty) 
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
