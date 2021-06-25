
type bug_info = { 
  id: int;
  exp_str: string; 
  exp_bt_str: string; 
  decls: Ast.decl list}

exception Timeout 
exception Failure of bug_info

let mk_bug_info id exp_str exp_bt_str decls =
  {id; exp_str; exp_bt_str; decls}

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

let run_with_timeout timeout proc decls =
  let old_handler = Sys.signal Sys.sigalrm
      (Sys.Signal_handle (fun _ -> raise Timeout)) in
  let finish () =
    ignore (Unix.alarm 0);
    ignore (Sys.signal Sys.sigalrm old_handler) in
  try
    ignore (Unix.alarm timeout);
    let res = proc decls in
    finish (); res
  with
  | Timeout -> finish (); raise Timeout
  | exn -> finish (); raise exn
