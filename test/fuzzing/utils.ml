
type bug_type =
  | Unsound 
  (* When the response of Alt-Ergo is different from the other solvers*)
  | InternalCrash (* Assert false *)
  | Timeout

exception Failure of bug_type

let cnt = ref 0 

type bug_info = { 
  id: int;
  bty: string;
  exp_str: string; 
  exp_bt_str: string; 
  decls: Ast.decl list}

let mk_bug_info id bty exp_str exp_bt_str decls =
  {id; bty; exp_str; exp_bt_str; decls}

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

let mknmarshall_bi 
    ?(verbose = false) ?(filename = "debug.txt")
    ?(crash_output_folder_path = "test/fuzzing/crash_output") 
    (exp: exn) (decls: Ast.decl list) = 
  let id = !cnt in 
  let exp_str = Printexc.to_string exp in 
  let exp_bt_str = Printexc.get_backtrace () in 
  let bty, sym = 
    match exp with 
    | Failure Unsound -> "unsoundness", "u"
    | Failure InternalCrash -> "internalcrash", "ic"
    | Failure Timeout -> "timeout", "to"
    | _ -> "other", "o"
  in
  let bi = mk_bug_info id bty exp_str exp_bt_str decls in 
  let m = Stdlib.Marshal.to_string bi [] in
  let file_name = 
    Format.sprintf
      "%s/crash_%s_%d_%f.txt"
      crash_output_folder_path sym !cnt (Unix.gettimeofday ())
  in
  let oc = open_out file_name in
  output_string oc m;
  close_out oc;
  if verbose 
  then (
    sh_printf ~filename (
      Format.sprintf "\nException: %s\n%s@." 
        exp_str exp_bt_str
    );
    sh_printf ~filename (
      Format.asprintf "\nCaused by: \n%a@." 
        ( fun fmt decll ->
            List.iter ( 
              fun decl ->
                Format.fprintf fmt "\n### %a@." Ast.print_decl decl;
            ) decll
        ) decls
    );
    sh_printf ~filename (
      Format.sprintf 
        "Marshalled and written to the file : %s@." 
        file_name
    )
  ) 

let timeout_limit = ref 5

let run_with_timeout timeout proc decls =
  let old_handler = Sys.signal Sys.sigalrm
      (Sys.Signal_handle (fun _ -> raise (Failure Timeout))) in
  let finish () =
    ignore (Unix.alarm 0);
    ignore (Sys.signal Sys.sigalrm old_handler) in
  try
    ignore (Unix.alarm timeout);
    let res = proc decls in
    finish (); res
  with
  | Failure Timeout -> finish (); raise (Failure Timeout)
  | exn -> finish (); raise exn

let cmp_answers_exn2 l1 l2 = 
  List.iter2 (
    fun x y -> if x != y then raise (Failure Unsound)
  ) l1 l2

let cmp_answers_pr2 l1 l2 = 
  List.iter2 (
    fun x y -> 
      let aux = 
        function 
        | Translate.Sat -> "sat"
        | Translate.Unsat -> "unsat"
        | Translate.Unknown -> "unknown"
      in
      Format.printf "%s %s@." 
        (aux x) (aux y)
  ) l1 l2
