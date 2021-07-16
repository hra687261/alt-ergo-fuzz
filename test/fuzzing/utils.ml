
type answer = 
  | Sat | Unsat | Unknown

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
  stmtcs: Ast.stmt_c list}

let mk_bug_info id bty exp_str exp_bt_str stmtcs =
  {id; bty; exp_str; exp_bt_str; stmtcs}

let answer_to_string = function 
  | Sat -> "sat"
  | Unsat -> "unsat"
  | Unknown -> "unknown"

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

let mknmarshall_bi ?(verbose = false)
    ?(crash_output_folder_path = "test/fuzzing/crash_output") 
    (exp: exn) stmtcs = 
  let id = !cnt in 
  let exp_str = Printexc.to_string exp in 
  let exp_bt_str = Printexc.get_backtrace () in 
  let bty, sym = 
    match exp with 
    | Failure Unsound -> "unsoundness", "u"
    | Failure InternalCrash -> "internalcrash", "i"
    | Failure Timeout -> "timeout", "t"
    | _ -> "other", "o"
  in 
  let bi = mk_bug_info id bty exp_str exp_bt_str stmtcs in 
  let data = Stdlib.Marshal.to_string bi [] in

  let file_name = 
    Format.sprintf
      "%s/%s%d_%f.txt"
      crash_output_folder_path sym !cnt (Unix.gettimeofday ())
  in

  let oc = open_out file_name in 
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "%s" data;
  close_out oc;

  if verbose 
  then (
    Format.printf "\nException: %s\n%s@." 
      exp_str exp_bt_str;
    Format.printf "\nCaused by: \n%a@." 
      ( fun fmt stmtcl ->
          List.iter ( 
            fun Ast.{stmt; _} ->
              Format.fprintf fmt "\n### %a@." 
                Ast.print_stmt stmt
          ) stmtcl
      ) stmtcs;
    Format.printf 
      "Marshalled and written to the file : %s@." 
      file_name
  ) 

let timeout_limit = ref 5

let run_with_timeout timeout proc stmts =
  let old_handler = Sys.signal Sys.sigalrm
      (Sys.Signal_handle (fun _ -> raise (Failure Timeout))) in
  let finish () =
    ignore (Unix.alarm 0);
    ignore (Sys.signal Sys.sigalrm old_handler) in
  try
    ignore (Unix.alarm timeout);
    let res = proc stmts in
    finish (); res
  with
  | Failure Timeout -> finish (); raise (Failure Timeout)
  | exn -> finish (); raise exn

let cmp_answers_exn2 l1 l2 =
  List.iter2 (
    fun x y ->
      match x, y with
      | Sat, Unsat 
      | Unsat, Sat -> raise (Failure Unsound)
      | _ -> ()
  ) l1 l2

let rec cmp_answers_exn3 l1 l2 l3 =
  match l1, l2, l3 with
  | h1 :: t1, h2 :: t2, h3 :: t3 ->
    begin match h1, h2, h3 with
      | Unsat, Unsat, Sat
      | Unknown, Unsat, Sat
      | Unsat, Unknown, Sat -> raise (Failure Unsound)
      | _ -> cmp_answers_exn3 t1 t2 t3
    end
  | [], [], [] -> ()
  | _ -> assert false


let cmp_answers_pr2 l1 l2 =
  List.iter2 (
    fun x y ->
      Format.printf "%s  %s@."
        (answer_to_string x)
        (answer_to_string y)
  ) l1 l2

let rec cmp_answers_pr3 l1 l2 l3 =
  match l1, l2, l3 with
  | h1 :: t1, h2 :: t2, h3 :: t3 ->
    Format.printf "%s  %s  %s@."
      (answer_to_string h1)
      (answer_to_string h2)
      (answer_to_string h3);
    cmp_answers_pr3 t1 t2 t3
  | [], [], [] -> ()
  | _ -> assert false
