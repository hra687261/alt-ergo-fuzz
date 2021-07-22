
type answer = 
  | Sat | Unsat | Unknown

type bug_type =
  | Unsound
  (* When the response of Alt-Ergo is different from the other solvers*)
  | InternalCrash (* Assert false *)
  | Timeout
  | Other of string

exception Failure of bug_type

type bug_info = { 
  id: int;
  bty: string;
  ae_c: answer list;
  ae_t: answer list;
  cvc5: answer list;
  exp_str: string; 
  exp_bt_str: string; 
  stmtcs: Ast.stmt_c list
}

type stmt_cache = bug_info list

let mk_bi_aux id ae_c ae_t cvc5 bty exp_str exp_bt_str stmtcs =
  {id; ae_c; ae_t; cvc5; bty; exp_str; exp_bt_str; stmtcs}

let mk_bi_success id ae_c ae_t cvc5 stmtcs =
  mk_bi_aux id ae_c ae_t cvc5 "" "" "" stmtcs

let mk_bi id exp ae_c ae_t c5 stmtcs =
  let exp_str = Printexc.to_string exp in 
  let exp_bt_str = Printexc.get_backtrace () in 
  let bty = 
    match exp with 
    | Failure Unsound -> "unsoundness"
    | Failure InternalCrash -> "internalcrash"
    | Failure Timeout -> "timeout"
    | _ -> "other"
  in 
  mk_bi_aux id ae_c ae_t c5 bty exp_str exp_bt_str stmtcs 

let mk_bi_empty id exp stmtcs =
  mk_bi id exp [] [] [] stmtcs

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

let data_to_file data of_path =
  let str = Stdlib.Marshal.to_string data [] in
  let oc = open_out of_path in
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "%s" str;
  close_out oc

let mknmarshall_bi ?(verbose = false)
    ?(output_folder_path = "test/fuzzing/crash_output") 
    id exn stmtcs ae_c ae_t cvc5 = 
  let exn_str = Printexc.to_string exn in 
  let exn_bt_str = Printexc.get_backtrace () in 
  let bty, sym = 
    match exn with 
    | Failure Unsound -> "unsoundness", "u"
    | Failure InternalCrash -> "internalcrash", "i"
    | Failure Timeout -> "timeout", "t"
    | _ -> "other", "o"
  in 
  let bi =
    mk_bi_aux id ae_c ae_t cvc5 bty exn_str exn_bt_str stmtcs
  in

  let of_path =
    Format.sprintf
      "%s/%s%d_%f.txt"
      output_folder_path sym id (Unix.gettimeofday ())
  in

  data_to_file bi of_path;

  if verbose
  then (
    Format.printf "\nException: %s\n%s@."
      exn_str exn_bt_str;
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
      of_path
  ) 

let mknmarshall_bi_na ?(verbose = false)
    ?(output_folder_path = "test/fuzzing/crash_output")
    id exn stmtcs =
  mknmarshall_bi id
    ~verbose ~output_folder_path
    exn stmtcs [] [] []

let mknmarshall_stmt_cache 
    ?(crash_output_folder_path = "test/fuzzing/crash_output") 
    (bis: stmt_cache) =

  let of_path = 
    Format.sprintf
      "%s/c_%f.txt"
      crash_output_folder_path (Unix.gettimeofday ())
  in

  data_to_file bis of_path

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

let cmp_answers_exn3 l1 l2 l3 =
  let rec aux l1 l2 l3 =
    match l1, l2, l3 with
    | h1 :: t1, h2 :: t2, h3 :: t3 ->
      begin match h1, h2, h3 with
        | Unsat, Unsat, Sat
        | Unknown, Unsat, Sat
        | Unsat, Unknown, Sat -> raise (Failure Unsound)
        | _ -> aux t1 t2 t3
      end
    | [], [], [] -> ()
    | _ -> assert false
  in
  let len1, len2, len3 =
    List.length l1, List.length l2, List.length l3
  in
  if (len1 = len2 && len2 = len3)
  then
    aux l1 l2 l3
  else
    raise (
      Invalid_argument (
        Format.sprintf
          "cmp_answers_exn3 [%d] [%d] [%d]"
          len1 len2 len3
      )
    )

let rec cmp_answers_pr2 l1 l2 =
  match l1, l2 with
  | [], [] -> ()
  | [], h2::t2 ->
    Format.printf "no-answer  %s@."
      (answer_to_string h2);
    cmp_answers_pr2 t2 []
  | h1::t1, [] ->
    Format.printf "%s  no-answer@."
      (answer_to_string h1);
    cmp_answers_pr2 t1 []
  | h1::t1, h2::t2 ->
    Format.printf "%s  %s@."
      (answer_to_string h1)
      (answer_to_string h2);
    cmp_answers_pr2 t1 t2

let cmp_answers_pr2_exn l1 l2 =
  List.iter2 (
    fun x y ->
      Format.printf "%s  %s@."
        (answer_to_string x)
        (answer_to_string y)
  ) l1 l2

let cmp_answers_pr3_exn l1 l2 l3 =
  let rec aux l1 l2 l3 =
    match l1, l2, l3 with
    | h1 :: t1, h2 :: t2, h3 :: t3 ->
      Format.printf "%s  %s  %s@."
        (answer_to_string h1)
        (answer_to_string h2)
        (answer_to_string h3);
      aux t1 t2 t3
    | [], [], [] -> ()
    | _ -> assert false
  in
  let len1, len2, len3 =
    List.length l1, List.length l2, List.length l3
  in
  if (len1 = len2 && len2 = len3)
  then
    aux l1 l2 l3
  else
    raise (
      Invalid_argument (
        Format.sprintf
          "cmp_answers_exn3 [%d] [%d] [%d]"
          len1 len2 len3
      )
    )

let rec cmp_answers_pr3 l1 l2 l3 =
  match l1, l2, l3 with
  | h1 :: t1, h2 :: t2, h3 :: t3 ->
    Format.printf "%s  %s  %s@."
      (answer_to_string h1)
      (answer_to_string h2)
      (answer_to_string h3);
    cmp_answers_pr3 t1 t2 t3
  | [], h2 :: t2, h3 :: t3 ->
    Format.printf "no-answer  %s  %s@."
      (answer_to_string h2)
      (answer_to_string h3);
    cmp_answers_pr3 [] t2 t3
  | h1 :: t1, [], h3 :: t3 ->
    Format.printf "%s  no-answer  %s@."
      (answer_to_string h1)
      (answer_to_string h3);
    cmp_answers_pr3 t1 [] t3
  | h1 :: t1, h2 :: t2, [] ->
    Format.printf "%s  %s  no-answer@."
      (answer_to_string h1)
      (answer_to_string h2);
    cmp_answers_pr3 t1 t2 []
  | [], [], h3 :: t3 ->
    Format.printf "no-answer  no-answer  %s@."
      (answer_to_string h3);
    cmp_answers_pr3 [] [] t3
  | [], h2 :: t2, [] ->
    Format.printf "no-answer  %s  no-answer@."
      (answer_to_string h2);
    cmp_answers_pr3 [] t2 []
  | h1 :: t1, [], [] ->
    Format.printf "%s  no-answer  no-answer@."
      (answer_to_string h1);
    cmp_answers_pr3 t1 [] []
  | [], [], [] -> ()
