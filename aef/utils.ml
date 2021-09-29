
type answer = 
  | Sat | Unsat | Unknown

(* When the response of Alt-Ergo is different from the other solvers*)
exception Unsoundness
(* Assert failure or something similar *)
exception InternalCrash
exception Timeout
exception Other of string


type bug_info = { 
  id: int;
  exn: exn option;
  stmtcs: Ast.stmt_c list;
  ae_c: answer list;
  ae_t: answer list;
  cvc5: answer list
}

let mk_bi id exn stmtcs ae_c ae_t cvc5 =
  {id; exn; stmtcs; ae_c; ae_t; cvc5}

let mk_bi_success id stmtcs ae_c ae_t cvc5 =
  mk_bi id None stmtcs ae_c ae_t cvc5


let mk_bi_empty id exn stmtcs =
  mk_bi id exn [] [] [] stmtcs

let answer_to_string = function 
  | Sat -> "sat"
  | Unsat -> "unsat"
  | Unknown -> "unknown"

let exn_to_string = function
  | Unsoundness -> "Failure [Unsoundness]"
  | InternalCrash -> "Failure [Internal Crash]"
  | Timeout -> "Failure [Timeout]"
  | Other str ->
    Format.sprintf "Failure [Other(%s)]" str
  | AltErgoLib.Errors.Error x ->
    Format.asprintf "Failure [AEL_Error(%a)]" AltErgoLib.Errors.report x
  | exn -> Printexc.to_string_default exn

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
    ?(output_folder_path = "aef/crash_output") 
    id exn stmtcs ae_c ae_t cvc5 = 
  let bi =
    mk_bi id (Some exn) stmtcs ae_c ae_t cvc5
  in

  let of_path = 
    Format.sprintf (
      match exn with 
      | Unsoundness -> "%s/u%d_%f.txt"
      | InternalCrash -> "%s/i%d_%f.txt"
      | Timeout -> "%s/t%d_%f.txt"
      | _ -> "%s/o%d_%f.txt"
    ) output_folder_path id (Unix.gettimeofday ())
  in 

  data_to_file bi of_path;

  if verbose
  then (
    Format.printf "\nException: %s@."
      (exn_to_string exn);
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
    ?(output_folder_path = "aef/crash_output")
    id exn stmtcs =
  mknmarshall_bi id
    ~verbose ~output_folder_path
    exn stmtcs [] [] []

let timeout_limit = ref 5

let run_with_timeout timeout proc stmts =
  let old_handler = Sys.signal Sys.sigalrm
      (Sys.Signal_handle (fun _ -> raise Timeout)) in
  let finish () =
    ignore (Unix.alarm 0);
    ignore (Sys.signal Sys.sigalrm old_handler) in
  try
    ignore (Unix.alarm timeout);
    let res = proc stmts in
    finish (); res
  with
  | Timeout -> finish (); raise Timeout
  | exn -> finish (); raise exn

let cmp_answers_exn2 l1 l2 =
  List.iter2 (
    fun x y ->
      match x, y with
      | Sat, Unsat
      | Unsat, Sat -> raise Unsoundness
      | _ -> ()
  ) l1 l2

let cmp_answers_exn3 l1 l2 l3 =
  let rec aux l1 l2 l3 =
    match l1, l2, l3 with
    | h1 :: t1, h2 :: t2, h3 :: t3 ->
      begin match h1, h2, h3 with
        | Unsat, Unsat, Sat
        | Unknown, Unsat, Sat
        | Unsat, Unknown, Sat -> raise Unsoundness
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
