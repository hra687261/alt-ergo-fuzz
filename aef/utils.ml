
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
  ae_ct: answer list;
  ae_t: answer list;
  ae_tc: answer list;
  cvc5: answer list
}

let mk_bi id exn stmtcs ae_c ae_ct ae_t ae_tc cvc5 =
  {id; exn; stmtcs; ae_c; ae_ct; ae_t; ae_tc; cvc5}

(*
let ans_to_str = function
  | Sat -> "sat"
  | Unsat -> "unsat"
  | Unknown -> "unknown"
*)

let exn_to_str = function
  | Unsoundness -> "Failure [Unsoundness]"
  | InternalCrash -> "Failure [Internal Crash]"
  | Timeout -> "Failure [Timeout]"
  | Other str ->
    Format.sprintf "Failure [Other(%s)]" str
  | AltErgoLib.Errors.Error x ->
    Format.asprintf "Failure [AEL_Error(%a)]" AltErgoLib.Errors.report x
  | exn -> Printexc.to_string_default exn

let data_to_file data of_path =
  let str = Stdlib.Marshal.to_string data [] in
  let oc = open_out of_path in
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "%s" str;
  close_out oc

let handle_unsoundness_bug ?(verbose = false)
    ?(output_folder_path = "aef/store")
    id exn stmtcs ae_c ae_ct ae_t ae_tc cvc5 =

  let exn_str = function
    | Unsoundness -> "unsoundness"
    | InternalCrash -> "internalcrash"
    | Timeout -> "timeout"
    | Stack_overflow -> "stackoverflow"
    | Out_of_memory -> "outofmemory"
    | _ -> "other"
  in
  let bi = mk_bi id (Some exn) stmtcs ae_c ae_ct ae_t ae_tc cvc5 in
  let of_path =
    Format.sprintf "%s/%s/%d_%.22f.txt" output_folder_path
      (exn_str exn) (Unix.getpid ()) (Unix.gettimeofday ())
  in
  data_to_file bi of_path;

  if verbose
  then (
    Format.printf "\nException: %s@." (exn_to_str exn);
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

let handle_failure_bug ?(verbose = false)
    ?(output_folder_path = "aef/store")
    id exn stmtcs =
  handle_unsoundness_bug id
    ~verbose ~output_folder_path
    exn stmtcs [] [] [] [] []

let cmp_answers l1 l2 l3 l4 l5 =
  let is_unsat = function
      Unsat -> true
    | _ -> false
  in
  let rec aux l1 l2 l3 l4 l5 =
    match l1, l2, l3, l4, l5 with
    | h1 :: t1, h2 :: t2, h3 :: t3, h4 :: t4, h5 :: t5 ->
      begin match h1, h2, h3, h4, h5 with
        | a1, a2, a3, a4, Sat when
            is_unsat a1 || is_unsat a2 ||
            is_unsat a3 || is_unsat a4 -> raise Unsoundness
        | _ -> aux t1 t2 t3 t4 t5
      end
    | [], [], [], [], [] -> ()
    | _ -> assert false
  in
  let len1, len2, len3, len4, len5 =
    List.length l1,
    List.length l2,
    List.length l3,
    List.length l4,
    List.length l5
  in
  if len1 = len2 && len2 = len3 && len3 = len4 && len4 = len5
  then
    aux l1 l2 l3 l4 l5
  else
    raise (
      Invalid_argument (
        Format.sprintf
          "cmp_answers [%d] [%d] [%d] [%d] [%d]"
          len1 len2 len3 len4 len5
      )
    )

let pp_answers l1 l2 l3 l4 l5 =
  let len = List.length in
  let len1 = len l1 in
  let len2 = len l2 in
  let len3 = len l3 in
  let len4 = len l4 in
  let len5 = len l5 in
  let ml = max len1 len2 in
  let ml = max ml len3 in
  let ml = max ml len4 in
  let ml = max ml len5 in
  let map = List.map in
  let some = Option.some in
  let l1 = (map some l1) @ (List.init (ml - len1) (fun _ -> None)) in
  let l2 = (map some l2) @ (List.init (ml - len2) (fun _ -> None)) in
  let l3 = (map some l3) @ (List.init (ml - len3) (fun _ -> None)) in
  let l4 = (map some l4) @ (List.init (ml - len4) (fun _ -> None)) in
  let l5 = (map some l5) @ (List.init (ml - len5) (fun _ -> None)) in

  let ans_to_str = function
    | Some Sat -> "sat   "
    | Some Unsat -> "unsat "
    | Some Unknown -> "unkown"
    | None -> "none  "
  in

  let rec aux l1 l2 l3 l4 l5 =
    match l1, l2, l3, l4, l5 with
    | h1 :: t1, h2 :: t2, h3 :: t3, h4 :: t4, h5 :: t5 ->
      Format.printf "%s  %s  %s  %s  %s@."
        (ans_to_str h1) (ans_to_str h2) (ans_to_str h3)
        (ans_to_str h4) (ans_to_str h5);
      aux t1 t2 t3 t4 t5
    | [], [], [], [], [] -> ()
    | _ -> assert false
  in
  aux l1 l2 l3 l4 l5
