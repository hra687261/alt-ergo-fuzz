
type answer =
  | Sat | Unsat | Unknown

type solver =
  | AE_C
  | AE_CT
  | AE_T
  | AE_TC
  | CVC5

type output_lang = Native | Smtlib2

type terminal =
  | GnomeTerminal
  | XTerm
  | Konsole

let pp_terminal fmt term =
  match term with
  | GnomeTerminal ->
    Format.fprintf fmt "gnome-terminal"
  | XTerm ->
    Format.fprintf fmt "xterm"
  | Konsole ->
    Format.fprintf fmt "konsole"

let terminal_wrap_cmd term cmd =
  match term with
  | GnomeTerminal ->
    Format.sprintf "gnome-terminal -- bash -c \"%s; exec bash\"" cmd
  | XTerm ->
    Format.sprintf "xterm -hold -e \"%s\"" cmd
  | Konsole ->
    Format.sprintf "konsole --hold -e \"%s\"" cmd

let (<+>) v_opt default =
  match v_opt with
  | Some v -> v
  | None -> default

let opt_app f ~default v_opt =
  match v_opt with
  | Some v -> f v
  | None -> default

let opt_app_fmt fmt fmtter v_opt =
  opt_app (Format.fprintf fmtter fmt) ~default:() v_opt

(* When the response of Alt-Ergo is different from the other solvers*)
exception Unsoundness
(* Assert failure or something similar *)
exception InternalCrash
exception Timeout
exception Other of string

let pp_output_lang fmt opl =
  match opl with
  | Native -> Format.fprintf fmt "native"
  | Smtlib2 -> Format.fprintf fmt "smtlib2"

let print_solver fmt = function
  | AE_C -> Format.fprintf fmt "Alt-Ergo(CDCL)"
  | AE_CT -> Format.fprintf fmt "Alt-Ergo(CDCL-Tableaux)"
  | AE_T -> Format.fprintf fmt "Alt-Ergo(Tableaux)"
  | AE_TC -> Format.fprintf fmt "Alt-Ergo(Tableaux-CDCL)"
  | CVC5 -> Format.fprintf fmt "CVC5"

let solver_to_sid = function
  | AE_C -> 1
  | AE_CT -> 2
  | AE_T -> 3
  | AE_TC -> 4
  | CVC5 -> 5

let sid_to_solver = function
  | 1 -> AE_C
  | 2 -> AE_CT
  | 3 -> AE_T
  | 4 -> AE_TC
  | 5 -> CVC5
  | n ->
    failwith (Format.sprintf "Unbound solver-id: %d" n)

module IM = Map.Make(
  struct
    type t = int
    let compare = Int.compare
  end)

type bug_info = {
  id: int;
  exn: exn option;
  stmtcs: Ast.stmt_c list;
  answers: answer list IM.t;
}

let get_bug_info ipf =
  let ic = open_in ipf in
  let str = really_input_string ic (in_channel_length ic) in
  close_in ic;
  (Marshal.from_string str 0: bug_info)

let mk_im (answers: (solver * answer list) list) =
  List.fold_left (
    fun m (s, al) ->
      IM.add (solver_to_sid s) al m
  ) IM.empty answers

let im_to_list (m: answer list IM.t) =
  IM.fold (
    fun sid al acc ->
      (sid_to_solver sid, al) :: acc
  ) m []

let im_to_list_ext (m: answer list IM.t) to_cmp =
  match
    IM.fold (
      fun sid al (fopt, acc) ->
        match fopt with
        | Some _ ->
          fopt, (sid_to_solver sid, al) :: acc
        | None when sid = to_cmp ->
          Some (sid_to_solver sid, al), acc
        | None ->
          fopt, (sid_to_solver sid, al) :: acc
    ) m (None, [])
  with
  | Some x, l -> x, l
  | _ -> assert false

let mk_bi id exn stmtcs answers =
  { id; exn; stmtcs; answers; }

let exn_to_str = function
  | Unsoundness -> "Failure [Unsoundness]"
  | InternalCrash -> "Failure [Internal Crash]"
  | Timeout -> "Failure [Timeout]"
  | Other str ->
    Format.sprintf  "Failure [Other(%s)]" str
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
    id exn stmtcs answers =

  let exn_str = function
    | Unsoundness -> "unsoundness"
    | InternalCrash -> "internalcrash"
    | Timeout -> "timeout"
    | Stack_overflow -> "stackoverflow"
    | Out_of_memory -> "outofmemory"
    | _ -> "other"
  in
  let bi = mk_bi id (Some exn) stmtcs answers in
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
    exn stmtcs IM.empty

let cmp_answers m cmp_to =
  let (rs, ra), oans = im_to_list_ext m cmp_to in
  let _ =
    List.fold_left (
      fun ra (s, a) ->
        let la = List.length a in
        if ra = la
        then ra
        else
          failwith (
            Format.asprintf
              "The solvers %a and %a don't have the same number of answers\
               %d and %d"
              print_solver rs print_solver s ra la
          )
    ) (List.length ra) oans
  in
  let check_for_conflict ans1 ans2 =
    match ans1, ans2 with
    | Sat, Sat | Unsat, Unsat | Unknown, Unknown
    | Sat, Unknown | Unsat, Unknown
    | Unknown, Sat | Unknown, Unsat -> true
    | _ -> false
  in
  let _ =
    List.fold_left (
      fun oans ans ->
        List.map (
          fun (s, l) ->
            match l with
            | h :: t ->
              if check_for_conflict h ans
              then (s, t)
              else assert false
            | _ -> assert false
        ) oans
    ) oans ra
  in
  ()

let mk_str_mid l s =
  let sl = String.length s in
  if sl >= l
  then
    failwith (Format.sprintf "Field width(%d) larger than expected(%d)" sl l)
  else
    let slml = l - sl in
    let ld2,lm2 = slml / 2, slml mod 2 in
    Format.sprintf "%s%s%s"
      (String.init ld2 (fun _ -> ' ')) s
      (String.init (ld2 + lm2) (fun _ -> ' '))

let pp_answers =
  let _FIELD_WIDTH_ = 24 in
  let rec pp_aux (ll: answer list list) =
    if not (List.for_all (fun l -> List.length l = 0) ll) then
      let rll =
        List.fold_left (
          fun acc l ->
            match l with
            | h :: t ->
              Format.printf "%s" (
                match h with
                | Sat -> mk_str_mid _FIELD_WIDTH_ "Sat"
                | Unsat -> mk_str_mid _FIELD_WIDTH_ "Unsat"
                | Unknown -> mk_str_mid _FIELD_WIDTH_ "Unknown"
              );
              t :: acc
            | [] -> [] :: acc
        ) [] ll
      in
      Format.printf "@.";
      pp_aux (List.rev rll)
  in
  fun (_m: answer list IM.t) ->
    let bindings = IM.bindings _m in
    let rev_ans_ll =
      List.rev_map (
        fun (solv, ansl) ->
          Format.printf "%s" (
            mk_str_mid _FIELD_WIDTH_ @@
            Format.asprintf "%a" print_solver (sid_to_solver solv)
          );
          ansl
      ) bindings
    in
    Format.printf "@.";
    pp_aux (List.rev rev_ans_ll)

let cpu_count () =
  try match Sys.os_type with
    | "Win32" -> int_of_string (Sys.getenv "NUMBER_OF_PROCESSORS")
    | _ ->
      let i = Unix.open_process_in "getconf _NPROCESSORS_ONLN" in
      let close () = ignore (Unix.close_process_in i) in
      try
        Scanf.bscanf (Scanf.Scanning.from_channel i) "%d"
          (fun n -> close (); n)
      with e -> close (); raise e
  with
  | Not_found | Sys_error _ | Failure _ | Scanf.Scan_failure _
  | End_of_file | Unix.Unix_error (_, _, _) -> 1
