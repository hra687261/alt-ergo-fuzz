
type answer = Sat | Unsat | Unknown

exception Unsoundness

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

val exn_to_str: exn -> string

val handle_unsoundness_bug:
  ?verbose:bool ->
  ?output_folder_path:string ->
  int ->
  exn ->
  Ast.stmt_c list ->
  answer list -> answer list ->
  answer list -> answer list ->
  answer list -> unit

val handle_failure_bug:
  ?verbose:bool ->
  ?output_folder_path:string ->
  int -> exn -> Ast.stmt_c list -> unit

val cmp_answers:
  answer list -> answer list ->
  answer list -> answer list ->
  answer list -> unit

(*
val pr_answers:
  answer list -> answer list ->
  answer list -> unit
*)
