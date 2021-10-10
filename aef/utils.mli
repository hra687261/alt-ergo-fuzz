type answer = Sat | Unsat | Unknown

exception Unsoundness

exception InternalCrash

exception Timeout

exception Other of string

type bug_info = {
  id : int;
  exn : exn option;
  stmtcs : Ast.stmt_c list;
  ae_c : answer list;
  ae_t : answer list;
  cvc5 : answer list;
}

val exn_to_string : exn -> string

val mknmarshall_bi :
  ?verbose:bool ->
  ?output_folder_path:string ->
  int ->
  exn ->
  Ast.stmt_c list ->
  answer list -> answer list -> answer list -> unit

val mknmarshall_bi_na :
  ?verbose:bool ->
  ?output_folder_path:string ->
  int -> exn -> Ast.stmt_c list -> unit

val cmp_answers_exn3 : answer list -> answer list -> answer list -> unit

val cmp_answers_pr3 : answer list -> answer list -> answer list -> unit
