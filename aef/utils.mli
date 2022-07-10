
(* Command line utils *)

type answer = Sat | Unsat | Unknown

type solver =
  | AE_C
  | AE_CT
  | AE_T
  | AE_TC
  | CVC5

type output_lang = Native | Smtlib2

val pp_output_lang: Format.formatter -> output_lang -> unit

type terminal = GnomeTerminal | XTerm | Konsole

val pp_terminal: Format.formatter -> terminal -> unit

(* Helpers *)

val terminal_wrap_cmd: terminal -> string -> string

val (<+>): 'a option -> 'a -> 'a

val opt_app: ('a -> 'b) -> default:'b -> 'a option -> 'b

val opt_app_fmt: ('a -> unit, Format.formatter, unit) format ->
  Format.formatter -> 'a option -> unit

(* Testing utils *)

exception Unsoundness
exception InternalCrash
exception Timeout
exception Other of string

module IM: Map.S with type key = int

type bug_info = {
  id: int;
  exn: exn option;
  stmtcs: Ast.stmt_c list;
  answers: answer list IM.t;
}

val get_bug_info: string -> bug_info

val solver_to_sid : solver -> int

val mk_im: (solver * answer list) list -> answer list IM.t

val im_to_list: answer list IM.t -> (solver * answer list) list

val exn_to_str: exn -> string

val handle_unsoundness_bug: ?verbose:bool -> ?output_folder_path:string ->
  int -> exn -> Ast.stmt_c list -> answer list IM.t -> unit

val handle_failure_bug: ?verbose:bool -> ?output_folder_path:string -> int ->
  exn -> Ast.stmt_c list -> unit

val cmp_answers: answer list IM.t -> int -> unit

val pp_answers: answer list IM.t -> unit

val cpu_count: unit -> int
