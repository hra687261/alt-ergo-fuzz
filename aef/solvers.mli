
(* CVC5 *)

val call_cvc5: ?timeout:int -> Ast.stmt_c list -> unit

val get_cvc5_response: unit -> Utils.answer list


(* Alt-Ergo *)

val run_with_ae_t: Ast.stmt_c list -> Utils.answer list

val run_with_ae_tc: Ast.stmt_c list -> Utils.answer list

val run_with_ae_c: Ast.stmt_c list -> Utils.answer list

val run_with_ae_ct: Ast.stmt_c list -> Utils.answer list
