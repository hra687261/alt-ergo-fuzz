module Cr = Crowbar

(** ['a gen_res] A container for a generated value of type ['a].*)
type 'a gen_res = {
  g_res : 'a;
  (** [g_res] the generated value *)

  u_bvars : Ast.VS.t;
  (** [u_bvars] used bound variables: a set of the bound variables that were
      used in the generated value *)

  u_dt : Ast.SS.t;
  (** [u_dt] used data types: a set of the names of the used user-defined
      data types that were used in the generated value *)

  u_us : Ast.SS.t Ast.TCM.t;
  (** [u_us] used uninterpreted symbols: a map associating sets of the names
      of the used uninterpreted symbols, both variables and functions,
      to their types *)

  c_funcs : Ast.SS.t
  (** [c_funcs] called functions: a set of the names of the user-defined
      functions that were called in the generated value *)
}

(** [dk_gen] A statement-kind generator *)
val dk_gen : Ast.stmtkind Cr.gen

(** [pr_gr grpr ppf gr] prints a ['a gen_res] *)
val pr_gr : (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a gen_res -> unit

(** [expr_gen ?uqvars ?args ?fdefs ?tydecls fuel ty] generates an expression of
    type [ty], and of maximum depth [fuel]. [uqvars] determines whether or not
    the expression can contain quantifiers, [args] is a set of usable bound
    variables, [fdefs] is a set of usable generated functions and [tydecls] is
    a set of generated type declarations.
*)
val expr_gen :
  ?uqvars:bool ->
  ?args:Ast.tvar list ->
  ?fdefs:Ast.fd_info list ->
  ?tydecls:Ast.typedecl list ->
  int -> Ast.ty -> Ast.expr gen_res Cr.gen

(** [stmt_gen ?fdefs ?tydecls ?name sk] generates a statement of the kind [sk],
    named with the value of [name] and where [fdefs] is a set of usable
    generated functions and [tydecls] is a set of generated type declarations.
*)
val stmt_gen :
  ?fdefs:Ast.fd_info list -> ?tydecls:Ast.typedecl list ->
  ?name:string -> Ast.stmtkind -> Ast.stmt gen_res Cr.gen

(** Generates and sets the generation options then generates a list of type
    declarations and statements which can use the generated type declarations
*)
val stmts_gen : unit -> Ast.stmt_c list Cr.gen
