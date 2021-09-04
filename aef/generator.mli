
module Cr = Crowbar 

type 'a gen_res = {
  g_res : 'a;
  u_args : Ast.VS.t; 
  u_bvars : Ast.VS.t; 
  u_dt : Ast.SS.t;
  u_us : Ast.SS.t Ast.TCM.t;
  c_funcs : Ast.SS.t}

val dk_gen : Ast.stmtkind Cr.gen

val pr_gr : (Format.formatter -> 'a -> unit) -> 
  Format.formatter -> 'a gen_res -> unit

val expr_gen : 
  ?isform:bool -> ?uqvars:bool -> 
  ?args:Ast.tvar list -> 
  ?fdefs:Ast.fd_info list -> 
  ?tydecls:Ast.typedecl list -> 
  int -> Ast.typ -> Ast.expr gen_res Cr.gen

val stmt_gen : 
  ?fdefs:Ast.fd_info list -> ?tydecls:Ast.typedecl list -> 
  ?name:string -> Ast.stmtkind -> Ast.stmt gen_res Cr.gen

val gen_stmts : 
  ?nb_tds:int -> ?nb_dks:int -> unit -> Ast.stmt_c list Cr.gen
