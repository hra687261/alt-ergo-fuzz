
module Cr = Crowbar 

type 'a gen_res = {
  g_res : 'a;
  u_args : Ast.VS.t; 
  u_bvars : Ast.VS.t; 
  u_dt : Ast.SS.t;
  c_funcs : Ast.SS.t}

val dk_gen : Ast.stmtkind Cr.gen

val pr_gr : (Format.formatter -> 'a -> unit) -> 
  Format.formatter -> 'a gen_res -> unit

val get_gen : 
  Ast.fd_info list -> Ast.stmtkind -> Ast.stmt gen_res Cr.gen

val generate_expr : 
  ?isform:bool -> ?qvars:bool -> ?args:Ast.tvar list -> 
  ?fdefs:Ast.fd_info list -> 
  ?adts:Ast.adt list -> 
  int -> Ast.typ -> Ast.expr gen_res Cr.gen

val generate_stmt : 
  ?fdefs:Ast.fd_info list -> ?adts:Ast.adt list -> 
  ?name:string -> Ast.stmtkind -> Ast.stmt gen_res Cr.gen

val gen_stmts : (Ast.typedecl list * Ast.stmt list) Cr.gen
