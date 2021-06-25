
module Cr = Crowbar 

type ast_gen_res = 
  { gast : Ast.ast; 
    u_args : Ast.VS.t; 
    u_bvars : Ast.VS.t; 
    c_funcs : Ast.SS.t}

type decl_gen_res = 
  { gdecl : Ast.decl; 
    c_funcs : Ast.SS.t}

type declkind = (* declaration kind *) 
  | FD (* function declaration *)
  | AxD (* axiom declaration *)
  | GD (* goal declaration *)

val dk_gen : declkind Cr.gen

val pr_gar : Format.formatter -> ast_gen_res -> unit

val pr_fdi : Format.formatter -> Ast.fd_info -> unit

val get_gen : Ast.fd_info list -> declkind -> decl_gen_res Cr.gen

val generate_ast : 
  ?isform:bool -> ?qvars:bool -> ?args:Ast.tvar list -> 
  ?fdefs:Ast.fd_info list -> int -> Ast.typ -> ast_gen_res Cr.gen

val generate_decl : 
  ?fdefs:Ast.fd_info list -> ?name:string -> declkind -> decl_gen_res Cr.gen

val gen_decls : Ast.decl list Cr.gen
