
module Cr = Crowbar 

type 'a gen_res = {
  g_res : 'a;
  u_args : Ast.VS.t; 
  u_bvars : Ast.VS.t; 
  u_dt : Ast.SS.t;
  c_funcs : Ast.SS.t}

type declkind = (* declaration kind *) 
  | FD (* function declaration *)
  | AxD (* axiom declaration *)
  | GD (* goal declaration *)

val dk_gen : declkind Cr.gen

val pr_gr : (Format.formatter -> 'a -> unit) -> 
  Format.formatter -> 'a gen_res -> unit

val get_gen : Ast.fd_info list -> declkind -> Ast.decl gen_res Cr.gen

val generate_ast : 
  ?isform:bool -> ?qvars:bool -> ?args:Ast.tvar list -> 
  ?fdefs:Ast.fd_info list -> 
  ?adts:Ast.adt list -> 
  int -> Ast.typ -> Ast.ast gen_res Cr.gen

val generate_decl : 
  ?fdefs:Ast.fd_info list -> ?adts:Ast.adt list -> 
  ?name:string -> declkind -> Ast.decl gen_res Cr.gen

val gen_decls : (Ast.typedecl list * Ast.decl list) Cr.gen
