open Ast

module Cr = Crowbar 

module FCS : Set.S with type elt = string

type ast_gen_res = 
  { gast : ast; 
    u_args : VS.t; 
    u_bvars : VS.t; 
    c_funcs : FCS.t}

type decl_gen_res = 
  { gdecl : decl; 
    c_funcs : FCS.t}

type declkind = (* declaration kind *) 
  | FD (* function declaration *)
  | AxD (* axiom declaration *)
  | GD (* goal declaration *)

val dk_gen : declkind Cr.gen

val pr_gar : Format.formatter -> ast_gen_res -> unit

val pr_fdi : Format.formatter -> fd_info -> unit

val get_gen : fd_info list -> declkind -> decl_gen_res Cr.gen

val generate_ast : 
  ?isform:bool -> ?qvars:bool -> ?args:tvar list -> ?fdefs:fd_info list -> 
  int -> typ -> ast_gen_res Cr.gen

val generate_decl : 
  ?fdefs:fd_info list -> ?name:string -> declkind -> decl_gen_res Cr.gen

val gen_decls : decl list Cr.gen
