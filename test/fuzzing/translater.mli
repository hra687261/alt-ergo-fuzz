
module type T = 
sig
  type t
  val translate_decl: Ast.decl -> t 
  val print_decls: 
    Format.formatter -> Ast.typedecl list * Ast.decl list -> unit
end 
