
module type T = 
sig
  type t
  val translate_stmt: Ast.stmt -> t 
  val print_stmts: 
    Format.formatter -> Ast.typedecl list * Ast.stmt list -> unit
end 
