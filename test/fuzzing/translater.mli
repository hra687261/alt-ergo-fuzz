
module type T = 
sig
  type t
  val translate_stmt: Ast.stmt -> t 
  val print_stmts: 
    Format.formatter -> Ast.stmt_c list -> unit
end 
