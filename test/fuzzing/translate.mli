

type answer = 
  | Sat | Unsat | Unknown

module type T = 
sig
  type t
  val print_decls: Format.formatter -> Ast.decl list -> unit
  val process_decls: Ast.decl list -> answer list
end 
