
module type T = Translater.T

module type ST =
sig 
  include T
  val process_decls : Ast.typedecl list -> Ast.decl list -> Utils.answer list
end 

module CVC5: ST

module AE: ST
