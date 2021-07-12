
module type T = Translater.T

module type ST =
sig 
  include T
  val process_stmts : Ast.typedecl list -> Ast.stmt list -> Utils.answer list
end 

module CVC5: ST

module AE: ST
