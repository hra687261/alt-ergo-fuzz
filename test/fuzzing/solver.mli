
module type T = Translater.T

module type ST =
sig 
  include T
  val process_stmts : 
    ?debug:bool -> Ast.stmt_c list -> Utils.answer list
end 

module CVC5: ST

module AE_Tableaux: ST

module AE_CDCL: ST
