
module type S = 
sig 
  val get_exec_str : unit -> string
end 

module Z3 : S = 
struct 
  let get_exec_str () = "z3" 
end 

module CVC4 : S = 
struct 
  let get_exec_str () = "cvc4" 
end 

module CVC5 : S = 
struct 
  let get_exec_str () = "cvc5" 
end 
