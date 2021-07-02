
module type T =
sig 
  val write : string -> string -> unit  
end 

module Make(Tr: Solver.ST): T