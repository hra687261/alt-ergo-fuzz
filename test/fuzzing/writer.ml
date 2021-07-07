
module type T =
sig 
  val write : string -> string -> unit  
end 

module Make(Tr: Solver.ST) : T =
struct 
  let write srcmf destf =

    let ic = open_in srcmf in 
    let str = really_input_string ic (in_channel_length ic) in
    close_in ic;

    let {tydecls; decls; _} : Utils.bug_info = 
      Marshal.from_string str 0
    in 
    let oc = open_out destf in 
    let fmt = Format.formatter_of_out_channel oc in
    Format.fprintf fmt "%a" Tr.print_decls (tydecls, decls);
    close_out oc

end 