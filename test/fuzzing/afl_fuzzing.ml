open Utils

module Cr = Crowbar 
module Tae = Tr_altergo
(*module Z3S = Smtlib2_solver.Make(Solvers.Z3)*)
module CVC5S = Smtlib2_solver.Make(Solvers.CVC5)


let proc decls = 
  try
    sh_printf "\n";
    incr cnt;
    let aeres = 
      Tae.process_decls decls
    in
    let cvc5res = 
      CVC5S.process_decls decls
    in
    cmp_answers_exn2 aeres cvc5res;
    true
  with
  | exp ->
    mknmarshall_bi exp decls; 
    false

let () =
  sh_printf ~firstcall:true ""; 
  Cr.add_test ~name:"ae" [Generator.gen_decls] 
    (fun decls -> Cr.check (proc decls))



