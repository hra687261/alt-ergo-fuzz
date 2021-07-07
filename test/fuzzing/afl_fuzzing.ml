open Utils

module Cr = Crowbar 
module AES = Solver.AE
module C5S = Solver.CVC5

let () =
  sh_printf ~firstcall:true ""; 
  Cr.add_test ~name:"ae" 
    [Generator.gen_decls] 
    ( fun (tydecls, decls) -> 
        Cr.check (
          try
            sh_printf "\n";
            incr cnt;
            let aeres = 
              AES.process_decls tydecls decls
            in
            let cvc5res = 
              C5S.process_decls tydecls decls
            in
            cmp_answers_exn2 aeres cvc5res;
            true
          with
          | exp ->
            mknmarshall_bi exp tydecls decls; 
            false
        )
    )



