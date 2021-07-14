open Utils

module Cr = Crowbar 
module AES = Solver.AE
module C5S = Solver.CVC5

let () =
  sh_printf ~firstcall:true ""; 
  Cr.add_test ~name:"ae" 
    [Generator.gen_stmts] 
    ( fun stmtcs -> 
        Cr.check (
          try
            sh_printf "\n";
            incr cnt;
            let aeres = 
              AES.process_stmts stmtcs
            in
            let cvc5res = 
              C5S.process_stmts stmtcs
            in
            cmp_answers_exn2 aeres cvc5res;
            true
          with
          | exp ->
            mknmarshall_bi exp stmtcs; 
            false
        )
    )



