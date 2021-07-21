open Utils

module Cr = Crowbar 
module AE_CDCL = Solver.AE_CDCL
module AE_Tableaux = Solver.AE_Tableaux
module C5S = Solver.CVC5

let () =
  Cr.add_test ~name:"ae" 
    [Generator.gen_stmts] 
    ( fun stmtcs -> 
        Cr.check (
          try
            incr cnt;
            let ae_cdcl_res = 
              AE_CDCL.process_stmts stmtcs
            in
            let ae_t_res = 
              AE_Tableaux.process_stmts stmtcs
            in
            let cvc5res = 
              C5S.process_stmts stmtcs
            in
            cmp_answers_exn3 ae_t_res ae_cdcl_res cvc5res;
            true
          with
          | exp ->
            mknmarshall_bi_na exp stmtcs; 
            false
        )
    )



