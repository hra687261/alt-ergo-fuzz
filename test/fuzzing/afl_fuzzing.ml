open Utils

module Cr = Crowbar 

let cnt = ref 0 

let () =
  Cr.add_test ~name:"ae" 
    [Generator.gen_stmts] 
    ( fun stmtcs -> 
        Cr.check (
          try
            incr cnt;
            Solvers.call_cvc5 stmtcs;
            let ae_cr = Solvers.run_with_ae_c stmtcs in
            let ae_tr = Solvers.run_with_ae_t stmtcs in
            let cvc5 = Solvers.get_cvc5_response () in

            try  
              cmp_answers_pr3 ae_cr ae_tr cvc5;
              cmp_answers_exn3 ae_cr ae_tr cvc5;
              true
            with
            | exp ->
              mknmarshall_bi !cnt exp stmtcs ae_cr ae_tr cvc5; 
              false
          with
          | exp ->
            mknmarshall_bi_na !cnt exp stmtcs; 
            false
        )
    )



