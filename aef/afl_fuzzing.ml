open Utils

module Cr = Crowbar 

let cnt = ref 0 

let () =
  Cr.add_test ~name:"ae" 
    [Generator.gen_stmts ()] 
    ( fun stmtcs -> 
        Cr.check (
          try
            incr cnt;
            Solvers.call_cvc5 stmtcs;
            let ae_cr = Solvers.solve_with_ae_c stmtcs in
            let ae_tr = Solvers.solve_with_ae_t stmtcs in
            let cvc5 = Solvers.get_cvc5_response () in
            pr_answers ae_cr ae_tr cvc5;
            try
              cmp_answers ae_cr ae_tr cvc5;
              true
            with
            | exp ->
              handle_bug !cnt exp stmtcs ae_cr ae_tr cvc5;
              false
          with
          | exp ->
            handle_bug_na !cnt exp stmtcs;
            false
        )
    )
