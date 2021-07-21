open Utils

module Cr = Crowbar 
module AE_CDCL = Solver.AE_CDCL
module AE_Tableaux = Solver.AE_Tableaux
module C5S = Solver.CVC5

let cnt = ref 0 

let () =
  let l = ref [] in 
  Cr.add_test ~name:"ae" 
    [Generator.gen_stmts] 
    ( fun stmtcs -> 
        Cr.check (
          try
            incr cnt;
            let ae_cr = AE_CDCL.process_stmts stmtcs in
            let ae_tr = AE_Tableaux.process_stmts stmtcs in
            let cvc5 = C5S.process_stmts stmtcs in
            cmp_answers_exn3 ae_cr ae_tr cvc5;
            l := !l @ [mk_bi_success !cnt ae_cr ae_tr cvc5 stmtcs];
            true
          with
          | exp ->
            l := !l @ [mk_bi_empty !cnt exp stmtcs];
            mknmarshall_stmt_cache !l;
            l := [];
            false
        )
    )

