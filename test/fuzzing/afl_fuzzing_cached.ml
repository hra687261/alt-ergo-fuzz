open Utils

module Cr = Crowbar 
module AE_CDCL = Solver.AE_CDCL
module AE_Tableaux = Solver.AE_Tableaux
module C5S = Solver.CVC5

let () =
  let l = ref [] in 
  Cr.add_test ~name:"ae" 
    [Generator.gen_stmts] 
    ( fun stmtcs -> 
        Cr.check (
          try
            incr cnt;
            let ae_c = 
              AE_CDCL.process_stmts stmtcs
            in
            let ae_t = 
              AE_Tableaux.process_stmts stmtcs
            in
            let c5 = 
              C5S.process_stmts stmtcs
            in
            cmp_answers_exn3 ae_t ae_c c5; 
            l := !l @ [mk_bi_success ae_c ae_t c5 stmtcs];
            true
          with
          | exp ->
            l := !l @ [mk_bi_empty exp stmtcs];
            mknmarshall_stmt_cache !l;
            l := [];
            false
        )
    )

