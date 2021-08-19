open Utils

module Cr = Crowbar 

let cnt = ref 0 

let () =
  let l = ref [] in 
  Cr.add_test ~name:"ae" 
    [Generator.gen_stmts] 
    ( fun stmtcs -> 
        Cr.check (
          try
            incr cnt;
            Solvers.call_cvc5 stmtcs;
            let ae_cr = Solvers.run_with_ae_c stmtcs in
            let ae_tr = Solvers.run_with_ae_t stmtcs in
            let c5_r = Solvers.get_cvc5_response () in
            try
              cmp_answers_exn3 ae_cr ae_tr c5_r;
              l := !l @ [mk_bi_success !cnt stmtcs ae_cr ae_tr c5_r];
              true
            with
            | exn ->
              l := !l @ [mk_bi_empty !cnt exn stmtcs];
              mknmarshall_stmt_cache exn !l;
              l := [];
              false
          with
          | exn ->
            l := !l @ [mk_bi_empty !cnt exn stmtcs];
            mknmarshall_stmt_cache exn !l;
            l := [];
            false
        )
    )

