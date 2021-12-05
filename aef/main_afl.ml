open Utils

module Cr = Crowbar

let test_fun =
  let cnt = ref 0 in
  fun ?(verbose = false) stmtcs ->
    Cr.check (
      try
        incr cnt;
        Solvers.call_cvc5 stmtcs;

        let ae_c = Solvers.solve_with_ae_c stmtcs in
        let ae_ct = Solvers.solve_with_ae_c stmtcs in
        let ae_t = Solvers.solve_with_ae_t stmtcs in
        let ae_tc = Solvers.solve_with_ae_t stmtcs in

        let cvc5 = Solvers.get_cvc5_response () in
        if verbose || true then
          pp_answers ae_c ae_ct ae_t ae_tc cvc5;
        try
          cmp_answers ae_c ae_ct ae_t ae_tc cvc5;
          true
        with
        | exp ->
          handle_unsoundness_bug !cnt exp stmtcs ae_c ae_ct ae_t ae_tc cvc5;
          false
      with
      | exp ->
        handle_failure_bug !cnt exp stmtcs;
        false
    )

let () =
  Cr.add_test ~name:"ae" [Generator.stmts_gen ()] test_fun
