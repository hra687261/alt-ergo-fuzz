open Utils

module Cr = Crowbar

let test_fun =
  let cnt = ref 0 in
  fun ?(verbose = false) stmtcs ->
    Cr.check (
      try
        incr cnt;
        let ansl = [] in
        Solvers.call_cvc5 stmtcs;

        let ansl = (AE_C, (Solvers.solve_with_ae_c stmtcs)) :: ansl in
        let ansl = (AE_CT, (Solvers.solve_with_ae_ct stmtcs)) :: ansl in
        let ansl = (AE_T, (Solvers.solve_with_ae_t stmtcs)) :: ansl in
        let ansl = (AE_TC, (Solvers.solve_with_ae_ct stmtcs)) :: ansl in

        let ansl = (CVC5, (Solvers.get_cvc5_response ())) :: ansl in
        let n_answers = mk_im ansl in
        if verbose || true then
          pp_answers n_answers;
        try
          cmp_answers n_answers (solver_to_sid CVC5);
          true
        with
        | exp ->
          handle_unsoundness_bug !cnt exp stmtcs n_answers;
          false
      with
      | exp ->
        handle_failure_bug !cnt exp stmtcs;
        false
    )

let () =
  Cr.add_test ~name:"ae" [Generator.stmts_gen ()] test_fun
