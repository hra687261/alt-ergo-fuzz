open Utils

module Cr = Crowbar

let run_solvers stmtcs =
  let ansl = [] in
  Solvers.call_cvc5 stmtcs;
  let ansl = (AE_C, (Solvers.solve_with_ae_c stmtcs)) :: ansl in
  let ansl = (AE_CT, (Solvers.solve_with_ae_ct stmtcs)) :: ansl in
  let ansl = (AE_T, (Solvers.solve_with_ae_t stmtcs)) :: ansl in
  let ansl = (AE_TC, (Solvers.solve_with_ae_tc stmtcs)) :: ansl in
  let ansl = (CVC5, (Solvers.get_cvc5_response ())) :: ansl in
  mk_im ansl

let test_fun =
  let cnt = ref 0 in
  fun ?(verbose = true) stmtcs ->
    Cr.check (
      try
        incr cnt;
        let n_answers = run_solvers stmtcs in
        if verbose then pp_answers n_answers;
        try
          cmp_answers n_answers (solver_to_sid CVC5);
          true
        with
        | exp ->
          handle_unsoundness_bug ~verbose !cnt exp stmtcs n_answers;
          false
      with
      | exp ->
        handle_failure_bug !cnt exp stmtcs;
        false
    )
