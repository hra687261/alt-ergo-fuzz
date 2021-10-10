open Utils

module Cr = Crowbar 

let cnt = ref 0 

let test_fun stmtcs = 
  Cr.check (
    try
      incr cnt;
      Solvers.call_cvc5 stmtcs;

      let ae_c = Solvers.solve_with_ae_c stmtcs in
      let ae_ct = Solvers.solve_with_ae_c stmtcs in
      let ae_t = Solvers.solve_with_ae_t stmtcs in
      let ae_tc = Solvers.solve_with_ae_t stmtcs in

      let cvc5 = Solvers.get_cvc5_response () in
      (* pr_answers ae_c ae_ct ae_t ae_tc cvc5; *)
      try
        cmp_answers ae_c ae_ct ae_t ae_tc cvc5;
        true
      with
      | exp ->
        handle_bug !cnt exp stmtcs ae_c ae_ct ae_t ae_tc cvc5;
        false
    with
    | exp ->
      handle_bug_na !cnt exp stmtcs;
      false
  )

let () =
  Cr.add_test ~name:"ae" [Generator.gen_stmts ()] test_fun
