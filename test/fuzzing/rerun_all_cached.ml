open Utils

let () =
  if not (Array.length Sys.argv = 2)
  then
    failwith
      "Expected one argument:\n./rerun_all_cached.exe path_to_file_containing_marshalled_stmt_cache@.";

  let if_path = Sys.argv.(1) in
  let ic = open_in if_path in
  let str = really_input_string ic (in_channel_length ic) in
  close_in ic;

  let bil: stmt_cache = Marshal.from_string str 0 in
  let cpt = ref 0 in 

  List.iter (
    fun {exn; stmtcs; ae_c; ae_t; cvc5; _} -> 

      Format.printf
        "\n######################################### %d@." !cpt;
      incr cpt;
      begin match exn with 
        | None -> ()
        | Some exn ->
          Format.printf "\nException: %s@." (exn_to_string exn)
          (*;
            Format.printf "\nCaused by: \n%a@."
            ( fun fmt stmts ->
                List.iter (
                  fun Ast.{stmt;_} ->
                    Format.fprintf fmt "\n### %a@." Ast.print_stmt stmt;
                ) stmts
            ) stmtcs*)
      end;
      Format.printf "Original answers: ";
      Format.printf "%d %d %d@."
        (List.length ae_c) 
        (List.length ae_t) 
        (List.length cvc5);
      cmp_answers_pr3 ae_c ae_t cvc5;

      Format.printf "Rerunning answers: ";
      try
        Solvers.call_cvc5 stmtcs;
        let ae_cr = Solvers.run_with_ae_c stmtcs in
        let ae_tr = Solvers.run_with_ae_t stmtcs in
        let c5_r = Solvers.get_cvc5_response () in

        Format.printf "%d %d %d@."
          (List.length ae_cr) 
          (List.length ae_tr) 
          (List.length c5_r);

        try
          cmp_answers_exn3 ae_cr ae_tr c5_r; 
          cmp_answers_pr3 ae_cr ae_tr c5_r;
        with exp -> 
          cmp_answers_pr3 ae_cr ae_tr c5_r;
          Format.printf "\nException: %s@." (exn_to_string exp)
      with exp -> 
        Format.printf "\nException: %s@." (exn_to_string exp)
  ) bil 
