open Utils

module AE_CDCL = Solver.AE_CDCL
module AE_Tableaux = Solver.AE_Tableaux
module C5S = Solver.CVC5

let () =
  if not (Array.length Sys.argv = 2)
  then
    failwith
      "Expected one argument:\n./rerun_all_cached.exe path_to_file_containing_marshalled_stmt_cache@.";

  let file_name = Sys.argv.(1) in
  let ic = open_in file_name in
  let str = really_input_string ic (in_channel_length ic) in
  close_in ic;
  let bil: stmt_cache =
    Marshal.from_string str 0
  in
  let cpt = ref 0 in 
  List.iter (
    fun {ae_c; ae_t; cvc5; stmtcs; exp_str; exp_bt_str;_} -> 
      Format.printf 
        "\n######################################### %d@." 
        !cpt; 
      incr cpt;
      if exp_str <> "" then (
        Format.printf 
          "Original exception: \nFatal error: exception %s\n%s@."
          exp_str exp_bt_str;
      );
      Format.printf "Original answers: ";
      Format.printf "%d %d %d@."
        (List.length ae_c) 
        (List.length ae_t) 
        (List.length cvc5);
      try
        cmp_answers_pr3_exn ae_c ae_t cvc5;

        let ae_cr = AE_CDCL.process_stmts stmtcs in
        let ae_tr = AE_Tableaux.process_stmts stmtcs in
        let c5rn = C5S.process_stmts stmtcs in

        Format.printf "\nRerunning answers:@.";
        cmp_answers_pr3_exn ae_cr ae_tr c5rn
      with exp -> 
        let exp_str = Printexc.to_string_default exp in 
        Format.printf "Fatal error: exception %s@." exp_str
  ) bil 
