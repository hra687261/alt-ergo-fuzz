open Utils

module AE_CDCL = Solver.AE_CDCL
module AE_Tableaux = Solver.AE_Tableaux
module C5S = Solver.CVC5

let () =
  if not (Array.length Sys.argv = 2)
  then
    failwith
      "Expected one argument:\n./rerun_last_cached.exe path_to_file_containing_marshalled_stmt_cache@.";

  let file_name = Sys.argv.(1) in
  let ic = open_in file_name in
  let str = really_input_string ic (in_channel_length ic) in
  close_in ic;
  let bis: stmt_cache =
    Marshal.from_string str 0
  in

  let {ae_c; ae_t; cvc5; stmtcs; exp_str; exp_bt_str; _} = 
    List.hd (List.rev bis) 
  in  

  Format.printf 
    "\n######################################### %d@." 
    (List.length bis - 1);
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
  cmp_answers_pr3 ae_c ae_t cvc5;

  let c5rn = C5S.process_stmts stmtcs in
  let ae_cr = AE_CDCL.process_stmts stmtcs in
  let ae_tr = AE_Tableaux.process_stmts stmtcs in

  Format.printf "\nRerunning answers:@.";
  cmp_answers_pr3_exn ae_cr ae_tr c5rn
