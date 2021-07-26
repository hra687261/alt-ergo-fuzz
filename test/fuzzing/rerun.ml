open Utils

module AE_CDCL = Solver.AE_CDCL
module AE_Tableaux = Solver.AE_Tableaux
module C5S = Solver.CVC5

let () =
  if not (Array.length Sys.argv = 2)
  then
    failwith
      "Expected one argument:\n./rerun.exe path_to_file_containing_marshalled_bug_info@.";

  let file_name = Sys.argv.(1) in
  Format.printf "Reading from the file: %s@." file_name;

  let ic = open_in file_name in
  let str = really_input_string ic (in_channel_length ic) in
  close_in ic;

  let {stmtcs; exn; ae_c; ae_t; cvc5; _}: bug_info =
    Marshal.from_string str 0
  in

  begin match exn with 
    | None -> Format.printf "\nNo exception.@."
    | Some exn ->
      Format.printf "\nException: %s@." (exn_to_string exn);
      Format.printf "\nCaused by: \n%a@."
        ( fun fmt stmts ->
            List.iter (
              fun Ast.{stmt;_} ->
                Format.fprintf fmt "\n### %a@." Ast.print_stmt stmt;
            ) stmts
        ) stmtcs
  end;

  Format.printf "\nOriginal answers:@.";
  Format.printf "%d %d %d@."
    (List.length ae_c) 
    (List.length ae_t) 
    (List.length cvc5);
  cmp_answers_pr3 ae_c ae_t cvc5;

  Format.printf "\nRerunning answers:@.";
  let ae_cr = AE_CDCL.process_stmts stmtcs in
  let ae_tr = AE_Tableaux.process_stmts stmtcs in
  let c5rn = C5S.process_stmts stmtcs in

  Format.printf "%d %d %d@."
    (List.length ae_cr) 
    (List.length ae_tr) 
    (List.length c5rn);
  cmp_answers_pr3 ae_c ae_t cvc5;
  cmp_answers_pr3_exn ae_cr ae_tr c5rn
