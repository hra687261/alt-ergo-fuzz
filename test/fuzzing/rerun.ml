open Utils

module AES = Solver.AE
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

  let {stmtcs; exp_str; exp_bt_str; _}: bug_info = 
    Marshal.from_string str 0 
  in 

  Format.printf "\nException: %s\n%s@." exp_str exp_bt_str;
  Format.printf "\nCaused by: \n%a@." 
    ( fun fmt stmts ->
        List.iter ( 
          fun Ast.{stmt;_} ->
            Format.fprintf fmt "\n### %a@." Ast.print_stmt stmt;
        ) stmts
    ) stmtcs;

  let aeres = AES.process_stmts stmtcs in
  let c5res = C5S.process_stmts stmtcs in
  cmp_answers_pr2 aeres c5res
