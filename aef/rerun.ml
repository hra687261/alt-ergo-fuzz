open Utils

let () =
  if not (Array.length Sys.argv = 2)
  then
    failwith
      "Expected one argument:\n./rerun.exe path_to_file_containing_marshalled_bug_info@.";

  let if_name = Sys.argv.(1) in
  Format.printf "Reading from the file: %s@." if_name;

  let ic = open_in if_name in
  let str = really_input_string ic (in_channel_length ic) in
  close_in ic;

  let {stmtcs; exn; ae_c; ae_ct; ae_t; ae_tc; cvc5; _}: bug_info =
    Marshal.from_string str 0
  in

  begin match exn with 
    | None -> Format.printf "\nNo exception.@."
    | Some exn ->
      Format.printf "\nException: %s@." (exn_to_str exn);
      Format.printf "\nCaused by: \n%a@."
        ( fun fmt stmts ->
            List.iter (
              fun Ast.{stmt;_} ->
                Format.fprintf fmt "\n### %a@." Ast.print_stmt stmt;
            ) stmts
        ) stmtcs
  end;

  Format.printf "\nOriginal answers:@.";
  Format.printf "%d %d %d %d %d@."
    (List.length ae_c) 
    (List.length ae_ct) 
    (List.length ae_t) 
    (List.length ae_tc) 
    (List.length cvc5);
  (* pr_answers ae_c ae_t cvc5; *)

  Format.printf "\nRerunning answers:@.";
  Solvers.call_cvc5 stmtcs;

  let ae_c = Solvers.solve_with_ae_c stmtcs in
  let ae_ct = Solvers.solve_with_ae_c stmtcs in
  let ae_t = Solvers.solve_with_ae_t stmtcs in
  let ae_tc = Solvers.solve_with_ae_t stmtcs in

  let cvc5 = Solvers.get_cvc5_response () in

  Format.printf "%d %d %d %d %d@."
    (List.length ae_c) 
    (List.length ae_ct) 
    (List.length ae_t) 
    (List.length ae_tc) 
    (List.length cvc5);
  (* pr_answers ae_c ae_ct ae_t ae_tc c5_r; *)
  cmp_answers ae_c ae_ct ae_t ae_tc cvc5
