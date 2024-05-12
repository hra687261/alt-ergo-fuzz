open Utils

let rerun ?(verbose = false)
    {stmtcs; exn; answers; _ } =

  if verbose then begin
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
    pp_answers answers;
  end;

  let n_answers = Common.run_solvers stmtcs in
  if verbose then begin
    Format.printf "\nRerunning answers:@.";
    pp_answers n_answers
  end;
  cmp_answers n_answers (solver_to_sid CVC5)
(* TODO: instead of throwing an exception, catch it, and print it out. *)
