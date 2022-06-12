open Utils
open Cmdliner

type output_lang = Native | Smtlib2

let output_lang_list =
  [ "ae", Native;
    "native", Native;
    "altergo", Native;
    "alt-ergo", Native;
    "sl2", Smtlib2;
    "smtlib2", Smtlib2;
    "smt-lib2", Smtlib2;
  ]

let output_lang_parser s =
  try
    Ok (List.assoc s output_lang_list)
  with
    Not_found ->
    let msg =
      Format.sprintf
        "The output language %s is not supported" s
    in
    Error (`Msg msg)

let output_lang_printer fmt opl =
  match opl with
  | Native -> Format.fprintf fmt "native"
  | Smtlib2 -> Format.fprintf fmt "smtlib2"

let output_lang_conv =
  Arg.conv (
    output_lang_parser,
    output_lang_printer
  )

let cmd_line_args =
  let rerun =
    let doc = "Reruns the solvers on the provided hashconsed file." in
    Arg.(value & flag & info ["r"; "rerun"] ~doc)
  in
  let trfile =
    let doc = Format.sprintf
        "Set the output format to %s."
        (Arg.doc_alts (fst @@ List.split output_lang_list)) in
    Arg.(
      value &
      opt (some output_lang_conv) None &
      info ["t"; "translate"] ~doc
    )
  in
  let input_file =
    let doc = "The hashconsed input file" in
    Arg.(value & opt (some string) None & info ["i"; "input"] ~doc)
  in
  let output_file =
    let doc =
      "The output file in which the translated statements will be printed"
    in
    Arg.(value & opt (some string) None & info ["o"; "output"] ~doc)
  in
  let verbose =
    let doc = "Output information about what is done." in
    Arg.(value & flag & info ["v"; "verbose"] ~doc)
  in
  let f rr tr ipf opf vrb =
    `Ok (rr, tr, ipf, opf, vrb)
  in
  Term.(
    ret (
      const f $
      rerun $
      trfile $
      input_file $
      output_file $
      verbose
    )
  )

let get_bug_info ipf =
  let ic = open_in ipf in
  let str = really_input_string ic (in_channel_length ic) in
  close_in ic;
  (Marshal.from_string str 0: bug_info)

let translate_and_write stmtcs opl destf =
  let module Tr = (
    val (
      match opl with
      | Native -> (module Tr_altergo)
      | Smtlib2 -> (module Smtlib2_tr)
    ): Translater.T
  )
  in

  let oc = open_out destf in
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "%a" Tr.print_stmts stmtcs;
  close_out oc

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

  let ansl = [] in
  Solvers.call_cvc5 stmtcs;

  let ansl = (AE_C, (Solvers.solve_with_ae_c stmtcs)) :: ansl in
  let ansl = (AE_CT, (Solvers.solve_with_ae_ct stmtcs)) :: ansl in
  let ansl = (AE_T, (Solvers.solve_with_ae_t stmtcs)) :: ansl in
  let ansl = (AE_TC, (Solvers.solve_with_ae_tc stmtcs)) :: ansl in

  let ansl = (CVC5, (Solvers.get_cvc5_response ())) :: ansl in
  let n_answers = mk_im ansl in
  if verbose then begin
    Format.printf "\nRerunning answers:@.";
    pp_answers n_answers
  end;
  cmp_answers n_answers (solver_to_sid CVC5)

let () =
  let open Cmdliner in
  let i = Cmd.info "alt-ergo-fuzz" in
  match Cmd.eval_value (Cmd.v i cmd_line_args) with
  | Ok (`Ok (true, None, Some ipf, None, verbose)) ->
    (* rerun *)
    begin
      try
        rerun ~verbose (get_bug_info ipf)
      with exn ->
        Format.printf "Rerunning failure:\n%s@." (exn_to_str exn)
    end

  | Ok (`Ok (false, Some opl, Some ipf, Some opf, _)) ->
    (* translate *)
    let bi = get_bug_info ipf in
    translate_and_write bi.stmtcs opl opf

  | Ok (`Ok (true, Some opl, Some ipf, Some opf, verbose)) ->
    (* rerun and translate *)
    let bi = get_bug_info ipf in
    begin
      try
        rerun ~verbose bi
      with exn ->
        Format.printf "Rerunning failure:\n%s@." (exn_to_str exn)
    end;
    translate_and_write bi.stmtcs opl opf

  | Ok (`Ok (false, None, None, None, verbose)) ->
    (* run the fuzzing loop *)
    Crowbar.add_test
      ~name:"ae" [Generator.stmts_gen ()] (Common.test_fun ~verbose)

  | Ok `Version | Ok `Help -> exit 0
  | Error `Parse -> exit Cmd.Exit.cli_error
  | Error `Term -> exit Cmd.Exit.internal_error
  | Error `Exn -> exit Cmd.Exit.internal_error
  | _ ->
    assert false

