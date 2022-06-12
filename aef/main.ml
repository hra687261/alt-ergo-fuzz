open Utils

module Run = struct
  let cmd =
    let open Cmdliner in
    let aux verbose =
      Crowbar.add_test
        ~name:"alt-ergo-fuzz" [Generator.stmts_gen ()]
        (Common.test_fun ~verbose)
    in
    let verbose =
      Arg.(value & flag & info ["v";"verbose"]
             ~doc:"Set verbose printing to true")
    in
    let doc =
      "Run AEF with AFL or in quickcheck mode"
    in
    Cmd.v (Cmd.info ~doc "run")
      (Term.(const aux $ verbose))
end

module Rerun = struct
  let cmd =
    let open Cmdliner in
    let aux verbose ipfl =
      match ipfl with
      | [] ->
        failwith "rerun: provide the path to at least one file containig \
                  hashconsed bug info to reproduce"
      | _ ->
        let bil = List.map get_bug_info ipfl in
        List.iter (Rerun.rerun ~verbose) bil
    in
    let verbose =
      Arg.(value & flag & info ["v";"verbose"]
             ~doc:"Set verbose printing to true")
    and ipfl =
      Arg.(value & pos_all string [] & info []
             ~doc:"Path to a file containing marshalled information about a \
                   detected bug")
    in
    let doc =
      "Rerun AEF to reproduce a detected bug"
    in
    Cmd.v (Cmd.info ~doc "rerun")
      (Term.(const aux $ verbose $ ipfl))
end

module Translate = struct
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

  let output_lang_list = [
    "ae", Native;
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

  let output_lang_conv =
    let open Cmdliner in
    Arg.conv (
      output_lang_parser,
      pp_output_lang
    )

  let cmd =
    let open Cmdliner in
    let aux ipf_opt opf_opt opl_opt =
      match ipf_opt with
      | None -> assert false
      | Some ipf ->
        let opl =
          match opl_opt with
          | Some Smtlib2 -> Smtlib2
          | Some Native | None -> Native
        in
        let opf =
          match opf_opt with
          | Some opf -> opf
          | None ->
            Filename.remove_extension ipf ^
            (match opl with Smtlib2 -> ".smt2" | Native -> ".ae")
        in
        let bi = get_bug_info ipf in
        translate_and_write bi.stmtcs opl opf
    in
    let ipf_opt =
      Arg.(value & pos 0 (some string) None & info [] ~docv:"PATH"
             ~doc:"Path to a file containing marshalled information about a \
                   detected bug.")
    and opf_opt =
      Arg.(value & opt (some string) None  & info ["o";"output"]
             ~doc:"Path to the destination file in which to store the
             translated smt statements.")
    and trlang =
      let doc = Format.sprintf
          "Set the output format to %s, default is native."
          (Arg.doc_alts (fst @@ List.split output_lang_list))
      in
      Arg.(
        value &
        opt (some output_lang_conv) None &
        info ["t"; "translate"] ~doc
      )
    in
    let doc =
      "Translate a file containig hashconsed bug information into a \".ae\" \
       or \".smt2\" file containig the smt statement that caused the bug."
    in
    Cmd.v (Cmd.info ~doc "translate")
      (Term.(const aux $ ipf_opt $ opf_opt $ trlang))
end

let parse_opt () =
  let open Cmdliner in
  let default, info =
    let doc = "A fuzzer for the Alt-Ergo SMT solver." in
    let man = [
      `S "DESCRIPTION";
      `P "$(b,alt-ergo-fuzz) is a tool to automatically generate tests cases \
          on which to run the Alt-Ergo smt solver.";
      `S "COMMANDS";
      `S "OPTIONS";
    ] in
    let def = Term.(const (fun () -> `Help (`Pager, None)) $ const ()) in
    Term.(ret def),
    Cmd.info ~version:"dev" ~man ~doc "alt-ergo-fuzz"
  in
  let cmds = [
    Run.cmd;
    Rerun.cmd;
    Translate.cmd;
  ] in
  let v = Cmd.eval_value (Cmd.group info ~default cmds) in
  v

let () =
  match parse_opt () with
  | Error (`Parse | `Term | `Exn) -> exit 2
  | Ok (`Ok () | `Version | `Help) -> ()