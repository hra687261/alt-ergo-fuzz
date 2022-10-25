open Utils

module Fuzz = struct

  let term_list = [
    "gnome-terminal", GnomeTerminal;
    "xterm", XTerm;
    "konsole", Konsole;
  ]

  let terminal_parser str =
    try
      Ok (List.assoc str term_list)
    with
      Not_found ->
      Error (`Msg (Format.sprintf "The terminal %s is not supported" str))

  let terminal_conv =
    Cmdliner.Arg.conv (terminal_parser, pp_terminal)

  let cmd: unit Cmdliner.Cmd.t =
    let open Cmdliner in
    let aux: bool -> int option -> int option -> int option -> string option ->
      string option -> bool -> bool -> terminal option -> unit  =
      fun verbose parallel timeout memory input output tofiles print
        terminal ->
        ignore print;
        Commands.run verbose parallel timeout memory input output tofiles true
          terminal
    in
    let doc =
      "Run AEF with in fuzzing mode"
    and verbose =
      Arg.(value & flag & info ["v";"verbose"]
             ~doc:"Set verbose printing to true")
    and parallel =
      Arg.(value & opt (some int) None & info ["p";"parallel-mode"]
             ~doc:"Run in parallel mode, expects a number of cores to use.")
    and timeout =
      Arg.(value & opt (some int) None & info ["t";"timeout"]
             ~doc:"Timeout for each run in milliseconds.")
    and memory =
      Arg.(value & opt (some int) None & info ["m";"memory"]
             ~doc:"Memory limit for child processes in megabytes.")
    and input =
      Arg.(value & opt (some string) None & info ["i";"input"]
             ~doc:"Input directory with raw data files. Default is \
                   \"./input/\" containing one file \"input.txt\" containing \
                   \"000000\".")
    and output =
      Arg.(value & opt (some string) None & info ["o";"output"]
             ~doc:"Output directory for AFL's findings.")
    and tofiles =
      Arg.(value & flag & info ["F";"to-files"]
             ~doc:"Store AFL standard output into files instead of opening \
                   new terminals.")
    and print =
      Arg.(value & flag & info ["P";"print"]
             ~doc:"Print the generated script or command and don't run it.")
    and terminal =
      let doc = Format.sprintf
          "Sets the terminal to use to launch AFL instances. Possible values \
           %s, default is \"gnome-terminal\". The value is ignored if the \
           \"--to-files\" flag is provided."
          (Arg.doc_alts (List.map fst term_list))
      in
      Arg.(
        value &
        opt (some terminal_conv) None &
        info ["T"; "terminal"] ~doc
      )
    in
    Cmd.v (Cmd.info ~doc "fuzz")
      (Term.(const aux $ verbose $ parallel $ timeout $ memory $ input $
             output $ tofiles $ print $ terminal))
end

module Rerun = struct
  let cmd =
    let open Cmdliner in
    let aux verbose ipfl =
      match ipfl with
      | [] ->
        failwith "rerun: provide the path to at least one file containig \
                  marshalled bug info to reproduce"
      | _ ->
        let bil = List.map get_bug_info ipfl in
        List.iter (Rerun.rerun ~verbose) bil
    in
    let doc =
      "Rerun Alt-Ergo on test case that reveiled a bug"
    and verbose =
      Arg.(value & flag & info ["v";"verbose"]
             ~doc:"Set verbose printing to true")
    and ipfl =
      Arg.(value & pos_all string [] & info []
             ~doc:"Path to a file containing marshalled information about a \
                   detected bug")
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
      Error (`Msg (Format.sprintf "The output language %s is not supported" s))

  let output_lang_conv =
    Cmdliner.Arg.conv (output_lang_parser, pp_output_lang)

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
    let doc =
      "Translate a file containig marshalled bug information into a \".ae\" \
       or \".smt2\" file containig the SMT formulas that caused the bug."
    and ipf_opt =
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
          (Arg.doc_alts (List.map fst output_lang_list))
      in
      Arg.(
        value &
        opt (some output_lang_conv) None &
        info ["t"; "translate"] ~doc
      )
    in
    Cmd.v (Cmd.info ~doc "translate")
      (Term.(const aux $ ipf_opt $ opf_opt $ trlang))
end

module Init = struct
  let cmd: unit Cmdliner.Cmd.t =
    let open Cmdliner in
    let doc =
      "Initialize the necessary repositories to run AEF in the fuzzing mode"
    in
    Cmd.v (Cmd.info ~doc "init")
      (Term.(const Commands.init $ const ()))
end

module Cleanup = struct
  let cmd: unit Cmdliner.Cmd.t =
    let open Cmdliner in
    let doc =
      "Delete all the repositiries that were created with the $(b,init) command"
    in
    Cmd.v (Cmd.info ~doc "cleanup")
      (Term.(const Commands.cleanup $ const ()))
end

module Kill_all = struct
  let cmd: unit Cmdliner.Cmd.t =
    let open Cmdliner in
    let doc =
      "Kill the instances of AFL, the output of which was redirected to files \
       the files in DATA_DIR/afl_out/"
    in
    Cmd.v (Cmd.info ~doc "kill_all")
      (Term.(const Commands.kill_all $ const ()))
end

(* module Rename = struct
   let cmd: unit Cmdliner.Cmd.t =
    let open Cmdliner in
    let aux src dest_opt move =
      Commands.rename src dest_opt move
    in
    let file =
      Arg.(value & pos 0 (some string) None & info []
             ~doc:"Path to the file to rename")
    and move =
      Arg.(value & flag & info ["m"; "move"]
             ~doc:"Move the file to the destination file (if a file is \
                   provided instead of a file name as destination)")
    and dest_opt =
      Arg.(value & pos 0 (some string) None & info [] ~docv:"PATH"
             ~doc:"The new name to give the file or the path to which it \
                   should be moved (if -m is given)")
    in
    Cmd.v (Cmd.info "rename")
      (Term.(const aux $ file $ dest_opt $ move))
   end *)

let parse_opt () =
  let open Cmdliner in
  let default, info =
    let doc = "A fuzzer for the Alt-Ergo SMT solver." in
    let man = [
      `S "DESCRIPTION";
      `P "$(b,alt-ergo-fuzz) is a automatic testing tool for the Alt-Ergo SMT \
          solver which use the AFL fuzzer's capabilities to generate test \
          cases on which to run Alt-Ergo. It also offers QuickCheck-like \
          random generation of test cases.";
      `S "COMMANDS";
      `S "OPTIONS";
    ] in
    Term.(
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
      const aux $ verbose
    ),
    Cmd.info ~version:"dev" ~man ~doc "alt-ergo-fuzz"
  in
  let cmds = [
    Fuzz.cmd;
    Rerun.cmd;
    Translate.cmd;
    Init.cmd;
    Cleanup.cmd;
    Kill_all.cmd;
    (* Rename.cmd; *)
  ] in
  let v = Cmd.eval_value (Cmd.group info ~default cmds) in
  v

let () =
  match parse_opt () with
  | Error (`Parse | `Term | `Exn) -> exit 2
  | Ok (`Ok () | `Version | `Help) -> ()
