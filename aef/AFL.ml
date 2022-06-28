open Utils

let mk_afl_cmd ?timeout ?memory ?input ?output ?(master = false) ?id exec =
  Format.asprintf "afl-fuzz%a%a%a%a%a%s @@"
    (opt_app_fmt " -t %d") timeout
    (opt_app_fmt " -m %d") memory
    (opt_app_fmt " -i %s") input
    (opt_app_fmt " -o %s") output
    (opt_app_fmt (if master then " -M %s" else " -S %s")) id
    exec

let afl_cmd_tofile ?timeout ?memory ?input ?output ?(master = false) ?id
    dest_file ids_file exec =
  Format.asprintf "%s > %s 2>&1 & echo $! > %s"
    (mk_afl_cmd ?timeout ?memory ?input ?output ~master ?id exec)
    dest_file ids_file

let afl_cmd_new_term ?timeout ?memory ?input ?output ?(master = false) ?id
    terminal exec =
  let cmd = mk_afl_cmd ?timeout ?memory ?input ?output ~master ?id exec in
  terminal_wrap_cmd terminal cmd

let aux_for_loop n cmd =
  Format.sprintf "for ((i = 1; i < %d; i++ )); do\n\ \ %s\ndone" n cmd

(** TODO: check for the existance of subfolders and files, create them if they
    don't exist. *)
let run verbose parallel timeout memory input output tofiles print terminal =
  ignore verbose;
  let cwd = Sys.getcwd () in
  let input = input <+> (cwd^"/input/") in
  let output = output <+> (cwd^"/output/") in
  let copf = output^"/fuzz_output/" in
  let exec = Sys.executable_name in
  let terminal = terminal <+> GnomeTerminal in
  let memory = memory <+> Foptions._MEM_LIMIT_ in
  let timeout = timeout <+> Foptions._TIME_LIMIT_ in
  let processes =
    match parallel with
    | Some n when n <= 0 ->
      failwith "the value of \"--parallel\" has to be a positive integer."
    | Some n ->
      let np = cpu_count () in
      if n > np then
        failwith @@
        Format.sprintf
          "the value of \"--parallel\" has to be less than or equal to the \
           number of available CPUs in the machine (%d in this one)." np;
      n
    | None -> 1
  in
  let idf = cwd ^ "/ids.txt" in
  let afl_cmd =
    match tofiles with
    | true when processes = 1 ->
      afl_cmd_tofile ~timeout ~memory ~input ~output (copf^"fop0.txt") idf exec
    | true ->
      let cmd0 =
        afl_cmd_tofile ~timeout ~memory ~input ~output ~master:true
          ~id:"fuzzer0" (copf^"fop0.txt") idf exec
      in
      let loop =
        aux_for_loop processes (
          afl_cmd_tofile ~timeout ~memory ~input ~output
            ~id:"fuzzer$i" (copf^"fop$i.txt") idf exec)
      in
      Format.sprintf "%s\n%s\n" cmd0 loop
    | false when processes = 1 ->
      mk_afl_cmd ~timeout ~memory ~input ~output exec
    | false ->
      let cmd0 =
        terminal_wrap_cmd terminal @@
        mk_afl_cmd ~timeout ~memory ~input ~output ~master:true
          ~id:"fuzzer0" exec
      in
      let loop =
        aux_for_loop processes (
          terminal_wrap_cmd terminal @@
          mk_afl_cmd ~timeout ~memory ~input ~output ~id:"fuzzer$i" exec)
      in
      Format.sprintf "%s\n%s\n" cmd0 loop
  in
  let afl_script = "#!/bin/bash\n\n"^afl_cmd in
  if print
  then Format.printf "%s@." afl_script
  else ()
