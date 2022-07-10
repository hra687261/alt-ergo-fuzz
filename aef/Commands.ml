open Utils

let default_mem_limit = 500
let default_time_limit = 10000

let data_dir = Sys.getcwd () ^ "/aef_data/"

let input_dir = data_dir ^ "input/"
let output_dir = data_dir ^ "output/"
let store_dir = data_dir ^ "store/"

let afl_out_dir = data_dir ^ "afl_out/"
let internalcrash_dir  = store_dir ^ "internalcrash/"
let other_dir = store_dir ^ "other/"
let outofmemory_dir = store_dir ^ "outofmemory/"
let stackoverflow_dir = store_dir ^ "stackoverflow/"
let timeout_dir = store_dir ^ "timeout/"
let unsoundness_dir = store_dir ^ "unsoundness/"

let input_file = input_dir ^ "input.txt"
let ids_file = data_dir ^ "ids.txt"

let dirs = [
  data_dir;
  input_dir;
  output_dir;
  store_dir;
  afl_out_dir;
  internalcrash_dir;
  other_dir;
  outofmemory_dir;
  stackoverflow_dir;
  timeout_dir;
  unsoundness_dir;
]

let mkdir path =
  if Sys.file_exists path
  then
    if Sys.is_directory path
    then () (* TODO: print a warning if the repo is not empty *)
    else
      failwith (
        Format.sprintf
          "Can't create the repository %s. \
           A file already exists on that path."
          path)
  else Unix.mkdir path 0o744

let rec rmrf path =
  if Sys.file_exists path
  then
    if Sys.is_directory path
    then (
      Sys.readdir path |>
      Array.iter (fun name -> rmrf (Filename.concat path name));
      Unix.rmdir path)
    else Sys.remove path

let init () =
  List.iter mkdir dirs;
  let oc = open_out input_file in
  Format.fprintf (Format.formatter_of_out_channel oc) "000000";
  close_out oc

let cleanup () =
  List.iter rmrf dirs

let kill_all () =
  assert (
    Sys.command (
      Format.sprintf
        "for i in $(cat %s); do kill -HUP $i; done"
        ids_file
    ) = 0
  )

(* let rename _src _dest _move = () *)

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
  Format.sprintf "for ((i = 1; i < %d; i++ )); do %s; done" n cmd

let run verbose parallel timeout memory input output tofiles print terminal =
  ignore verbose;
  let input = input <+> input_dir in
  let output = output <+> output_dir in
  let exec = Sys.executable_name in
  let terminal = terminal <+> GnomeTerminal in
  let memory = memory <+> default_mem_limit in
  let timeout = timeout <+> default_time_limit in
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
  let afl_cmd =
    match tofiles with
    | true when processes = 1 ->
      afl_cmd_tofile ~timeout ~memory ~input ~output (afl_out_dir^"fop0.txt") ids_file exec
    | true ->
      let cmd0 =
        afl_cmd_tofile ~timeout ~memory ~input ~output ~master:true
          ~id:"fuzzer0" (afl_out_dir^"fop0.txt") ids_file exec
      in
      let loop =
        aux_for_loop processes (
          afl_cmd_tofile ~timeout ~memory ~input ~output
            ~id:"fuzzer$i" (afl_out_dir^"fop$i.txt") ids_file exec)
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
  if print then Format.printf "%s@." afl_script;
