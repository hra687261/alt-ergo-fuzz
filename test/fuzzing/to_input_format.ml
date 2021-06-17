open FUtils

let inputs = ref []

let () =
  Arg.parse []
    (fun s -> inputs := !inputs @ [s])
    "Usage: ./rerun.exe inputfile outputfile"

let () =

  assert (List.length !inputs = 2);
  let input_file_name = List.hd !inputs in 
  let output_file_name = List.nth !inputs 1 in
  Format.printf "\n%s\n%s@."  input_file_name output_file_name; 
  Format.printf "Reading from the file: %s@." input_file_name;
  let line = Core.In_channel.read_all input_file_name in 

  let {decls; _} : bug_info = 
    Marshal.from_string line 0 
  in 
  Format.printf "Writing to the file: %s@." output_file_name;
  let oc = open_out output_file_name in
  output_string oc (Format.asprintf "%a" Translate_ae.print_decls decls);
  close_out oc