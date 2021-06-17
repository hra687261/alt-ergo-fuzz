open AltErgoLib 
open Ast
open Translate_ae


let inputs = ref []

let () =
  Arg.parse []
    (fun s -> inputs := !inputs @ [s])
    "Usage: ./rerun.exe inputfile outputfile"

module SAT = Fun_sat.Make(Theory.Main_Default)
module FE = Frontend.Make(SAT)

type bug_info = { 
  id: int;
  exp_str: string; 
  exp_bt_str: string; 
  decls: cmd list}


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
  output_string oc (Format.asprintf "%a" print_decls decls);
  close_out oc