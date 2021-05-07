(*
To run: 
./_build/default/test/fuzzing/rerun.exe ./test/fuzzing/crash_output/op_XXXXXXXXXX.txt

Reads the marshalled expression written in the file given as an argument (which was written by
afl_fuzzing.exe) and recalls the solver on it to reproduce the bug that got it written in the
first place.
*)
open AltErgoLib 
open Ast

module SAT = Fun_sat.Make(Theory.Main_Default)
module FE = Frontend.Make(SAT)

let inputs = ref []

let () =
  Arg.parse []
    (fun s -> inputs := s::!inputs)
    "Usage: ./rerun.exe file"

let () = Options.set_is_gui false

let () =
  Format.printf "\n\nRERUNNING @.";
  assert (List.length !inputs = 1);
  
  let file = List.hd !inputs in 
  Format.printf "file = %s@." file;
  let line = Core.In_channel.read_all file in 
  
  let (exp_str, cmds) : string * cmd list = 
    Marshal.from_string line 0 
  in 
  Format.printf "\nException: %s\n@." exp_str;

  List.iter (Format.printf "###  %a" print_cmd) cmds;
  Format.printf "\n@.";

  let commands = 
    List.map 
      cmd_to_commad
      cmds
  in
  List.iter (Format.printf ">>>>  %a@." Commands.print) commands;
  Format.printf "\n@.";

  let _, consistent, _ = 
    List.fold_left 
      ( fun acc d ->
          FE.process_decl 
            ( fun _ _ -> ()) (*FE.print_status*)
            (FE.init_all_used_context ()) 
            (Stack.create ()) 
            acc d)
      (SAT.empty (), true, Explanation.empty) 
      commands
  in
    Format.printf "%s@."
      ( if consistent 
        then "unknown" 
        else "unsat")

