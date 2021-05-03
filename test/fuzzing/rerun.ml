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
  let line = Core.In_channel.read_all file in 
  
  let (exp, cmdlist) : exn * cmd list = Marshal.from_string line 0 in 
  let msg = Printexc.to_string exp in 
  Format.printf "\nException: %s\n@." msg;

  List.iter (Format.printf "###  %a" print_cmd) cmdlist;
  Format.printf "\n@.";


  let cmds = 
    List.map 
      cmd_to_commad
      cmdlist
  in
  List.iter (Format.printf ">>>>  %a@." Commands.print) cmds;

  let _, consistent, _ = 
    List.fold_left 
      ( fun acc d ->
          FE.process_decl 
            ( fun _ _ -> ()) (*FE.print_status*)
            (FE.init_all_used_context ()) 
            (Stack.create ()) 
            acc d)
      (SAT.empty (), true, Explanation.empty) 
      cmds
  in
    Format.printf "%s@."
      ( if consistent 
        then "unknown" 
        else "unsat")

