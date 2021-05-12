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

let reinit_env () = 
  SAT.reset_refs ();
  Expr.clear_hc ();
  Shostak.Combine.empty_cache ()

let rerun_cmds_debug cmds =
  let commands =
    List.map 
      (fun cmd ->
        let command = cmd_to_commad cmd in 
        Format.printf "### %a@." print_cmd cmd;
        Format.printf ">>> %a\n@." Commands.print command;
        command) 
      cmds
  in 
  let _, consistent, _ = 
    List.fold_left 
      (fun acc d ->
        FE.process_decl 
          (fun _ _ -> ()) (*FE.print_status*)
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

let () =
  Format.printf "\n\nRERUNNING @.";
  assert (List.length !inputs = 1);
  
  let file = List.hd !inputs in 
  Format.printf "file = %s@." file;
  let line = Core.In_channel.read_all file in 
  
  let (_, exp_str, cmds) : cmd list list * string * cmd list = 
    Marshal.from_string line 0 
  in 
  Format.printf "\nException: %s\n\n@." exp_str;
  
  rerun_cmds_debug cmds
