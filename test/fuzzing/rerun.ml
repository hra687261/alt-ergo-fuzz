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

let _ =
  Arg.parse []
    (fun s -> inputs := s::!inputs)
    "Usage: ./rerun.exe file"

let _ = Options.set_is_gui false

let _ =
  Format.printf "RERUNNING @.";
  assert (List.length !inputs = 1);
  let file = List.hd !inputs in 
  let line = Core.In_channel.read_all file in 
  let astlist : ast list = Marshal.from_string line 0 in 
  let cmds = 
    List.map 
      ( fun x -> 
          let expr = ast_to_expr (quantify x) in 
          let name = "thm" ^ string_of_int 0 in 
          let gsty = Typed.Thm in 
            Commands.{ 
              st_loc = Loc.dummy;
              st_decl = 
                Commands.Query (name, expr, gsty)})
      astlist
  in
  
  List.iter (Format.printf "\n####  %a\n@." print) astlist;
  List.iter (Format.printf "\n>>>>  %a\n\n@." Commands.print) cmds;
  
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

