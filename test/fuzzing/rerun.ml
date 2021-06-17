(*
To run: 
./_build/default/test/fuzzing/rerun.exe ./test/fuzzing/crash_output/op_XXXXXXXXXX.txt

Reads the marshalled expression written in the file given as an argument (which was written by
afl_fuzzing.exe) and recalls the solver on it to reproduce the bug that got it written in the
first place.
*)
open AltErgoLib 
open Ast
open Translate_ae


module SAT = Fun_sat.Make(Theory.Main_Default)
module FE = Frontend.Make(SAT)

type bug_info = { 
  id: int;
  exp_str: string; 
  exp_bt_str: string; 
  decls: cmd list}

let inputs = ref []

let () =
  Arg.parse []
    (fun s -> inputs := s::!inputs)
    "Usage: ./rerun.exe file"


let reinit_env () = 
  SAT.reset_refs ();
  Expr.clear_hc ();
  Shostak.Combine.empty_cache ();
  Gc.major ()

let solve decls =
  reinit_env ();
  let _, consistent, _ = 
    List.fold_left 
      ( fun acc decl ->
          let command = translate_decl decl in 
          Format.printf "### %a@." print_cmd decl;
          Format.printf ">>> %a\n@." Commands.print command;

          FE.process_decl 
            (fun _ _ -> ()) (*FE.print_status*)
            (FE.init_all_used_context ()) 
            (Stack.create ()) 
            acc command)
      (SAT.empty (), true, Explanation.empty) 
      decls
  in
  Format.printf "%s@."
    ( if consistent 
      then "unknown" 
      else "unsat")

let () =
  Options.set_disable_weaks true;
  Options.set_is_gui false;

  Format.printf "\n\nRERUNNING @.";
  assert (List.length !inputs = 1);

  let file_name = List.hd !inputs in 
  Format.printf "Reading from the file: %s@." file_name;
  let line = Core.In_channel.read_all file_name in 

  let {decls; exp_str; exp_bt_str; _} : bug_info = 
    Marshal.from_string line 0 
  in 

  Format.printf "\nException: %s\n%s@." exp_str exp_bt_str;
  Format.printf "\nCaused by: \n%a@." 
    ( fun fmt cmdl ->
        List.iter ( 
          fun cmd ->
            Format.fprintf fmt "\n### %a@." print_cmd cmd;
            let command = translate_decl cmd in 
            Format.fprintf fmt ">>> %a@." Commands.print command
        ) cmdl
    ) decls;
  solve decls
