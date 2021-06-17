(*
To run: 
./_build/default/test/fuzzing/rerun.exe ./test/fuzzing/crash_output/op_XXXXXXXXXX.txt

Reads the marshalled expression written in the file given as an argument (which was written by
afl_fuzzing.exe) and recalls the solver on it to reproduce the bug that got it written in the
first place.
*)
open FUtils

module AEL = AltErgoLib

let inputs = ref []

let () =
  Arg.parse []
    (fun s -> inputs := s::!inputs)
    "Usage: ./rerun.exe file"

let () =
  AEL.Options.set_disable_weaks true;
  AEL.Options.set_is_gui false;

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
    ( fun fmt decls ->
        List.iter ( 
          fun decl ->
            Format.fprintf fmt "\n### %a@." Ast.print_decl decl;
            let tdecl = Translate_ae.translate_decl decl in 
            Format.fprintf fmt ">>> %a@." AEL.Commands.print tdecl
        ) decls
    ) decls;
  run_with_timeout !timeout_limit solve decls