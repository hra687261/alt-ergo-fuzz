open Utils

module Tae = Tr_altergo
module Z3S = Smtlib2_solver.Make(Solvers.Z3)
module CVC5S = Smtlib2_solver.Make(Solvers.CVC5)

let inputs = ref []

let () =
  Arg.parse []
    (fun s -> inputs := s::!inputs)
    "Usage: ./rerun.exe file"

let () =
  if not (List.length !inputs = 1)
  then 
    failwith
      "Expected one argument:\n./rerun.exe path_to_file_containing_marshalled_bug_info@.";

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
        ) decls
    ) decls;
  let aeres = 
    run_with_timeout !timeout_limit Tae.process_decls decls
  in
  let z3res = 
    run_with_timeout !timeout_limit Z3S.process_decls decls
  in
  let cvc5res = 
    run_with_timeout !timeout_limit CVC5S.process_decls decls
  in
  List.iter2 (
    fun x (y, z) ->
      let aux = 
        function 
        | Translate.Sat -> "sat"
        | Translate.Unsat -> "unsat"
        | Translate.Unknown -> "unknown"
      in
      Format.printf "%s %s %s@." 
        (aux x) (aux y) (aux z)
  ) aeres (List.map2 (fun a b -> a, b) z3res cvc5res);