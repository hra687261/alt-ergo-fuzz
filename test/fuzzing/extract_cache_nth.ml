open Utils

module AE_CDCL = Solver.AE_CDCL
module AE_Tableaux = Solver.AE_Tableaux
module C5S = Solver.CVC5

let () =
  if not (Array.length Sys.argv = 4)
  then
    failwith
      "Expected one argument: ./extract_cache_nth.exe if_path of_path num";

  let if_path = Sys.argv.(1) in
  let of_path = Sys.argv.(2) in
  let num = int_of_string Sys.argv.(3) in

  let ic = open_in if_path in
  let str = really_input_string ic (in_channel_length ic) in
  close_in ic;

  let bis: stmt_cache = Marshal.from_string str 0 in
  let bi = List.nth bis num in

  data_to_file bi of_path
