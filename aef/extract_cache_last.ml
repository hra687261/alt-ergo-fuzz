open Utils

let () =
  if not (Array.length Sys.argv = 3)
  then
    failwith
      "Expected one argument: ./extract_cache_last.exe if_path of_path";

  let if_path = Sys.argv.(1) in
  let of_path = Sys.argv.(2) in

  let ic = open_in if_path in
  let str = really_input_string ic (in_channel_length ic) in
  close_in ic;

  let bis: stmt_cache = Marshal.from_string str 0 in
  let bi = List.hd (List.rev bis) in

  data_to_file bi of_path
