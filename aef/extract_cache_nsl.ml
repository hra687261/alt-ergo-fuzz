open Utils

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
  let rec get_sl n l = 
    if n = 0 
    then []
    else
      match l with 
      | h :: t -> h :: get_sl (n - 1) t
      | _ -> []
  in
  let bi = get_sl num bis in
  Format.printf "\n%d %d@." (List.length bis) (List.length bi); 

  data_to_file bi of_path
