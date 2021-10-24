
let () =
  assert (Array.length Sys.argv = 4);

  let input_file_name = Sys.argv.(1) in
  let output_file_name = Sys.argv.(2) in
  let t = Sys.argv.(3) in

  let issl2 = t = "smtlib2" || t = "sl2" in
  let isae = t = "alt-ergo" || t = "ae" in

  let module Tr = (
    val (
      if issl2
      then (module Smtlib2_tr)
      else (
        if isae
        then (module Tr_altergo)
        else assert false)
    ): Translater.T
  )
  in

  let module W = Writer.Make(Tr) in
  W.write input_file_name output_file_name
