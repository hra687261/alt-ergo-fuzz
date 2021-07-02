

let () =
  assert (Array.length Sys.argv = 4);
  
  let t = Sys.argv.(1) in
  let issl2 = t = "smtlib2" || t = "sl2" in 
  let isae = t = "alt-ergo" || t = "ae" in 

  let module W = (val ( 
      if issl2 
      then (module Writer.Make(Solver.CVC5): Writer.T)
      else (
        if isae 
        then (module Writer.Make(Solver.AE): Writer.T)
        else assert false)
    ): Writer.T)
  in 
  
  let input_file_name = Sys.argv.(2) in 
  let output_file_name = Sys.argv.(3) in
  W.write input_file_name output_file_name

