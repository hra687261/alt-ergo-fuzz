open Utils

module Cr = Crowbar 
module Tae = Tr_altergo
(*module Z3S = Smtlib2_solver.Make(Solvers.Z3)*)
module CVC5S = Smtlib2_solver.Make(Solvers.CVC5)

let cnt = ref 0 

let proc decls = 

  try
    sh_printf "\n";
    incr cnt;
    let aeres = 
      Tae.process_decls decls
    in
    let cvc5res = 
      CVC5S.process_decls decls
    in
    List.iter2 (
      fun x y ->
        let aux = 
          function 
          | Translate.Sat -> "sat"
          | Translate.Unsat -> "unsat"
          | Translate.Unknown -> "unknown"
        in
        Format.printf "%s %s@." 
          (aux x) (aux y)
    ) aeres cvc5res;
    true
  with
  | Timeout -> 
    sh_printf "timed out\n"; 

    let id = !cnt in 
    let exp_str = "Timeout" in 
    let exp_bt_str = Printexc.get_backtrace () in 
    let bi = mk_bug_info id exp_str exp_bt_str decls in 

    let tmp = Stdlib.Marshal.to_string bi [] in
    let file_name = 
      Format.sprintf
        "test/fuzzing/crash_output/timeout_%d_%f.txt"
        !cnt (Unix.gettimeofday ())
    in

    let oc = open_out file_name in
    output_string oc tmp;
    close_out oc;
    true

  | exp ->
    let id = !cnt in 
    let exp_str = Printexc.to_string exp in 
    let exp_bt_str = Printexc.get_backtrace () in 
    let bi = mk_bug_info id exp_str exp_bt_str decls in 

    let tmp = Stdlib.Marshal.to_string bi [] in
    let file_name = 
      Format.sprintf
        "test/fuzzing/crash_output/crash_%d_%f.txt"
        !cnt (Unix.gettimeofday ())
    in

    let oc = open_out file_name in
    output_string oc tmp;
    close_out oc;

    sh_printf (
      Format.sprintf "\nException: %s\n%s@." 
        exp_str exp_bt_str
    );
    sh_printf (
      Format.asprintf "\nCaused by: \n%a@." 
        ( fun fmt decll ->
            List.iter ( 
              fun decl ->
                Format.fprintf fmt "\n### %a@." Ast.print_decl decl;
            ) decll
        ) decls
    );
    sh_printf (
      Format.sprintf 
        "Marshalled and written to the file : %s@." 
        file_name
    );
    false

let () =
  sh_printf ~firstcall:true ""; 
  Cr.add_test ~name:"ae" [Generator.gen_decls] 
    (fun decls -> Cr.check (proc decls))



