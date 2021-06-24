open Utils

module AEL = AltErgoLib

module Cr = Crowbar 

let proc decls = 
  try
    sh_printf "\n";
    run_with_timeout !timeout_limit solve decls; 
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
                let tdecl = Translate_ae.translate_decl decl in 
                Format.fprintf fmt ">>> %a@." AEL.Commands.print tdecl
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
  AEL.Options.set_disable_weaks true;
  AEL.Options.set_is_gui false;
  sh_printf ~firstcall:true ""; 
  Cr.add_test ~name:"ae" [Generator.gen_decls] 
    (fun decls -> Cr.check (proc decls))



