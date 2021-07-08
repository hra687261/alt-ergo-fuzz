
module type T = Translater.T

module type ST =
sig 
  include T
  val process_decls : Ast.typedecl list -> Ast.decl list -> Utils.answer list
end 

module CVC5: ST = 
struct 
  include Smtlib2_tr
  let process_decls tydecls decls = 
    ignore tydecls;
    let rec get_lines (ic: in_channel) =
      try
        let l = input_line ic in
        l :: get_lines ic
      with End_of_file ->
        close_in ic; 
        []
    in
    let filename = "_.smt2" in 

    let oc = open_out filename in 
    let fmt = Format.formatter_of_out_channel oc in
    Format.fprintf fmt "%a" print_decls (tydecls, decls);
    close_out oc;

    let ic = 
      Unix.open_process_in 
        (Format.sprintf "cvc5 --incremental %s" filename)
    in
    let lines = get_lines ic in 
    close_in ic;

    List.map (
      function 
      | "sat" -> Utils.Sat
      | "unsat" -> Utils.Unsat
      | "unknown" -> Utils.Unknown 
      | _ as x -> Format.printf "\n(((%s)))@." x; assert false
    ) lines

end

module AE: ST = 
struct 
  include Tr_altergo

  let process_decls =
    let module AEL = AltErgoLib in
    let module SAT = AEL.Fun_sat.Make(AEL.Theory.Main_Default) in
    let module FE = AEL.Frontend.Make(SAT) in
    let reinit_env () = 
      SAT.reset_refs ();
      AEL.Expr.clear_hc ();
      AEL.Shostak.Combine.empty_cache ();
      Gc.major ()
    in
    AEL.Options.set_disable_weaks true;
    AEL.Options.set_is_gui false;
    fun tydecls decls -> 
      reinit_env ();
      let filename = "_.ae" in 

      let oc = open_out filename in 
      let fmt = Format.formatter_of_out_channel oc in
      Format.fprintf fmt "%a" print_decls (tydecls, decls);
      close_out oc;

      let al, _ = 
        List.fold_left 
          ( fun (al, (env, consistent, ex)) decl ->

              let tdecl = translate_decl decl in 

              let env, consistent, ex = 
                FE.process_decl 
                  (fun _ _ -> ()) (*FE.print_status*)
                  (FE.init_all_used_context ()) 
                  (Stack.create ()) 
                  (env, consistent, ex) tdecl
              in

              if Ast.is_goal decl 
              then 
                if consistent 
                then Utils.Unknown :: al, (env, consistent, ex)
                else Utils.Sat :: al, (env, consistent, ex)
              else 
                al, (env, consistent, ex)
          )
          ([], (SAT.empty (), true, AEL.Explanation.empty)) 
          decls
      in al

end 