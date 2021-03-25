(*
build from root with: 
  make && dune build

to fuzz with afl-fuzz execute:
  afl-fuzz -i ./test/fuzzing/input/ -o ./test/fuzzing/output/ ./_build/default/test/fuzzing/afl_fuzzing.exe  @@

to run quickcheck-like property testing execute:
  ./_build/default/test/fuzzing/afl_fuzzing.exe
*)


open Crowbar
open AltErgoLib

module PA = Parsed_interface

let thmid = ref 0

let max_fuel = 2

(** Generator of a constant boolean Expr.t *)
let gen_cst_bexpr = 
  Crowbar.(
    with_printer Expr.print @@ 
      map [bool] 
      ( function 
        | true -> Expr.vrai
        | false -> Expr.faux))
  
(** Generator of a constant integer Expr.t *)
let gen_cst_int_expr () =
  Crowbar.(
    with_printer Expr.print @@ 
      map [int] 
      (fun i -> Expr.int (Int.to_string i)))


(** Generator of a floating point number Expr.t *)
let gen_cst_real_expr () =
  Crowbar.(
    with_printer Expr.print @@ 
      map [float] (fun i -> 
            if Float.is_nan i
            then  Expr.real "0." 
            else Expr.real (Float.to_string i)))


let arithops (ty : Ty.t) =
  List.map (fun x -> Symbols.Op x) @@
  let l = [Symbols.Plus; Symbols.Minus; Symbols.Mult; Symbols.Div] in 
    match ty with 
    | Ty.Tint -> Symbols.Modulo::l
    | Ty.Treal -> l
    | _ -> assert false
   
  
(*** Generates an expr of type ty *)
let rec gen_iexpr ?(fuel = max_fuel) ?(ty = Ty.Tint) () =
  Crowbar.with_printer Expr.print @@
    match fuel = 0 with
    | true -> 
      begin 
        match ty with 
        | Ty.Tint -> gen_cst_int_expr ()
        | Ty.Treal -> gen_cst_real_expr ()
        | _ -> assert false
      end  
    | false ->
      let fn = 
        (fun acc op -> 
          (Crowbar.map
            [ gen_iexpr ~fuel:(fuel-1) ~ty (); 
              gen_iexpr ~fuel:(fuel-1) ~ty ()]
            (fun a b -> Expr.mk_term op [a;b] ty))::acc)
      in 
      Crowbar.choose @@
        List.fold_left fn [gen_iexpr ~fuel:(fuel-1) ~ty ()] (arithops ty)

      
(** Generator of a Expr.t of type bool/prop and of max depth max_fuel *)
let rec gen_expr_prop ?(fuel = max_fuel) () = 
  Crowbar.with_printer Expr.print @@
    match fuel = 0 with
    | true -> gen_cst_bexpr
    | false ->
      Crowbar.choose @@
      
      gen_expr_prop ~fuel:(fuel-1) () :: 

      List.map 
      ( Crowbar.map [ gen_expr_prop ~fuel:(fuel-1) (); 
                      gen_expr_prop ~fuel:(fuel-1) ()])
      [ (fun a b -> Expr.mk_and a b true 0);
        (fun a b -> Expr.mk_or a b true 0);
        (fun a b -> Expr.mk_imp a b 0);
        (fun a b -> Expr.mk_iff a b 0);
        (fun a b -> Expr.mk_xor a b 0);]


      @ List.map 
      ( (* let t = Array.get [|Ty.Tint; Ty.Treal|] (Random.int 2) in *)
        (* let ty = if Random.bool () then Ty.Tint else Ty.Treal in *) 
        Crowbar.map [ gen_iexpr ~fuel:(fuel-1) ~ty:Ty.Tint (); 
                      gen_iexpr ~fuel:(fuel-1) ~ty:Ty.Tint ()])
      [ 
        (fun a b -> Expr.mk_eq ~iff:true a b);
        (fun a b -> Expr.mk_eq ~iff:false a b);
        (fun a b -> Expr.mk_distinct ~iff:true [a;b]);
        (fun a b -> Expr.mk_distinct ~iff:false [a;b]);

        (fun a b -> 
          Expr.mk_builtin ~is_pos:true Symbols.LE [a;b]); 
        (fun a b -> 
          Expr.mk_builtin ~is_pos:true Symbols.LT [a;b]); 
        (fun a b ->
          Expr.mk_builtin ~is_pos:false Symbols.LE [a;b]);
        (fun a b ->
          Expr.mk_builtin ~is_pos:false Symbols.LT [a;b]);]
        
     @ List.map 
      ( 
        Crowbar.map [ gen_iexpr ~fuel:(fuel-1) ~ty:Ty.Treal (); 
                      gen_iexpr ~fuel:(fuel-1) ~ty:Ty.Treal ()])
      [ 
        (fun a b -> Expr.mk_eq ~iff:true a b);
        (fun a b -> Expr.mk_eq ~iff:false a b);
        (fun a b -> Expr.mk_distinct ~iff:true [a;b]);
        (fun a b -> Expr.mk_distinct ~iff:false [a;b]);

        (fun a b -> 
          Expr.mk_builtin ~is_pos:true Symbols.LE [a;b]); 
        (fun a b -> 
          Expr.mk_builtin ~is_pos:true Symbols.LT [a;b]); 
        (fun a b ->
          Expr.mk_builtin ~is_pos:false Symbols.LE [a;b]);
        (fun a b ->
          Expr.mk_builtin ~is_pos:false Symbols.LT [a;b]);]


(** Makes a Command.Query with goal_sort = theorem from an Expr.t *)
let mk_cq_thm_from_expr (exp : Expr.t) : Commands.sat_tdecl =
  {
    st_loc = Loc.dummy;
    st_decl = 
      Commands.Query 
        ("thm" ^ string_of_int (incr thmid; !thmid), exp, Typed.Thm)
  }

let gen_cq_thm_from_exprgen : (Commands.sat_tdecl) gen = 
  Crowbar.with_printer Commands.print @@
    Crowbar.map [gen_expr_prop ()] mk_cq_thm_from_expr


module SAT = Fun_sat.Make(Theory.Main_Default)
module FE = Frontend.Make(SAT)

let proc pbs = 
  List.iter
    (fun (pb, goal_name) -> ignore goal_name;
      let _, consistent, ex = 
        List.fold_left
          (fun acc d ->
             FE.process_decl 
               (fun _ _ -> ()) 
               (FE.init_all_used_context ()) 
               (Stack.create ()) 
               acc d)
          (SAT.empty (), true, Explanation.empty) 
          pb
      in
      ignore ex; ignore consistent;
      (*
      Format.printf "%s@."
        (if consistent then "consistent(?)\n" else "inconsistent(?)\n")
      *)
    ) pbs; 
  true (* for now *)

let () = Options.set_is_gui false

let test_id = ref 0 

let () =
  Crowbar.add_test ~name:"ae" [gen_cq_thm_from_exprgen] 
  @@ fun m -> 
  Crowbar.check 
    ((proc 
      [([m], "test_" ^ string_of_int (test_id := !test_id + 1; !test_id))]
    ))
