(*
build from root with: 
  make && dune build

to fuzz with afl-fuzz execute:
  afl-fuzz -i ./test/fuzzing/input/ -o ./test/fuzzing/output/ ./_build/default/test/fuzzing/afl_fuzzing.exe  @@
or (if max_fuel = 5):
  afl-fuzz -t 100 -m 50 -i ./test/fuzzing/input/ -o ./test/fuzzing/output/ ./_build/default/test/fuzzing/afl_fuzzing.exe  @@
this sets the timeout to 100ms, and the memory to 50MB

to run quickcheck-like property testing execute:
  ./_build/default/test/fuzzing/afl_fuzzing.exe
*)


open Crowbar
open AltErgoLib

module PA = Parsed_interface

let thmid = ref 0

let max_fuel = 3

let nb_usyms = 3

let get_usym_var (ty : Ty.t) = 
  match ty with 
  | Ty.Tint -> ["iv1"; "iv2"; "iv3"]
  | Ty.Treal -> ["rv1"; "rv2"; "rv3"]
  | Ty.Tbool -> ["bv1"; "bv2"; "bv3"]
  | _ -> assert false 

let get_usym_fun (ty : Ty.t) = 
  match ty with 
  | Ty.Tint ->
    [ "if1"; (* Ty.Tint -> Ty.Tint *)
      "if2"; (* Ty.Tint -> Ty.Treal -> Ty.Tint *)
      "if3"] (* Ty.Tint -> Ty.Treal -> Ty.Tbool -> Ty.Tint *)
  | Ty.Treal ->
    [ "rf1"; (* Ty.Tint -> Ty.Treal *)
      "rf2"; (* Ty.Tint -> Ty.Treal -> Ty.Treal *)
      "rf3"] (* Ty.Tint -> Ty.Treal -> Ty.Tbool -> Ty.Treal *)
  | Ty.Tbool ->
    [ "bf1"; (* Ty.Tint -> Ty.Tbool *)
      "bf2"; (* Ty.Tint -> Ty.Treal -> Ty.Tbool *)
      "bf3"] (* Ty.Tint -> Ty.Treal -> Ty.Tbool -> Ty.Tbool *)
  | _ -> assert false

let arithops (ty : Ty.t) =
  List.map (fun x -> Symbols.Op x) @@
  let l = [Symbols.Plus; Symbols.Minus; Symbols.Mult; Symbols.Div] in 
    match ty with 
    | Ty.Tint -> Symbols.Modulo::l
    | Ty.Treal -> l
    | _ -> assert false

(** Quantifier type *)
type qty =  
  | U (* Universal *) 
  | E (* Existential *)

(** Quantified variable type *)
type qvar = {name: string; id: int; vty: Ty.t; qt: qty} 

module V = 
  struct
    type t = qvar 
    let compare a b = 
      let cmp = Ty.compare a.vty b.vty in 
        if cmp = 0
        then Int.compare a.id b.id
        else cmp
  end

module VS = Set.Make(V)

type eg_res = {vst: VS.t; exp: Expr.t}

let mk_egr vst exp = {vst; exp}

let mk_var_expr v = Expr.mk_term (Symbols.name v.name) [] v.vty 

let iv_id, rv_id, bv_id = ref 0, ref 0, ref 0

let mk_new_qvar vty qt = 
  match vty with 
  | Ty.Tint -> 
    let id = incr iv_id; !iv_id in 
    let name = "xi_"^ (string_of_int id) in
      {name; id; vty; qt} 
  | Ty.Treal -> 
    let id = incr rv_id; !rv_id in 
    let name = "xr_"^ (string_of_int id) in
      {name; id; vty; qt} 
  | Ty.Tbool -> 
    let id = 
      incr bv_id; !bv_id in 
    let name = "xb_"^ (string_of_int id) in
      {name; id; vty; qt} 
  | _ -> assert false 

(** Binds each variable in vset and makes it quantified in exp *)
let quantify (vset: VS.t) (exp: Expr.t) = 
  let quantify_aux qty vnl exp ty = 
    ( match qty with 
      | U -> Expr.mk_forall
      | E -> Expr.mk_exists ) "" Loc.dummy 
    ( List.fold_left
        (fun acc v ->
          Symbols.Map.add (Symbols.Var (Var.of_string v.name)) (ty, v.id) acc) 
        Symbols.Map.empty
        vnl)
    [] exp (-42) ~toplevel:false ~decl_kind:Expr.Dgoal
  in
  let _, _, vsl, _, _ = 
    VS.fold 
    ( fun v (qacc, tacc, totacc, currq, currt) ->
        match currq, currt with
        | Some q, Some t -> 
          if Ty.equal t v.vty 
          then (
            if q == v.qt 
            then (v::qacc, tacc, totacc, currq, currt)
            else ([v], (qacc, currq)::tacc, totacc, Some v.qt, currt)) 
          else 
            ([v], [], ((qacc, currq)::tacc, currt)::totacc, Some v.qt, Some v.vty)
        
        | None, None -> 
          ([v], [], [], Some v.qt, Some v.vty)
        | _ -> assert false 
    )
    vset
    ([], [], [], None, None)
  in 
    List.fold_left 
      (fun acc (vl, oty : (qvar list * qty option) list * Ty.t option) -> 
        match oty with 
        | Some ty -> (
          List.fold_left 
            ( fun acc2 (qvl, oqty) ->
                match oqty with 
                | Some qty -> quantify_aux qty qvl acc2 ty
                | None -> assert false 
            )
            acc
            vl
        )
        | None -> assert false 
      )
      exp
      vsl

(** Generator of a constant Expr.t of type ty *)
let gen_cst (ty : Ty.t) = 
  match ty with 
  | Ty.Tint  -> 
      Crowbar.map [int] 
        (fun i -> mk_egr VS.empty (Expr.int (Int.to_string i)))
  | Ty.Treal -> 
      Crowbar.map [float] (fun i -> 
        mk_egr VS.empty (
        if Float.is_nan i
        then Expr.real "0." 
        else Expr.real (Float.to_string i)))
  | Ty.Tbool -> 
      Crowbar.map [bool] 
        ( function 
          | true -> mk_egr VS.empty Expr.vrai
          | false -> mk_egr VS.empty Expr.faux)
  | _ -> assert false 

(*** Generates an expr of type ty *)
let rec gen_iexpr ?(fuel = max_fuel) ?(ty = Ty.Tint) () : eg_res gen =
    match fuel = 0 with
    | true -> 
        Crowbar.choose @@ [gen_cst ty; gen_usym_var ty; gen_qvar ty]
    | false ->
      let fn = 
        List.fold_left 
          ( fun acc op -> 
            ( Crowbar.map
                [ gen_iexpr ~fuel:(fuel-1) ~ty (); 
                  gen_iexpr ~fuel:(fuel-1) ~ty ()]
                ( fun a b -> 
                    let exp = Expr.mk_term op [a.exp;b.exp] ty in
                    let nvs = VS.union a.vst b.vst in 
                      mk_egr nvs exp )
            )::acc)
          [gen_iexpr ~fuel:(fuel-1) ~ty ()] (arithops ty)
      in 
        Crowbar.choose @@ gen_qvar ty :: gen_usym_fun fuel ty @ fn 

(** Generates a quantified variable *)
and gen_qvar (ty: Ty.t) : eg_res gen =
  match ty with 
  | Ty.Tint -> 
      Crowbar.map 
      [Crowbar.bool] 
      ( function 
        | true -> 
          let id = incr iv_id; !iv_id in 
          let name = "Exi_"^ (string_of_int id) in
          let nv = {name; id; vty = ty; qt = E} in 
            mk_egr (VS.add nv VS.empty) (mk_var_expr nv)
        | false -> 
          let id = incr iv_id; !iv_id in 
          let name = "Uxi_"^ (string_of_int id) in
          let nv = {name; id; vty = ty; qt = U} in 
            mk_egr (VS.add nv VS.empty) (mk_var_expr nv))
  | Ty.Treal -> 
      Crowbar.map 
      [Crowbar.bool] 
      ( function 
        | true -> 
          let id = incr rv_id; !rv_id in 
          let name = "Exr_"^ (string_of_int id) in
          let nv = {name; id; vty = ty; qt = E} in 
            mk_egr (VS.add nv VS.empty) (mk_var_expr nv)
        | false -> 
          let id = incr rv_id; !rv_id in 
          let name = "Uxr_"^ (string_of_int id) in
          let nv = {name; id; vty = ty; qt = U} in 
            mk_egr (VS.add nv VS.empty) (mk_var_expr nv))
  | Ty.Tbool -> 
      Crowbar.map 
      [Crowbar.bool] 
      ( function 
        | true -> 
          let id = incr bv_id; !bv_id in 
          let name = "Exb_"^ (string_of_int id) in
          let nv = {name; id; vty = ty; qt = E} in 
            mk_egr (VS.add nv VS.empty) (mk_var_expr nv)
        | false -> 
          let id = incr bv_id; !bv_id in 
          let name = "Uxb_"^ (string_of_int id) in
          let nv = {name; id; vty = ty; qt = U} in 
            mk_egr (VS.add nv VS.empty) (mk_var_expr nv))
  | _ -> assert false 

(** Generator of a Expr.t of type bool/prop and of max depth max_fuel *)
and gen_bool_expr ?(fuel = max_fuel) () : eg_res gen =
    match fuel = 0 with
    | true -> gen_cst Ty.Tbool
    | false ->
      let arith_gens ty fuel = 
        List.map
        ( Crowbar.map [ gen_iexpr ~fuel:(fuel-1) ~ty (); 
                        gen_iexpr ~fuel:(fuel-1) ~ty ()])
        [ (fun a b ->
            let nvst = VS.union a.vst b.vst in  
            let exp = Expr.mk_eq ~iff:true a.exp b.exp in
              mk_egr VS.empty (quantify nvst exp)
          );
          (fun a b -> 
            let nvst = VS.union a.vst b.vst in  
            let exp = Expr.mk_eq ~iff:false a.exp b.exp in
              mk_egr VS.empty (quantify nvst exp)
          );
          (fun a b -> 
            let nvst = VS.union a.vst b.vst in  
            let exp = Expr.mk_distinct ~iff:true [a.exp; b.exp] in
              mk_egr VS.empty (quantify nvst exp)
          );
          (fun a b -> 
            let nvst = VS.union a.vst b.vst in  
            let exp = Expr.mk_distinct ~iff:false [a.exp; b.exp] in
              mk_egr VS.empty (quantify nvst exp)
          );

          (fun a b -> 
            let nvst = VS.union a.vst b.vst in  
            let exp = Expr.mk_builtin ~is_pos:true Symbols.LE [a.exp; b.exp] in
              mk_egr VS.empty (quantify nvst exp)
          ); 
          (fun a b -> 
            let nvst = VS.union a.vst b.vst in  
            let exp = Expr.mk_builtin ~is_pos:true Symbols.LT [a.exp; b.exp] in
              mk_egr VS.empty (quantify nvst exp)
          );
          (fun a b ->
            let nvst = VS.union a.vst b.vst in  
            let exp = Expr.mk_builtin ~is_pos:false Symbols.LE [a.exp; b.exp] in
              mk_egr VS.empty (quantify nvst exp)
          );
          (fun a b ->
            let nvst = VS.union a.vst b.vst in  
            let exp = Expr.mk_builtin ~is_pos:false Symbols.LT [a.exp; b.exp] in
              mk_egr VS.empty (quantify nvst exp)
          );]
      in

      Crowbar.choose @@
        gen_bool_expr ~fuel:(fuel-1) ()
        :: gen_qvar Ty.Tbool
        :: gen_usym_fun fuel Ty.Tbool  

        @ List.map 
        ( Crowbar.map [ gen_bool_expr ~fuel:(fuel-1) (); 
                        gen_bool_expr ~fuel:(fuel-1) ()])

        [ (fun a b ->
            let nvst = VS.union a.vst b.vst in
            let exp = Expr.mk_and a.exp b.exp true 0 in
              mk_egr nvst exp);
          (fun a b ->
            let nvst = VS.union a.vst b.vst in
            let exp = Expr.mk_or a.exp b.exp true 0 in
              mk_egr nvst exp);
          (fun a b ->
            let nvst = VS.union a.vst b.vst in
            let exp = Expr.mk_imp a.exp b.exp 0 in
              mk_egr nvst exp);
          (fun a b ->
            let nvst = VS.union a.vst b.vst in
            let exp = Expr.mk_iff a.exp b.exp 0 in
              mk_egr nvst exp);
          (fun a b ->
            let nvst = VS.union a.vst b.vst in
            let exp = Expr.mk_xor a.exp b.exp 0 in
              mk_egr nvst exp);]

        @ arith_gens Ty.Tint fuel
        @ arith_gens Ty.Treal fuel

and gen_expr ?(fuel = max_fuel) ~ty : eg_res gen = 
  match ty with 
  | Ty.Tbool -> gen_bool_expr ~fuel ()
  | _ -> gen_iexpr ~fuel ~ty ()

and gen_usym_var (ty : Ty.t) =
  Crowbar.map 
    [Crowbar.range nb_usyms] 
    ( fun pos -> 
      let var = List.nth (get_usym_var ty) pos in 
      let sym = Symbols.Name (Hstring.make var, Symbols.Other) in
      let exp = Expr.mk_term sym [] ty in 
        mk_egr VS.empty exp)

and gen_usym_fun (fuel : int) (ty : Ty.t) : (eg_res gen list ) =
  [ 
    Crowbar.map 
      [ gen_expr ~fuel:(fuel - 1) ~ty:Ty.Tint ]
      ( fun x ->
          let f = List.nth (get_usym_fun ty) 0 in 
          let sym = Symbols.Name (Hstring.make f, Symbols.Other) in
          let exp = Expr.mk_term sym [x.exp] ty in 
            mk_egr x.vst exp);

    Crowbar.map 
      [ gen_expr ~fuel:(fuel - 1) ~ty:Ty.Tint;
        gen_expr ~fuel:(fuel - 1) ~ty:Ty.Treal]
      ( fun x y ->
          let f = List.nth (get_usym_fun ty) 1 in 
          let sym = Symbols.Name (Hstring.make f, Symbols.Other) in
          let exp = Expr.mk_term sym [x.exp; y.exp] ty in 
          let nvst = VS.union x.vst y.vst in   
            mk_egr nvst exp);

    Crowbar.map 
      [ gen_expr ~fuel:(fuel - 1) ~ty:Ty.Tint;
        gen_expr ~fuel:(fuel - 1) ~ty:Ty.Treal;
        (* gen_bool_expr ~fuel:(fuel-1) () *) (*???*)
        gen_cst Ty.Tbool]
      ( fun x y z ->
        let f = List.nth (get_usym_fun ty) 2 in 
        let sym = Symbols.Name (Hstring.make f, Symbols.Other) in
        let exp = Expr.mk_term sym [x.exp; y.exp; z.exp] ty in 
        let nvst = VS.union x.vst (VS.union y.vst z.vst) in
          mk_egr nvst exp)
    ]

let cq_thm_gen : (Commands.sat_tdecl) gen = 
  Crowbar.with_printer Commands.print @@
    Crowbar.map [gen_bool_expr ()] 
      ( fun {vst; exp} : Commands.sat_tdecl ->
          let nexp = quantify vst exp in
            { st_loc = Loc.dummy;
              st_decl = 
                Commands.Query 
                  ("thm" ^ string_of_int (incr thmid; !thmid), nexp, Typed.Thm)})

module SAT = Fun_sat.Make(Theory.Main_Default)
module FE = Frontend.Make(SAT)

let proc pbs = 
  List.iter
    ( fun (pb, goal_name) -> 
        ignore goal_name;
        let _, consistent, ex = 
          List.fold_left
            ( fun acc d ->
                try 
                  FE.process_decl 
                    ( fun _ _ -> ()) (*FE.print_status*)
                    (FE.init_all_used_context ()) 
                    (Stack.create ()) 
                    acc d
                with Assert_failure (_, _, _) as exp ->
                  (* Will be replaced by writing the serialization of 
                   * of the lists of commands into the files *)
                  let time = Unix.gettimeofday () in
                  let file = 
                    "test/fuzzing/crash_output/tmp"^string_of_float time^".txt"
                  in
                  let oc = open_out file in 
                  let strl = 
                    List.map 
                      (Format.asprintf "%a" Commands.print) 
                      pb 
                  in
                  Printf.fprintf oc "%s\n\n" (Printexc.to_string exp);
                  List.iter
                    (Printf.fprintf oc "%s\n")
                    strl;
                  flush stdout;
                  close_out oc; 
                  raise exp 
          )
          (SAT.empty (), true, Explanation.empty) 
          pb
      in
      ignore ex; ignore consistent;
      (
      List.iter (Format.printf "\n ###  %s  %a\n" goal_name Commands.print) pb;
      Format.printf "%s@."
        (if consistent then "unknown" else "unsat"))
    ) pbs; 
  true (* for now *)

let () = Options.set_is_gui false

let test_id = ref 0 

let () =
  Crowbar.add_test ~name:"ae" [cq_thm_gen] 
  @@ fun m -> 
  Crowbar.check
    ((proc 
      [([m], "test_" ^ string_of_int (incr test_id; !test_id))]
    ))
