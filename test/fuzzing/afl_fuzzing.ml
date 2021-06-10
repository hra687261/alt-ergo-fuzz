open AltErgoLib
open Ast 

module Cr = Crowbar 
module Sy = Symbols 

module FCS = Set.Make(String)

type ast_gen_res = 
  {gast : ast; usedargs : VS.t; calledfuncs: FCS.t}
type cmd_gen_res = 
  {gcmd : cmd; calledfuncs: FCS.t}

let pr_gar fmt {gast; _} =
  print fmt gast

let pr_gcr fmt {gcmd; calledfuncs} =
  Format.fprintf fmt "{";  
  Format.fprintf fmt "\n  gcmd = \n    %a;" print_cmd gcmd;
  Format.fprintf fmt "\n  cldf = \n    %a;" 
    (fun fmt e -> 
       FCS.iter (Format.fprintf fmt "(%s)") e ) calledfuncs;
  Format.fprintf fmt "\n}"

let pr_fdi fmt {fn; params = p1, p2, p3, p4, p5; rtyp} =
  Format.fprintf fmt "{";
  Format.fprintf fmt "\n  fn = %s;" fn;
  Format.fprintf fmt "\n  params (";
  List.iter (
    Format.fprintf fmt " %a," print_typ;
  ) [p1; p2; p3; p4; p5];
  Format.fprintf fmt ")";
  Format.fprintf fmt "\n  rtyp = %a" print_typ rtyp;
  Format.fprintf fmt "\n}"

let dummy_gar = 
  {gast = Dummy; usedargs = VS.empty; calledfuncs = FCS.empty}

let typ_gen = 
  Cr.choose [
    Cr.const Tint;
    Cr.const Treal;
    Cr.const Tbool;
    (* to be completed *)
          (*
          Cr.dynamic_bind 
            (Cr.range ~min:1 64)
            (fun n -> Cr.const (TBitV n));*) 
    (*Cr.dynamic_bind 
      ( Cr.map 
          [typ_gen; typ_gen] 
          (fun x y -> x, y))
      ( fun (ti, tv) -> 
          Cr.const (TFArray {ti; tv}))*)
  ]

let cst_gen ty = 
  let rec pow x y =
    if y<=0 then 1
    else x * pow x (y-1)
  in
  match ty with 
  | Tint -> 
    Cr.map 
      [Cr.int] 
      (fun x -> 
         let gast = Cst (CstI x) in 
         {gast; usedargs = VS.empty; calledfuncs = FCS.empty})
  | Treal -> 
    Cr.map 
      [Cr.float] 
      (fun x -> 
         let gast = 
           Cst (CstR (
               if Float.is_nan x then 0. else x)) in 
         {gast; usedargs = VS.empty; calledfuncs = FCS.empty})
  | Tbool -> 
    Cr.map 
      [Cr.bool] 
      (fun x -> 
         let gast =
           Cst (CstB x) in 
         {gast; usedargs = VS.empty; calledfuncs = FCS.empty})
  | TBitV n -> 
    Cr.map 
      [Cr.range ((pow 2 n)-1)] 
      (fun x -> 
         let gast =
           Cst (CstBv (int_to_bitv ~wl:n x)) 
         in 
         {gast; usedargs = VS.empty; calledfuncs = FCS.empty})
  | _ -> assert false

let binop_gen ty fuel bop gen = 
  Cr.map 
    [ gen (fuel - 1) ty;
      gen (fuel - 1) ty] 
    ( fun x y -> 
        let gast = 
          mk_binop bop x.gast y.gast in 
        { gast; 
          usedargs = VS.union x.usedargs y.usedargs; 
          calledfuncs = FCS.union x.calledfuncs y.calledfuncs})   

let usymv_gen ty = 
  Cr.map 
    [Cr.range nb_us_vars] 
    ( fun pos -> 
        let gast =
          get_uvar_ast pos ty 
        in 
        { gast; 
          usedargs = VS.empty; 
          calledfuncs = FCS.empty})

let usymf_genl ty gen fuel = 
  let g1 =
    Cr.map 
      [Cr.range ~min:1 5]
      (fun n -> get_ufunc_ast n ty)
  in
  let g2 =
    fun {fn; params = (p1, p2, p3, p4, p5); rtyp} ->
      let auxg ty =
        match ty with
        | TDummy -> Cr.const dummy_gar
        | Tbool -> cst_gen Tbool
        | _ -> gen (fuel - 1) ty
      in 
      Cr.map [
        auxg p1; auxg p2; auxg p3; auxg p4; auxg p5
      ] (
        fun a1 a2 a3 a4 a5 ->
          let args, usedargs, calledfuncs = 
            List.fold_left (
              fun (l, vs, fcs) b -> 
                if is_dummy b.gast
                then l, vs, fcs
                else 
                  b.gast :: l, 
                  VS.union b.usedargs vs,
                  FCS.union b.calledfuncs fcs
            ) ([], VS.empty, FCS.empty) 
              [a1; a2; a3; a4; a5]
          in
          {gast = FunCall {fname = fn; rtyp; args}; usedargs; calledfuncs}
      )
  in
  Cr.dynamic_bind g1 g2


let qv_gen ty =
  let rec tts t = 
    match t with 
    | Tint -> "i" 
    | Treal -> "r"
    | Tbool -> "b"
    | TBitV n -> Format.sprintf "bv%d" n
    | TFArray {ti; tv} -> 
      Format.sprintf "%s%sfa" 
        (tts ti) (tts tv)
    | TDummy -> assert false
  in
  Cr.map 
    [Cr.bool; Cr.range nb_q_vars] 
    ( fun b pos -> 
        let gast =
          match b with 
          | true -> 
            ( match ty with
              | Tint -> mk_var (mk_vname "iuqv" pos)
              | Treal -> mk_var (mk_vname "ruqv" pos)
              | Tbool -> mk_var (mk_vname "buqv" pos)
              | TBitV n ->
                let pref = Format.sprintf "bv%duqv" n in 
                mk_var (mk_vname pref pos)
              | TFArray {ti; tv} -> 
                let pref = 
                  Format.sprintf "%s%sfa_uqv" 
                    (tts ti) (tts tv) 
                in 
                mk_var (mk_vname pref pos)
              | TDummy -> assert false
            ) ty UQ
          | false -> 
            ( match ty with
              | Tint -> mk_var (mk_vname "ieqv" pos)
              | Treal -> mk_var (mk_vname "reqv" pos)
              | Tbool -> mk_var (mk_vname "beqv" pos)
              | TBitV n ->
                let pref = Format.sprintf "bv%deqv" n in 
                mk_var (mk_vname pref pos)
              | TFArray {ti; tv} -> 
                let pref = 
                  Format.sprintf "%s%sfa_eqv" 
                    (tts ti) (tts tv) 
                in 
                mk_var (mk_vname pref pos)
              | TDummy -> assert false
            ) ty EQ
        in  
        {gast; usedargs = VS.empty; calledfuncs = FCS.empty})

let get_arg_gens ty args =
  let l = List.filter (fun x -> ty = x.vty) args in
  if List.length l > 0 then 
    List.map 
      ( fun x -> 
          Cr.const (
            { gast = Var x; 
              usedargs = VS.add x VS.empty; 
              calledfuncs = FCS.empty})
      ) l
  else []

let func_call_gen : 
  (int -> typ -> ast_gen_res Cr.gen) ->
  int -> typ -> fd_info list -> ast_gen_res Cr.gen list =
  fun gen fuel ty fdis ->
  let togen f = 
    let fname, rtyp, (p1, p2, p3, p4, p5) =
      f.fn , f.rtyp, f.params
    in 
    let auxg ty =
      match ty with
      | TDummy -> Cr.const dummy_gar
      | Tbool -> cst_gen Tbool
      | _ -> gen (fuel - 1) ty
    in 
    Cr.map [
      auxg p1; auxg p2; auxg p3; auxg p4; auxg p5
    ] (
      fun a1 a2 a3 a4 a5 ->
        let args, usedargs, calledfuncs = 
          List.fold_left (
            fun (l, vs, fcs) b -> 
              if is_dummy b.gast
              then 
                (l, vs, fcs)
              else 
                ( b.gast :: l, 
                  VS.union b.usedargs vs, 
                  FCS.union b.calledfuncs fcs)
          ) ([], VS.empty, FCS.add fname FCS.empty) [a1; a2; a3; a4; a5]
        in
        { gast = FunCall {fname; rtyp; args}; usedargs; calledfuncs}
    )
  in

  let fcs = 
    List.filter (fun x -> x.rtyp == ty) fdis
  in
  let fdgs = 
    List.map togen fcs 
  in
  if fdgs == []
  then []
  else [Cr.choose fdgs]


(********************************************************************)
let ast_gen ?(qvars = true) ?(args = []) 
    ?(fdefs: fd_info list = []) max_depth ty =
  let rec ag_aux fuel ty = 
    match fuel <= 0 with 
    | true -> 
      Cr.choose ( 
        [cst_gen ty; usymv_gen ty]
        @ (
          if qvars then [qv_gen ty] else []) 
        @ get_arg_gens ty args
      )

    | false -> 
      Cr.choose (
        cst_gen ty ::
        usymv_gen ty :: 
        [usymf_genl ty ag_aux fuel] @
        ( if qvars && fuel < max_depth 
          then [qv_gen ty] 
          else []) @

        get_arg_gens ty args @
        func_call_gen ag_aux fuel ty fdefs @

        ( match ty with 
          | Tint -> 
            List.map 
              (fun bop -> binop_gen ty fuel bop ag_aux)
              [ IAdd; ISub; IMul; IDiv; IMod; IPow]
          | Treal ->
            List.map 
              (fun bop -> binop_gen ty fuel bop ag_aux)
              [ RAdd; RSub; RMul; RDiv; RPow]
          | Tbool ->
            List.map 
              (fun bop -> binop_gen ty fuel bop ag_aux)
              [ And; Or; Xor; Imp; Iff] @   
            List.map 
              (fun bop -> binop_gen Tint fuel bop ag_aux)
              [ Lt; Le; Gt; Ge; Eq; Neq] @   
            List.map 
              (fun bop -> binop_gen Treal fuel bop ag_aux)
              [ Lt; Le; Gt; Ge; Eq; Neq]

          | TBitV len ->
            [ (* Extract *)
              Cr.dynamic_bind 
                ( Cr.map 
                    [ Cr.range ~min:1 (len - 1);
                      Cr.range ~min:1 (len - 1);]
                    (fun x y -> x, y))
                ( fun (x,y) ->
                    let l, r = 
                      if x <= y then x, y else y, x
                    in
                    Cr.map 
                      [ ag_aux (fuel - 1) (TBitV (r - l))] 
                      (fun b -> 
                         let gast = 
                           Unop (Extract {l; r}, b.gast)
                         in
                         { gast; 
                           usedargs = b.usedargs; 
                           calledfuncs = b.calledfuncs}
                      ));
              (* Concat *)
              Cr.dynamic_bind 
                (Cr.range ~min:1 (len - 1))
                (fun n -> 
                   Cr.map 
                     [ ag_aux (fuel - 1) (TBitV n);
                       ag_aux (fuel - 1) (TBitV (len - n))] 
                     ( fun x y -> 
                         let gast =
                           Binop (Concat len, x.gast, y.gast)
                         in
                         { gast; 
                           usedargs = 
                             VS.union x.usedargs y.usedargs; 
                           calledfuncs = 
                             FCS.union x.calledfuncs y.calledfuncs}
                     ))
            ]
          | TFArray {ti; tv} -> ignore (ti, tv);
            [ (* Access *)

              (* Extract *)
            ]
          | TDummy -> assert false 
        ))
  in 
  ag_aux max_depth ty


(********************************************************************)
let fdef_gen ?(fdefs = []) name func_max_depth = 
  let ag = 
    Cr.map [
      typ_gen; typ_gen; typ_gen;
      typ_gen; typ_gen; typ_gen;
    ] (
      fun t1 t2 t3 t4 t5 rtyp ->
        (t1, t2, t3, t4, t5), rtyp
    )
  in
  let fg = 
    fun ((t1, t2, t3, t4, t5), rtyp) ->
      let a1, a2, a3, a4, a5 = 
        mk_tvar "a1" t1 ARG,
        mk_tvar "a2" t2 ARG,
        mk_tvar "a3" t3 ARG,
        mk_tvar "a4" t4 ARG,
        mk_tvar "a5" t5 ARG
      in 
      let atyp = 
        [a1; a2; a3; a4; a5]
      in
      let gen =
        ast_gen ~qvars:false ~args:atyp ~fdefs func_max_depth rtyp
      in

      let ge =
        Cr.map [gen] ( 
          fun {gast = body; usedargs; calledfuncs} ->
            let atyp = 
              List.map (
                fun v -> 
                  if VS.mem v usedargs
                  then v
                  else {vname = ""; vty = TDummy; vk = US; id = 0}
              ) atyp
            in
            {gcmd = FuncDef {name; body; atyp; rtyp}; calledfuncs}
        )
      in 
      ge
  in
  Cr.with_printer pr_gcr @@
  Cr.dynamic_bind ag fg

let goal_gen ?(fdefs = []) query_max_depth () =
  Cr.with_printer pr_gcr @@
  Cr.map 
    [ast_gen ~fdefs query_max_depth Tbool]
    ( fun x ->
        let gcmd =
          Goal {
            name = "goal_" ^ (incr gid; string_of_int !gid);
            body = quantify x.gast}
        in 
        {gcmd; calledfuncs = x.calledfuncs}
    )

let axiom_gen ?(fdefs = []) axiom_max_depth () =
  Cr.with_printer pr_gcr @@
  Cr.map 
    [ast_gen ~fdefs axiom_max_depth Tbool]
    ( fun x ->
        let gcmd =
          Axiom {
            name = "ax_" ^ (incr axid; string_of_int !axid);
            body = quantify x.gast}
        in 
        {gcmd; calledfuncs = x.calledfuncs}
    )

(********************************************************************)
type declkind = (* declaration kind *) 
  | FD (* function declaration *)
  | AxD (* axiom declaration *)
  | GD (* goal declaration *)

let dk_gen =
  Cr.choose [
    Cr.const FD;
    Cr.const FD;
    Cr.const FD;
    Cr.const AxD;
  ]

let get_gen fdefs dk =
  match dk with 
  | FD ->
    fdef_gen ~fdefs 
      ("udf_"^string_of_int (incr fid; !fid)) 
      func_max_depth
  | AxD ->
    axiom_gen ~fdefs axiom_max_depth ()
  | GD ->
    goal_gen ~fdefs query_max_depth ()

(********************************************************************)
module SAT = Fun_sat.Make(Theory.Main_Default)
module FE = Frontend.Make(SAT)

let reinit_env () = 
  SAT.reset_refs ();
  Expr.clear_hc ();
  Shostak.Combine.empty_cache ();
  Gc.major ()

let solve cmds =
  let _, consistent, _ = 
    List.fold_left 
      ( fun acc cmd ->
          let command = cmd_to_commad cmd in 
          FE.process_decl 
            (fun _ _ -> ()) (*FE.print_status*)
            (FE.init_all_used_context ()) 
            (Stack.create ()) 
            acc command)
      (SAT.empty (), true, Explanation.empty) 
      cmds
  in 
  Format.printf "%s@."
    ( if consistent 
      then "unknown" 
      else "unsat")

let crash_cpt = ref 0 

let proc cmds = 
  try
    solve cmds;
    reinit_env ();
    true
  with
  | exp ->
    (*let exp_raw_bt = Printexc.get_raw_backtrace () in*)
    let exp_str = Printexc.to_string exp in 
    let exp_bt_str = Printexc.get_backtrace () in 

    let tmp = Stdlib.Marshal.to_string (exp_str, exp_bt_str, cmds) [] in
    let time = Unix.gettimeofday () in
    let file_name = 
      "test/fuzzing/crash_output/op_"^string_of_float time^".txt" 
    in

    let oc = open_out file_name in
    output_string oc tmp;
    close_out oc;

    Format.printf "\nException: %s\n%s@." exp_str exp_bt_str;
    Format.printf "\nCaused by: \n%a@." 
      ( fun fmt cmdl ->
          List.iter ( 
            fun cmd ->
              Format.fprintf fmt "\n### %a@." print_cmd cmd;
              let command = cmd_to_commad cmd in 
              Format.fprintf fmt ">>> %a@." Commands.print command
          ) cmdl
      ) cmds;
    Format.printf "Marshalled and written to the file : %s@." file_name;

    reinit_env ();
    (*Printexc.raise_with_backtrace exp exp_raw_bt*)
    false

let () =
  Options.set_disable_weaks true;
  Options.set_is_gui false;
  let mk_gen fdefs e =
    Cr.map [get_gen fdefs e] 
      (fun gres -> gres) 
  in
  let mk_fd_info fn (vs: tvar list) rtyp =
    { params = begin 
          match List.map (fun x -> x.vty) vs with 
          | [v1; v2; v3; v4; v5] -> (v1, v2, v3, v4, v5)
          | _ -> assert false
        end; fn; rtyp
    }
  in 
  let get_fdis fdefs  {gcmd; _} =
    match gcmd with
    | FuncDef {name; atyp; rtyp; _} -> 
      if (List.for_all (fun x -> x.vty == TDummy)) atyp 
      then fdefs
      else 
        let fdi = mk_fd_info name atyp rtyp in 
        fdefs @ [fdi]
    | _ -> fdefs
  in 
  let g =
    let fds = [] in 
    Cr.dynamic_bind (
      Cr.map 
        [dk_gen; dk_gen; dk_gen; dk_gen; dk_gen] 
        (fun e1 e2 e3 e4 e5 -> e1, e2, e3, e4, e5)
    ) @@ 
    fun (e1, e2, e3, e4, e5) -> 
    Cr.dynamic_bind (mk_gen fds e1)
      (fun x1 -> let fds = get_fdis fds x1 in 
        Cr.dynamic_bind (mk_gen fds e2) @@
        (fun x2 -> let fds = get_fdis fds x2 in 
          Cr.dynamic_bind (mk_gen fds e3) @@
          (fun x3 -> let fds = get_fdis fds x3 in 
            Cr.dynamic_bind (mk_gen fds e4) @@
            (fun x4 -> let fds = get_fdis fds x4 in 
              Cr.dynamic_bind (mk_gen fds e5) @@
              (fun x5 -> let fds = get_fdis fds x5 in 
                Cr.dynamic_bind (mk_gen fds GD) @@
                ( fun fg -> 
                    Cr.const @@ 
                    List.fold_right (
                      fun {gcmd; calledfuncs} (fcs, cmds) ->
                        (FCS.union calledfuncs fcs, gcmd :: cmds) 
                    ) [x1; x2; x3; x4; x5; fg] (FCS.empty ,[])
                )
              )
            )
          )
        )
      )

  in
  Cr.add_test ~name:"ae" [g] (
    fun (fcs, grl) ->
      let cmds = 
        List.fold_right (
          fun gcmd acc -> 
            match gcmd with 
            | FuncDef ({name; atyp; _} as f) ->
              if FCS.mem name fcs
              then 
                FuncDef 
                  {f with 
                   atyp = 
                     List.filter 
                       (fun v -> not (is_dummy_tvar v)) atyp} :: acc
              else 
                acc
            | _ -> gcmd :: acc
        ) grl []
      in
      Cr.check (proc cmds)
  )


