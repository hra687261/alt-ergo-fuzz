open Ast
module Cr = Crowbar 

type 'a gen_res = {
  g_res : 'a;
  u_args : VS.t; 
  u_bvars : VS.t; 
  u_dt : SS.t;
  u_us : SS.t TCM.t;
  c_funcs : SS.t}

let mk_empty_gen_res g_res =
  { g_res; 
    u_args = VS.empty;
    u_bvars = VS.empty; 
    u_dt = SS.empty;
    u_us = TCM.empty;
    c_funcs = SS.empty}

let dummy_gen_res = 
  mk_empty_gen_res Dummy

let pr_gr pr fmt {g_res; u_args; u_bvars; u_dt; c_funcs; _} =
  let pr_vs fmt e =
    Format.fprintf fmt "  {";
    VS.iter (
      fun {vname; _} ->
        Format.fprintf fmt "%s; " vname
    ) e;
    Format.fprintf fmt "}"
  in
  let pr_ss fmt e = 
    Format.fprintf fmt "  {";
    SS.iter (
      Format.fprintf fmt "%s; "
    ) e;
    Format.fprintf fmt "}"
  in
  Format.fprintf fmt "{@.";
  Format.fprintf fmt "  g_res = %a@." pr g_res;
  Format.fprintf fmt "  u_args = \n    %a;@." 
    pr_vs u_args;
  Format.fprintf fmt "  u_bvars = \n    %a;@." 
    pr_vs u_bvars;
  Format.fprintf fmt "  u_dt = \n    %a;@." 
    pr_ss u_dt;
  Format.fprintf fmt "  c_funcs = \n    %a;@." 
    pr_ss c_funcs;
  Format.fprintf fmt "}@."

let typ_gen () = 
  Cr.choose [
    Cr.const Tint;
    Cr.const Treal;
    Cr.const Tbool;
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
      ( fun x ->
          let g_res = Cst (CstI x) in 
          mk_empty_gen_res g_res
      )
  | Treal -> 
    Cr.map 
      [Cr.float] 
      (fun x -> 
         let g_res = 
           Cst (CstR (
               if Float.is_nan x then 0. else x)) 
         in 
         mk_empty_gen_res g_res)
  | Tbool -> 
    Cr.map 
      [Cr.bool] 
      (fun x -> 
         let g_res =
           Cst (CstB x) in 
         mk_empty_gen_res g_res)
  | TBitV n -> 
    Cr.map 
      [Cr.range ((pow 2 n)-1)] 
      (fun x -> 
         let g_res =
           Cst (CstBv (int_to_bitv ~wl:n x)) 
         in 
         mk_empty_gen_res g_res)
  | _ -> assert false

let binop_gen : 'a -> int -> binop -> 
  (int -> typ -> expr gen_res Cr.gen) -> expr gen_res Cr.gen =
  fun ty fuel bop gen ->
  Cr.map 
    [ gen (fuel - 1) ty;
      gen (fuel - 1) ty] 
    ( fun x y -> 
        let g_res = 
          mk_binop bop x.g_res y.g_res in 
        { g_res; 
          u_args = VS.union x.u_args y.u_args; 
          u_bvars = VS.union x.u_bvars y.u_bvars; 
          u_dt = SS.union x.u_dt y.u_dt;
          u_us = tcm_union x.u_us y.u_us;
          c_funcs = SS.union x.c_funcs y.c_funcs})   

let usymv_gen ty = 
  Cr.map 
    [Cr.range nb_us_vars] 
    ( fun pos -> 
        let g_res =
          get_uvar_expr pos ty 
        in 
        mk_empty_gen_res g_res)

let usymf_genl ty gen fuel = 
  let g1 =
    Cr.map 
      [Cr.range ~min:1 5]
      (fun n -> get_ufunc_expr n ty)
  in
  let g2 =
    fun {fn; params = (p1, p2, p3, p4, p5); rtyp} ->
      let auxg ty =
        match ty with
        | TDummy -> Cr.const dummy_gen_res
        | Tbool -> cst_gen Tbool
        | _ -> gen (fuel - 1) ty
      in 
      Cr.map [
        auxg p1; auxg p2; auxg p3; auxg p4; auxg p5
      ] (
        fun a1 a2 a3 a4 a5 ->
          let args, u_args, u_bvars, u_dt, u_us, c_funcs = 
            List.fold_left (
              fun (l, vs, bv, ud, uus, fcs) b -> 
                if is_dummy b.g_res
                then l, vs, bv, ud, uus, fcs
                else 
                  b.g_res :: l, 
                  VS.union b.u_args vs,
                  VS.union b.u_bvars bv,
                  SS.union b.u_dt ud,
                  tcm_union b.u_us uus,
                  SS.union b.c_funcs fcs
            ) ([], VS.empty, VS.empty, SS.empty, TCM.empty, SS.empty) 
              [a1; a2; a3; a4; a5]
          in
          { g_res = FunCall {
                fname = fn; fk = USF; 
                atyp = 
                  List.filter 
                    (fun x -> x <> TDummy) 
                    [p1; p2; p3; p4; p5];
                rtyp; args}; 
            u_args;
            u_bvars;
            u_dt;  
            u_us;
            c_funcs}
      )
  in
  Cr.dynamic_bind g1 g2

let qv_gen qvars ty =
  let aux pref pos = 
    mk_var (mk_vname pref pos)
  in
  if qvars then 
    [ Cr.map 
        [Cr.bool; Cr.range nb_q_vars] 
        ( fun b pos -> 
            let g_res =
              match b with 
              | true -> 
                ( match ty with
                  | Tint -> aux "iuqv" pos
                  | Treal -> aux "ruqv" pos
                  | Tbool -> aux "buqv" pos
                  | TBitV n ->
                    let pref = Format.sprintf "bv%duqv" n in 
                    aux pref pos
                  | TFArray {ti; tv} -> 
                    let pref = 
                      Format.sprintf "%s%sfa_uqv" 
                        (typ_to_str ti) (typ_to_str tv) 
                    in 
                    aux pref pos
                  | TDummy -> assert false
                  | Tadt (n, _) -> aux ("uqadt_"^n) pos 
                ) ty UQ
              | false -> 
                ( match ty with
                  | Tint -> aux "ieqv" pos
                  | Treal -> aux "reqv" pos
                  | Tbool -> aux "beqv" pos
                  | TBitV n ->
                    let pref = Format.sprintf "bv%deqv" n in 
                    aux pref pos
                  | TFArray {ti; tv} -> 
                    let pref = 
                      Format.sprintf "%s%sfa_eqv" 
                        (typ_to_str ti) (typ_to_str tv) 
                    in 
                    aux pref pos
                  | TDummy -> assert false
                  | Tadt (n, _) -> aux ("eqadt_"^n) pos 
                ) ty EQ
            in  
            mk_empty_gen_res g_res
        )
    ]
  else []

let get_arg_gens ty args =
  let l = List.filter (fun x -> ty = x.vty) args in
  if List.length l > 0 then 
    List.map 
      ( fun x -> 
          Cr.const (
            mk_empty_gen_res (Var x))
      ) l
  else []

let func_call_gen : 
  (int -> typ -> expr gen_res Cr.gen) ->
  int -> typ -> fd_info list -> expr gen_res Cr.gen list =
  fun gen fuel ty fdis ->
  let togen f = 
    let fname, rtyp, (p1, p2, p3, p4, p5) =
      f.fn , f.rtyp, f.params
    in 
    let auxg ty =
      match ty with
      | TDummy -> Cr.const dummy_gen_res
      | Tbool -> cst_gen Tbool
      | _ -> gen (fuel - 1) ty
    in 
    Cr.map [
      auxg p1; auxg p2; auxg p3; auxg p4; auxg p5
    ] (
      fun a1 a2 a3 a4 a5 ->
        let args, u_args, u_bvars, u_dt, u_us, c_funcs = 
          List.fold_left (
            fun (l, vs, bv, ud, uus, fcs) b -> 
              if is_dummy b.g_res
              then 
                l, vs, bv, ud, uus, fcs
              else 
                ( b.g_res :: l, 
                  VS.union b.u_args vs, 
                  VS.union b.u_bvars bv,  
                  SS.union b.u_dt ud,  
                  tcm_union b.u_us uus,
                  SS.union b.c_funcs fcs)
          ) ([], VS.empty, VS.empty, SS.empty, 
             TCM.empty, SS.add fname SS.empty) 
            [a1; a2; a3; a4; a5]
        in
        { g_res = FunCall {
              fname; fk = UDF; 
              atyp = 
                List.filter 
                  (fun x -> x <> TDummy) 
                  [p1; p2; p3; p4; p5];
              rtyp; 
              args}; 
          u_args; 
          u_bvars;
          u_dt;
          u_us;
          c_funcs}
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

let get_fa_access gen fuel tv = 
  Cr.dynamic_bind 
    ( typ_gen ()) 
    ( fun ti -> 
        Cr.map 
          [ gen (fuel - 1) (TFArray {ti;tv});
            gen (fuel - 1) ti]
          ( fun fa i ->
              let g_res =
                Unop (Access {ty = ti, tv; fa = fa.g_res}, i.g_res)
              in 
              { g_res;
                u_args = 
                  VS.union fa.u_args i.u_args;
                u_bvars = 
                  VS.union fa.u_bvars i.u_bvars;
                u_dt =
                  SS.union fa.u_dt i.u_dt;
                u_us = 
                  tcm_union fa.u_us i.u_us;
                c_funcs = 
                  SS.union fa.c_funcs i.c_funcs}
          )
    )

let get_fa_update gen fuel ti tv =
  let ty = 
    TFArray {ti; tv}
  in
  Cr.map 
    [ gen (fuel - 1) ty;
      gen (fuel - 1) ti;
      gen (fuel - 1) tv]
    ( fun fa i v ->
        let g_res = 
          FAUpdate {
            ty = (ti, tv); 
            fa = fa.g_res; 
            i = i.g_res; 
            v = v.g_res}
        in
        { g_res; 
          u_args = 
            VS.union fa.u_args 
              (VS.union i.u_args v.u_args); 
          u_bvars = 
            VS.union fa.u_bvars 
              (VS.union i.u_bvars v.u_bvars); 
          u_dt = 
            SS.union fa.u_dt 
              (SS.union i.u_dt v.u_dt); 
          u_us = 
            tcm_union fa.u_us 
              (tcm_union i.u_us v.u_us);
          c_funcs = 
            SS.union fa.c_funcs 
              (SS.union i.c_funcs v.c_funcs); }
    )

let get_bv_gens gen fuel len =
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
            [ gen (fuel - 1) (TBitV (r - l))] 
            ( fun {g_res; u_args; u_bvars; u_dt; u_us; c_funcs} -> 
                let g_res = 
                  Unop (Extract {l; r}, g_res)
                in
                { g_res; 
                  u_args; 
                  u_bvars;
                  u_dt;
                  u_us;
                  c_funcs}
            ));
    (* Concat *)
    Cr.dynamic_bind 
      (Cr.range ~min:1 (len - 1))
      (fun n -> 
         Cr.map 
           [ gen (fuel - 1) (TBitV n);
             gen (fuel - 1) (TBitV (len - n))] 
           (fun x y -> 
              let g_res =
                Binop (Concat len, x.g_res, y.g_res)
              in
              { g_res; 
                u_args = 
                  VS.union x.u_args y.u_args; 
                u_bvars = 
                  VS.union x.u_bvars y.u_bvars; 
                u_dt = 
                  SS.union x.u_dt y.u_dt; 
                u_us = 
                  tcm_union x.u_us y.u_us;
                c_funcs = 
                  SS.union x.c_funcs y.c_funcs}
           ))
  ]

let ite_gen gen fuel ty = 
  Cr.map 
    [ gen (fuel - 1) Tbool;
      gen (fuel - 1) ty;
      gen (fuel - 1) ty]
    (
      fun cond cons alt -> 
        { g_res = 
            ITE {ty; 
                 cond = cond.g_res; 
                 cons = cons.g_res; 
                 alt = alt.g_res}; 
          u_args = 
            VS.union cond.u_args 
              (VS.union cons.u_args alt.u_args); 
          u_bvars = 
            VS.union cond.u_bvars 
              (VS.union cons.u_bvars alt.u_bvars); 
          u_dt =
            SS.union cond.u_dt 
              (SS.union cons.u_dt alt.u_dt); 
          u_us = 
            tcm_union cond.u_us 
              (tcm_union cons.u_us alt.u_us);
          c_funcs =
            SS.union cond.c_funcs 
              (SS.union cons.c_funcs alt.c_funcs); 
        }
    )

let letin_gen : (?bvars:VS.t -> int -> typ -> expr gen_res Cr.gen) ->
  VS.t -> int -> typ -> expr gen_res Cr.gen =
  fun gen bvars fuel ty -> 
  let nv = mk_bound_var ty in
  Cr.map 
    [ gen ~bvars (fuel - 1) ty;
      gen ~bvars:(VS.add nv bvars) (fuel - 1) ty]
    (
      fun e b -> 
        { g_res = 
            if VS.mem nv b.u_bvars 
            then b.g_res 
            else LetIn (nv , e.g_res, b.g_res); 
          u_args = 
            VS.union e.u_args b.u_args; 
          u_bvars = 
            VS.union e.u_bvars b.u_bvars;
          u_dt =
            SS.union e.u_dt b.u_dt;
          u_us = 
            tcm_union e.u_us b.u_us;
          c_funcs =
            SS.union e.c_funcs b.c_funcs; 
        }
    )

let dest_gen id num =
  let dname = 
    Format.sprintf "E%d_%d" id num
  in
  Cr.map 
    [ typ_gen (); typ_gen (); typ_gen (); 
      typ_gen (); typ_gen ()]
    ( fun t1 t2 t3 t4 t5 ->
        let aux x y z = 
          Format.sprintf 
            "v%d_%d_%d"
            x y z  
        in
        let l1 = 
          [t1; t2; t3; t4; t5]
        in
        let rl2, _ = 
          List.fold_left (
            fun (acc, accn) v ->
              if v = TDummy 
              then (acc, accn)
              else (
                (aux id num accn, v) :: acc, 
                accn + 1)
          ) ([],1) l1
        in
        let l2 = 
          List.rev rl2 
        in 
        dname, l2
    )

let adt_gen () =
  let id = incr adt_id; !adt_id in
  Cr.map 
    [ dest_gen id 1;
      dest_gen id 2;
      dest_gen id 3;
      dest_gen id 4;
      dest_gen id 5]
    ( fun d1 d2 d3 d4 d5 ->
        Format.sprintf "adt_%d" id, 
        [d1; d2; d3; d4; d5]
    )

let pm_gen expr_gen (adtn, pattrns: adt) (fuel: int) (valty: typ) =
  let pattern_gen (fuel: int) (pty : patt_ty) : patt gen_res Cr.gen =
    let destrn, ppmsty = pty in 
    match ppmsty with 
    | [_; _; _; _; _] as ipts -> 
      let pattparams = 
        List.map (
          fun (n, a) -> Some (mk_tvar n a BLI)
        ) ipts
      in  
      Cr.map [
        expr_gen fuel valty 
      ] (
        fun {g_res; u_args; u_bvars; u_dt; u_us; c_funcs} ->
          { g_res = 
              {destrn; pattparams; mbody = g_res}; 
            u_args; u_bvars; u_dt; u_us; c_funcs}
      )
    | _ -> assert false     
  in 
  match pattrns with 
  | [p1; p2; p3; p4; p5] -> 
    Cr.map [
      expr_gen (fuel - 1) (Tadt (adtn, pattrns)); 
      pattern_gen (fuel - 1) p1;
      pattern_gen (fuel - 1) p2;
      pattern_gen (fuel - 1) p3;
      pattern_gen (fuel - 1) p4;
      pattern_gen (fuel - 1) p5
    ] (
      fun g p1 p2 p3 p4 p5 ->
        let {u_args; u_bvars; u_dt; u_us; c_funcs; _}, rpl =
          List.fold_left (
            fun ({g_res; u_args; u_bvars; u_dt; u_us; c_funcs}, pl) p -> 
              { g_res;
                u_args = VS.union p.u_args u_args; 
                u_bvars = VS.union p.u_bvars u_bvars; 
                u_dt = SS.union p.u_dt u_dt; 
                u_us = tcm_union p.u_us u_us; 
                c_funcs = SS.union p.c_funcs c_funcs
              }, p.g_res :: pl
          ) (p1, [p1.g_res]) [p2; p3; p4; p5]
        in 
        let patts = List.rev rpl in 
        { g_res = PMatching {mtchdv = g.g_res; patts; valty};
          u_args = VS.union g.u_args u_args;
          u_bvars = VS.union g.u_bvars u_bvars;
          u_dt = SS.add adtn (SS.union g.u_dt u_dt);
          u_us = tcm_union g.u_us u_us;
          c_funcs = SS.union g.c_funcs c_funcs;
        }
    )
  | _ -> assert false 

let app_expr_gen expr_gen fuel typ =
  match typ with 
  | TDummy -> Cr.const dummy_gen_res
  | _ -> expr_gen fuel typ

let adt_dstr_gen (expr_gen: int -> typ -> expr gen_res Cr.gen) 
    ((adtn, _) as adt: adt) (fuel: int) =
  let aux tadt_typ ((dstrn, pls): patt_ty) =
    match pls with 
    | [(n1, t1); (n2, t2); (n3, t3); (n4, t4); (n5, t5)] -> 
      Cr.map [ 
        app_expr_gen expr_gen (fuel - 1) t1;
        app_expr_gen expr_gen (fuel - 1) t2;
        app_expr_gen expr_gen (fuel - 1) t3; 
        app_expr_gen expr_gen (fuel - 1) t4;
        app_expr_gen expr_gen (fuel - 1) t5
      ] (
        fun 
          {g_res; u_args; u_bvars; u_dt; u_us; c_funcs} a2 a3 a4 a5 -> 
          let tmp = 
            List.filter 
              (fun (_, a) -> if a.g_res = Dummy then false else true)
              [(n2, a2); (n3, a3); (n4, a4); (n5, a5)]
          in
          let rparams, u_args, u_bvars, u_dt, u_us, c_funcs = 
            List.fold_left (
              fun (pl, ua, ub, ud, uus, cf) 
                (n, {g_res; u_args; u_bvars; u_dt; u_us; c_funcs}) -> 
                (n, g_res) :: pl, 
                VS.union u_args ua, 
                VS.union u_bvars ub, 
                SS.union u_dt ud, 
                tcm_union u_us uus,
                SS.union c_funcs cf
            ) ([n1, g_res], u_args, u_bvars, u_dt, u_us, c_funcs) tmp
          in
          let params = List.rev rparams in 
          let cty = tadt_typ in 
          let cname = dstrn in 
          { g_res = Cstr {cname; cty; params};
            u_args;
            u_bvars;
            u_dt = SS.add adtn u_dt;
            u_us;
            c_funcs;
          }
      ) 
    | _ -> assert false 
  in 
  let _, pls = adt in 
  Cr.dynamic_bind 
    (Cr.choose (List.map Cr.const pls))
    (aux (Tadt adt))

(********************************************************************)
let generate_expr ?(isform = false) ?(qvars = true) ?(args = []) 
    ?(fdefs: fd_info list = []) ?(adts : adt list = []) max_depth ty =
  ignore isform;
  let rec ag_aux ?(bvars = VS.empty) fuel ty = 
    if fuel <= 0 
    then
      begin 
        let gl =
          usymv_gen ty ::
          qv_gen qvars ty @
          get_arg_gens ty args
        in
        Cr.choose (
          match ty with 
          | TFArray _ | Tadt _ -> gl
          | _ -> cst_gen ty :: gl
        )
      end
    else 
      Cr.choose (
        let gl1 =
          usymv_gen ty :: 
          usymf_genl ty ag_aux fuel ::
          ite_gen ag_aux fuel ty ::
          letin_gen ag_aux bvars fuel ty ::
          (qv_gen qvars ty)  
        in
        let gl2 =
          match ty with 
          | TFArray _ | Tadt _ -> gl1
          | _ -> cst_gen ty :: gl1
        in
        let gl3 =
          get_arg_gens ty args
        in
        let gl4 =
          func_call_gen ag_aux fuel ty fdefs
        in
        let gl5 =  
          if isform 
          then []  
          else [get_fa_access ag_aux fuel ty]
        in
        let gl6 =
          ( match ty with 
            | Tint -> 
              List.map 
                (fun bop -> binop_gen ty fuel bop ag_aux)
                [ IAdd; ISub; IMul; IDiv; IMod(*; IPow*)]
            | Treal ->
              List.map 
                (fun bop -> binop_gen ty fuel bop ag_aux)
                [ RAdd; RSub; RMul; RDiv(*; RPow*)]
            | Tbool ->
              let l1 = 
                List.map 
                  (fun bop -> binop_gen ty fuel bop ag_aux)
                  [ And; Or; Xor; Imp; Iff]
              in
              let l2 =   
                List.map 
                  (fun bop -> binop_gen Tint fuel bop ag_aux)
                  [ Lt; Le; Gt; Ge; Eq; Neq] 
              in
              let l3 = 
                List.map 
                  (fun bop -> binop_gen Treal fuel bop ag_aux)
                  [ Lt; Le; Gt; Ge; Eq; Neq]
              in 
              let tmp = l2 @ l3 in 
              l1 @ tmp
            | TBitV len ->
              get_bv_gens ag_aux fuel len

            | TFArray {ti; tv} -> 
              [ 
                get_fa_update ag_aux fuel ti tv
              ]
            | Tadt adt -> 
              let adt_gen = 
                adt_dstr_gen (ag_aux ~bvars) adt fuel
              in
              if snd adt = []
              then []
              else [adt_gen]

            | _ -> assert false
          )
        in
        let pm_gens = 
          if adts = []
          then []
          else [
            Cr.dynamic_bind 
              (Cr.choose (List.map Cr.const adts))
              (fun adt -> pm_gen (ag_aux ~bvars) adt fuel ty)]
        in
        let gl7 =
          if pm_gens = []
          then []
          else [Cr.choose pm_gens]
        in
        let tmp = gl7 @ gl6 in
        let tmp = gl5 @ tmp in
        let tmp = gl4 @ tmp in
        let tmp = gl3 @ tmp in
        let tmp = gl2 @ tmp in
        gl1 @ tmp
      )
  in 
  ag_aux max_depth ty

(********************************************************************)
let fdef_gen ?(fdefs = []) ?(adts : adt list = []) 
    name func_max_depth = 
  let ag = 
    Cr.map [
      typ_gen (); typ_gen (); typ_gen ();
      typ_gen (); typ_gen (); typ_gen ();
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
        generate_expr ~qvars:false ~args:atyp ~fdefs ~adts 
          func_max_depth rtyp
      in

      let ge =
        Cr.map [gen] ( 
          fun {g_res = body; u_args; u_bvars; u_dt; u_us; c_funcs} ->
            let atyp = 
              List.map (
                fun v -> 
                  if VS.mem v u_args
                  then v
                  else {vname = ""; vty = TDummy; vk = US; id = 0}
              ) atyp
            in
            { g_res = FuncDef {name; body; atyp; rtyp}; 
              u_args; u_bvars; u_dt; u_us; c_funcs}
        )
      in 
      ge
  in
  Cr.with_printer (pr_gr print_stmt) @@
  Cr.dynamic_bind ag fg

let goal_gen ?(fdefs = []) ?(adts : adt list = []) 
    query_max_depth =
  Cr.with_printer (pr_gr print_stmt) @@
  Cr.map 
    [generate_expr ~isform:true ~fdefs ~adts query_max_depth Tbool]
    ( fun {g_res; u_args; u_bvars; u_dt; u_us; c_funcs} ->
        let g_res =
          Goal {
            name = "goal_" ^ (incr gid; string_of_int !gid);
            body = quantify g_res}
        in 
        {g_res; u_args; u_bvars; u_dt; u_us; c_funcs}
    )

let axiom_gen ?(fdefs = []) ?(adts : adt list = []) 
    axiom_max_depth =
  Cr.with_printer (pr_gr print_stmt) @@
  Cr.map 
    [generate_expr ~isform:true ~fdefs ~adts axiom_max_depth Tbool]
    ( fun {g_res; u_args; u_bvars; u_dt; u_us; c_funcs} ->
        let g_res =
          Axiom {
            name = "ax_" ^ (incr axid; string_of_int !axid);
            body = quantify g_res}
        in 
        {g_res; u_args; u_bvars; u_dt; u_us; c_funcs}
    )

(********************************************************************)
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
    axiom_gen ~fdefs axiom_max_depth 
  | GD ->
    goal_gen ~fdefs query_max_depth 

let generate_stmt 
    ?(fdefs = []) ?(adts : adt list = []) ?(name = "") kind =
  match kind with 
  | FD ->
    fdef_gen ~fdefs ~adts name func_max_depth
  | AxD ->
    axiom_gen ~fdefs ~adts axiom_max_depth 
  | GD ->
    goal_gen ~fdefs ~adts query_max_depth 

(********************************************************************)
let mk_gen : fd_info list -> adt list -> stmtkind -> stmt gen_res Cr.gen =
  fun fdefs adts e ->
  Cr.map [
    generate_stmt ~fdefs ~adts 
      ~name:("udf_"^string_of_int(incr fid; !fid)) e
  ] (fun gres -> gres) 

let mk_fd_info fn (vs: tvar list) rtyp =
  { params = begin 
        match List.map (fun x -> x.vty) vs with 
        | [v1; v2; v3; v4; v5] -> (v1, v2, v3, v4, v5)
        | _ -> assert false
      end; fn; rtyp
  }

let get_fdis : fd_info list -> stmt gen_res -> fd_info list =
  fun fdefs  {g_res; _} ->
  match g_res with
  | FuncDef {name; atyp; rtyp; _} -> 
    if (List.for_all (fun x -> x.vty == TDummy)) atyp 
    then fdefs
    else 
      let fdi = mk_fd_info name atyp rtyp in 
      fdefs @ [fdi]
  | _ -> fdefs

let rec iter : 
  fd_info list -> adt list -> SS.t -> SS.t -> stmtkind list -> 
  stmt gen_res list -> (typedecl list * stmt list) Cr.gen =
  fun fds adts udts cfs el acc ->
  match el with 
  | h :: t ->
    let a : stmt gen_res Cr.gen = mk_gen fds [] h in 
    let b : stmt gen_res -> (typedecl list * stmt list) Cr.gen = 
      fun x -> 
        let fds = get_fdis fds x in 
        let cfs = SS.union cfs x.c_funcs in
        let udts = SS.union udts x.u_dt in
        iter fds adts udts cfs t (x :: acc)
    in
    Cr.dynamic_bind a b
  | _ -> liter fds adts udts cfs acc

and liter : 
  fd_info list -> adt list -> SS.t -> SS.t -> stmt gen_res list -> (typedecl list * stmt list) Cr.gen =
  fun fds adts udts cfs acc -> 
  Cr.dynamic_bind (mk_gen fds adts GD) 
    ( fun fg ->
        let cfs = SS.union cfs fg.c_funcs in 
        let udts = SS.union udts fg.u_dt in
        let adts = 
          List.fold_left (
            fun acc ((an, _) as adt) ->
              if SS.mem an udts
              then adt :: acc
              else acc
          ) [] (List.rev adts)
        in
        let stmts = 
          List.fold_left (
            fun acc {g_res; _} ->
              match g_res with 
              | FuncDef ({name; atyp; _} as f) -> (
                  if SS.mem name cfs
                  then 
                    FuncDef 
                      {f with 
                       atyp = 
                         List.filter 
                           (fun v -> 
                              not (is_dummy_tvar v)) atyp} :: acc
                  else acc
                )
              | _ -> g_res :: acc
          ) [] (fg :: acc)
        in 
        Cr.const (adts, stmts)
    )

(********************************************************************)

let _ = ignore (get_fa_update, get_fa_access)

(*
(* the more stmts are generated the slower the fuzzing will be *)
let gen_stmts = 
  Cr.dynamic_bind (
    Cr.map 
      [dk_gen; dk_gen; dk_gen; dk_gen; dk_gen] 
      (fun e1 e2 e3 e4 e5 -> e1, e2, e3, e4, e5)
  ) @@ (
    fun (e1, e2, e3, e4, e5) -> 
      iter [] SS.empty [e1; e2; e3; e4; e5; GD] []
  )
*)

let gen_stmts = 
  Cr.dynamic_bind (
    Cr.map 
      [ adt_gen ();
        adt_gen ();
        dk_gen; 
        dk_gen] 
      (fun adt1 adt2 e1 e2 -> (adt1, adt2), (e1, e2))
  ) @@ (
    fun ((adt1, adt2), (e1, e2)) -> 
      iter [] [adt1; adt2] SS.empty SS.empty [e1; e2] []
  )
