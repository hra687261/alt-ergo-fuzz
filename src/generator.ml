open Ast
module Cr = Crowbar

type 'a gen_res = {
  g_res : 'a;
  u_bvars : VS.t;
  u_dt : SS.t;
  u_us : SS.t TCM.t;
  c_funcs : SS.t}

let aux_gen_list (gens: 'a Cr.gen list) gf =
  match gens with
  | [g1] ->
    Cr.map [g1] (
      fun g1 ->
        gf [g1]
    )
  | [g1; g2] ->
    Cr.map [g1; g2] (
      fun g1 g2 ->
        gf [g1; g2]
    )
  | [g1; g2; g3] ->
    Cr.map [g1; g2; g3] (
      fun g1 g2 g3 ->
        gf [g1; g2; g3]
    )
  | [g1; g2; g3; g4] ->
    Cr.map [g1; g2; g3; g4] (
      fun g1 g2 g3 g4 ->
        gf [g1; g2; g3; g4]
    )
  | [g1; g2; g3; g4; g5] ->
    Cr.map [g1; g2; g3; g4; g5] (
      fun g1 g2 g3 g4 g5 ->
        gf [g1; g2; g3; g4; g5]
    )
  | [g1; g2; g3; g4; g5; g6] ->
    Cr.map [g1; g2; g3; g4; g5; g6] (
      fun g1 g2 g3 g4 g5 g6 ->
        gf [g1; g2; g3; g4; g5; g6]
    )
  | [g1; g2; g3; g4; g5; g6; g7] ->
    Cr.map [g1; g2; g3; g4; g5; g6; g7] (
      fun g1 g2 g3 g4 g5 g6 g7 ->
        gf [g1; g2; g3; g4; g5; g6; g7]
    )
  | [g1; g2; g3; g4; g5; g6; g7; g8] ->
    Cr.map [g1; g2; g3; g4; g5; g6; g7; g8] (
      fun g1 g2 g3 g4 g5 g6 g7 g8 ->
        gf [g1; g2; g3; g4; g5; g6; g7; g8]
    )
  | [g1; g2; g3; g4; g5; g6; g7; g8; g9] ->
    Cr.map [g1; g2; g3; g4; g5; g6; g7; g8; g9] (
      fun g1 g2 g3 g4 g5 g6 g7 g8 g9 ->
        gf [g1; g2; g3; g4; g5; g6; g7; g8; g9]
    )
  | [g1; g2; g3; g4; g5; g6; g7; g8; g9; g10] ->
    Cr.map [g1; g2; g3; g4; g5; g6; g7; g8; g9; g10] (
      fun g1 g2 g3 g4 g5 g6 g7 g8 g9 g10 ->
        gf [g1; g2; g3; g4; g5; g6; g7; g8; g9; g10]
    )
  | _ -> assert false

let tcm_union (t1: SS.t TCM.t) (t2: SS.t TCM.t) : SS.t TCM.t =
  if TCM.is_empty t1
  then t2
  else
  if TCM.is_empty t2
  then t1
  else
    TCM.fold (
      fun k v acc ->
        match TCM.find_opt k acc with
        | Some x -> TCM.add k (SS.union x v) acc
        | None -> TCM.add k v acc
    ) t1 t2

let aux_join (l, bv, ud, uus, fcs)
    {g_res; u_bvars; u_dt; u_us; c_funcs} =
  g_res :: l,
  VS.union u_bvars bv,
  SS.union u_dt ud,
  tcm_union u_us uus,
  SS.union c_funcs fcs

let mk_empty_gen_res g_res =
  { g_res;
    u_bvars = VS.empty;
    u_dt = SS.empty;
    u_us = TCM.empty;
    c_funcs = SS.empty}

let dummy_gen_res = mk_empty_gen_res Dummy

let pr_gr pr fmt {g_res; u_bvars; u_dt; u_us; c_funcs} =
  let pr_vs fmt e =
    Format.fprintf fmt "{";
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
  let pr_tcm fmt e =
    Format.fprintf fmt "{";
    TCM.iter (
      fun k v ->
        Format.fprintf fmt "(%s:%a)"
          (get_tctag k) pr_ss v;
    ) e;
    Format.fprintf fmt "}"
  in
  Format.fprintf fmt "{@.";
  Format.fprintf fmt "  g_res = \n%a@."
    pr g_res;
  Format.fprintf fmt "  u_bvars = %a;@."
    pr_vs u_bvars;
  Format.fprintf fmt "  u_dt = %a;@."
    pr_ss u_dt;
  Format.fprintf fmt "  u_us = %a;@."
    pr_tcm u_us;
  Format.fprintf fmt "  c_funcs = %a;@."
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
      ( fun x ->
          let g_res =
            Cst (
              CstR (
                if Float.is_nan x
                then 0.
                else
                if x = Float.infinity
                then Float.max_float
                else
                if x = Float.neg_infinity
                then
                  (*because min_float is equal to -. neg_flaot*)
                  -. Float.max_float
                else
                  Float.of_string
                    (Str.replace_first
                       (Str.regexp "e.+") ""
                       (Float.to_string x)
                    )
              )
            )
          in
          mk_empty_gen_res g_res)
  | Tbool ->
    Cr.map
      [Cr.bool]
      (fun x ->
         let g_res =
           Cst (CstB x) in
         mk_empty_gen_res g_res)
  | TBitV n when Foptions.get_u_btv () ->
    Cr.map
      [Cr.range ((pow 2 n)-1)]
      (fun x ->
         let g_res =
           Cst (CstBv (int_to_bitv ~wl:n x))
         in
         mk_empty_gen_res g_res)
  | _ -> assert false

let binop_gen : 'a -> int -> binop ->
  (int -> ty -> expr gen_res Cr.gen) -> expr gen_res Cr.gen =
  fun ty fuel bop gen ->
  Cr.map
    [ gen (fuel - 1) ty;
      gen (fuel - 1) ty]
    ( fun x y ->
        let g_res =
          mk_binop bop x.g_res y.g_res in
        { g_res;
          u_bvars = VS.union x.u_bvars y.u_bvars;
          u_dt = SS.union x.u_dt y.u_dt;
          u_us = tcm_union x.u_us y.u_us;
          c_funcs = SS.union x.c_funcs y.c_funcs})

let usymv_gen ty =
  Cr.map
    [Cr.range (Foptions.get_nuv ())]
    ( fun pos ->
        let v =
          get_u_tvar pos ty
        in
        let g_res = Var v in
        { g_res;
          u_bvars = VS.empty;
          u_dt = SS.empty;
          u_us =
            TCM.add
              (A {tag = typ_tag v.vty; ty = v.vty})
              (SS.add v.vname SS.empty)
              TCM.empty;
          c_funcs = SS.empty})

let usymf_genl ty gen fuel =
  let g1 =
    Cr.map
      [Cr.range ~min:1 5]
      (fun n -> get_ufunc_expr n ty)
  in
  let g2 =
    fun {fn = fname; params; rtyp} ->
      let auxg ty =
        match ty with
        | TDummy -> Cr.const dummy_gen_res
        | Tbool -> cst_gen Tbool
        | _ -> gen (fuel - 1) ty
      in
      let egl, atyp =
        List.fold_left (
          fun (accel, acctyl) ty ->
            match ty with
            | TDummy -> (accel, acctyl)
            | _ -> auxg ty :: accel, ty :: acctyl
        ) ([] ,[]) (List.rev params)
      in
      let gf = fun rgl ->
        let args, u_bvars, u_dt, u_us, c_funcs =
          List.fold_left aux_join
            ([], VS.empty, SS.empty, TCM.empty, SS.empty)
            (List.rev rgl)
        in
        let fc =
          {fname; fk = USF; atyp; rtyp; args}
        in
        let typc =
          F {tag = typc_tag {atyp; rtyp}; atyp; rtyp}
        in
        { g_res = FunCall fc;
          u_bvars;
          u_dt;
          u_us =
            TCM.add typc (
              match TCM.find_opt typc u_us with
              | Some s -> SS.add fname s
              | None -> SS.add fname SS.empty
            ) u_us;
          c_funcs}
      in
      aux_gen_list egl gf
  in
  Cr.dynamic_bind g1 g2

let qv_gen uqvars ty =
  let aux pref pos =
    mk_var (pref ^ string_of_int pos)
  in
  if uqvars && (Foptions.get_nqv ()) > 0 && Foptions.get_u_qvrs () then
    [ Cr.map
        [Cr.bool; Cr.range (Foptions.get_nqv ())]
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

let get_bv_gens bvars ty =
  let bvgl =
    VS.fold (
      fun ({vty; _} as bv) acc ->
        if vty = ty
        then
          Cr.const (
            { g_res = Var bv;
              u_bvars = VS.add bv VS.empty;
              u_dt = SS.empty;
              u_us = TCM.empty;
              c_funcs = SS.empty
            }
          ) :: acc
        else acc
    ) bvars []
  in
  match bvgl with
  | [] -> []
  | _ -> [Cr.choose bvgl]

let get_arg_gens ty argl =
  List.fold_left (
    fun acc x ->
      if ty = x.vty
      then
        Cr.const (
          mk_empty_gen_res (Var x)
        ) :: acc
      else acc
  ) [] (List.rev argl)

let func_call_gen :
  (int -> ty -> expr gen_res Cr.gen) ->
  int -> ty -> fd_info list -> expr gen_res Cr.gen list =
  fun gen fuel ty fdis ->
  let togen {fn = fname; params; rtyp} =
    let egl, atyp =
      List.fold_left (
        fun (accel, acctyl) ty ->
          match ty with
          | TDummy -> (accel, acctyl)
          | Tbool -> cst_gen Tbool :: accel, Tbool :: acctyl
          | _ ->  gen (fuel - 1) ty :: accel, ty :: acctyl
      ) ([] ,[]) (List.rev params)
    in
    let gf =
      fun egl ->
        let args, u_bvars, u_dt, u_us, c_funcs =
          List.fold_left aux_join
            ( [], VS.empty, SS.empty,
              TCM.empty, SS.add fname SS.empty)
            (List.rev egl)
        in
        { g_res = FunCall {
              fname; fk = UDF;
              atyp;
              rtyp;
              args};
          u_bvars;
          u_dt;
          u_us;
          c_funcs}
    in
    aux_gen_list egl gf
  in
  let fdgs =
    List.fold_left (
      fun acc x ->
        if ty = x.rtyp
        then acc
        else togen x :: acc
    ) [] (List.rev fdis)
  in

  if fdgs = []
  then []
  else [Cr.choose fdgs]

let get_fa_access gen fuel tv =
  Cr.dynamic_bind (typ_gen ())
    ( fun ti ->
        Cr.map
          [ gen (fuel - 1) (TFArray {ti;tv});
            gen (fuel - 1) ti]
          ( fun fa i ->
              let g_res =
                Unop (Access {ty = ti, tv; fa = fa.g_res}, i.g_res)
              in
              { g_res;
                u_bvars = VS.union fa.u_bvars i.u_bvars;
                u_dt = SS.union fa.u_dt i.u_dt;
                u_us = tcm_union fa.u_us i.u_us;
                c_funcs = SS.union fa.c_funcs i.c_funcs}
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

let get_bvec_gens gen fuel len =
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
            ( fun ({g_res; _} as gr) ->
                { gr with g_res = Unop (Extract {l; r}, g_res)}
            )
      );
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
                u_bvars = VS.union x.u_bvars y.u_bvars;
                u_dt = SS.union x.u_dt y.u_dt;
                u_us = tcm_union x.u_us y.u_us;
                c_funcs = SS.union x.c_funcs y.c_funcs}
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

let letin_gen : (?bvars:VS.t -> int -> ty -> expr gen_res Cr.gen) ->
  VS.t -> int -> ty -> expr gen_res Cr.gen =
  fun gen bvars fuel ty ->
  let nv = mk_bound_var ty in
  Cr.map [
    gen ~bvars (fuel - 1) ty;
    gen ~bvars:(VS.add nv bvars) (fuel - 1) ty
  ] (
    fun e b ->
      if VS.mem nv b.u_bvars
      then
        { g_res = LetIn (nv , e.g_res, b.g_res);
          u_bvars = VS.union e.u_bvars b.u_bvars;
          u_dt = SS.union e.u_dt b.u_dt;
          u_us = tcm_union e.u_us b.u_us;
          c_funcs = SS.union e.c_funcs b.c_funcs;
        }
      else
        { g_res = b.g_res;
          u_bvars = b.u_bvars; u_dt = b.u_dt;
          u_us = b.u_us; c_funcs = b.c_funcs;
        }
  )

let dest_gen id num =
  let dname =
    Format.sprintf "E%d_%d" id num
  in
  Cr.map
    [ typ_gen (); typ_gen ()]
    ( fun t1 t2 ->
        let aux x y z =
          Format.sprintf
            "v%d_%d_%d"
            x y z
        in
        let l, _ =
          List.fold_left (
            fun (acc, accn) v ->
              if v = TDummy
              then acc, accn
              else
                (aux id num accn, v) :: acc, accn + 1
          ) ([],1) [t2; t1]
        in
        dname, l
    )

let adt_id = ref 0

let adt_gen () =
  let id = incr adt_id; !adt_id in
  Cr.map
    [ dest_gen id 1;
      dest_gen id 2]
    ( fun d1 d2 ->
        Adt_decl (Format.sprintf "adt_%d" id, [d1; d2])
    )

let pm_gen expr_gen (adtn, pattrns: adt) (fuel: int) (valty: ty) =
  let pattern_gen (fuel: int) (pty : rcrd_ty) : patt gen_res Cr.gen =
    let destrn, ppmsty = pty in
    match ppmsty with
    | [_; _] as ipts ->
      let pattparams =
        List.map (
          fun (n, a) -> Some (mk_tvar n a BLI)
        ) ipts
      in
      Cr.map
        [expr_gen fuel valty]
        ( fun ({g_res; _} as gr) ->
            { gr with
              g_res = {destrn; pattparams; mbody = g_res}}
        )
    | _ -> assert false
  in
  match pattrns with
  | [p1; p2] ->
    Cr.map [
      expr_gen (fuel - 1) (Tadt (adtn, pattrns));
      pattern_gen (fuel - 1) p1;
      pattern_gen (fuel - 1) p2
    ] (
      fun g p1 p2 ->
        let patts, u_bvars, u_dt, u_us, c_funcs =
          List.fold_left aux_join
            ( [], g.u_bvars,
              SS.add adtn g.u_dt, g.u_us, g.c_funcs)
            [p2; p1]
        in
        { g_res = PMatching {mtchdv = g.g_res; patts; valty};
          u_bvars; u_dt; u_us; c_funcs}
    )
  | _ -> assert false

let app_expr_gen expr_gen fuel ty =
  match ty with
  | TDummy -> Cr.const dummy_gen_res
  | _ -> expr_gen fuel ty

let adt_dstr_gen (expr_gen: int -> ty -> expr gen_res Cr.gen)
    ((adtn, _) as adt: adt) (fuel: int) =
  let aux tadt_typ ((dstrn, pls): rcrd_ty) =
    match pls with
    | [(n1, t1); (n2, t2)] ->
      Cr.map [
        app_expr_gen expr_gen (fuel - 1) t1;
        app_expr_gen expr_gen (fuel - 1) t2
      ] (
        fun a1 a2 ->
          let tmp =
            List.filter
              (fun (_, a) -> not (a.g_res = Dummy))
              [(n2, a2); (n1, a1)]
          in
          let n, {g_res; u_bvars; u_dt; u_us; c_funcs} =
            List.hd tmp
          in
          let params, u_bvars, u_dt, u_us, c_funcs =
            List.fold_left (
              fun (pl, ub, ud, uus, cf)
                (n, {g_res; u_bvars; u_dt; u_us; c_funcs}) ->
                (n, g_res) :: pl,
                VS.union u_bvars ub,
                SS.union u_dt ud,
                tcm_union u_us uus,
                SS.union c_funcs cf
            ) ( [n, g_res], u_bvars, SS.add adtn u_dt,
                u_us, c_funcs) (List.tl tmp)
          in
          let cty = tadt_typ in
          let cname = dstrn in
          { g_res = Cstr {cname; cty; params};
            u_bvars; u_dt; u_us; c_funcs}
      )
    | _ -> assert false
  in
  let _, pls = adt in
  Cr.dynamic_bind (Cr.choose (List.rev_map Cr.const pls))
    (aux (Tadt adt))

let gen_int_binop fuel ag_aux =
  List.rev_map
    (fun bop -> binop_gen Tint fuel bop ag_aux)
    [ IAdd; ISub; IMul; IDiv; IMod(*; IPow*)]

let gen_real_binop fuel ag_aux =
  List.rev_map
    (fun bop -> binop_gen Treal fuel bop ag_aux)
    [ RAdd; RSub; RMul; RDiv(*; RPow*)]

let gen_bool_binop fuel ag_aux =
  let l1 =
    List.rev_map
      (fun bop -> binop_gen Tbool fuel bop ag_aux)
      [ And; Or; Xor; Imp; Iff]
  in
  let l2 =
    List.rev_map
      (fun bop -> binop_gen Tint fuel bop ag_aux)
      [ Lt; Le; Gt; Ge; Eq; Neq]
  in
  let l3 =
    List.rev_map
      (fun bop -> binop_gen Treal fuel bop ag_aux)
      [ Lt; Le; Gt; Ge; Eq; Neq]
  in
  let tmp = List.rev_append l2 l3 in
  List.rev_append l1 tmp

let get_binop_gens bvars fuel ty
    (ag_aux: ?bvars:VS.t -> int -> ty -> expr gen_res Cr.gen) =
  match ty with
  | Tint ->
    gen_int_binop fuel ag_aux
  | Treal ->
    gen_real_binop fuel ag_aux
  | Tbool ->
    gen_bool_binop fuel ag_aux
  | TBitV len when Foptions.get_u_btv () ->
    get_bvec_gens ag_aux fuel len
  | TFArray {ti; tv} when Foptions.get_u_fa () ->
    [ get_fa_update ag_aux fuel ti tv ]
  | Tadt adt when Foptions.get_u_adts () ->
    let adt_gen =
      adt_dstr_gen (ag_aux ~bvars) adt fuel
    in
    if snd adt = []
    then []
    else [adt_gen]
  | _ -> assert false

let get_pm_gens tydecls ag_aux fuel ty =
  let adts_gens =
    List.fold_left (
      fun acc ty ->
        match ty with
        | Adt_decl adt -> Cr.const adt :: acc
        | _ -> acc
    ) [] tydecls
  in
  if adts_gens = [] then []
  else [
    Cr.dynamic_bind (Cr.choose adts_gens) (
      fun td ->
        pm_gen ag_aux td fuel ty
    )]

(********************************************************************)
let expr_gen ?(uqvars = true)
    ?(args = []) ?(fdefs: fd_info list = [])
    ?(tydecls : typedecl list = []) max_depth ty =
  let rec ag_aux ?(bvars = VS.empty) fuel ty =

    let gl =
      usymv_gen ty :: qv_gen uqvars ty
    in
    let gl =
      List.rev_append (get_bv_gens bvars ty) gl
    in
    let gl =
      match ty with
      | TFArray _ | Tadt _ -> gl
      | _ -> cst_gen ty :: gl
    in

    if fuel <= 0
    then Cr.choose gl
    else
      let gl =
        usymf_genl ty ag_aux fuel :: gl
      in
      let gl =
        if (Foptions.get_u_li ())
        then
          letin_gen ag_aux bvars fuel ty :: gl
        else gl
      in
      let gl =
        if (Foptions.get_u_ite ())
        then
          ite_gen ag_aux fuel ty :: gl
        else gl
      in
      let tmp =
        get_binop_gens bvars fuel ty ag_aux
      in
      let gl = List.rev_append gl tmp in
      let gl =
        List.rev_append
          (get_arg_gens ty args)
          gl
      in
      let gl =
        List.rev_append
          (func_call_gen ag_aux fuel ty fdefs)
          gl
      in
      let gl =
        if Foptions.get_u_fa ()
        then get_fa_access ag_aux fuel ty :: gl
        else gl
      in
      let gl =
        if (Foptions.get_u_adts ()) then
          List.rev_append (
            get_pm_gens tydecls (ag_aux ~bvars) fuel ty
          ) gl
        else gl
      in
      Cr.choose gl
  in
  ag_aux max_depth ty

(********************************************************************)
let fdef_gen ?(nbp = 5) ?(fdefs = []) ?(tydecls : typedecl list = [])
    name fmd =
  let tgens = List.init (nbp + 1) (fun _ -> typ_gen ()) in
  let ftyg =
    aux_gen_list tgens (
      fun l ->
        let rl = List.rev l in
        let rhd, rtl = List.hd rl, List.tl rl in
        List.rev rtl, rhd
    )
  in

  let fg =
    fun (ptl, rtyp) ->
      let _, rargs =
        List.fold_left (
          fun (n, al) a ->
            n + 1, mk_tvar ("a"^string_of_int n) a ARG :: al
        ) (1, []) ptl
      in
      let args = List.rev rargs in

      let gen =
        expr_gen ~uqvars:false ~args ~fdefs
          ~tydecls fmd rtyp
      in

      let ge =
        Cr.map [gen] (
          fun {g_res = body; u_bvars; u_dt; u_us; c_funcs} ->
            let atyp =
              List.fold_left (
                fun vacc v ->
                  if VS.mem v u_bvars
                  then v :: vacc
                  else vacc
              ) [] rargs
            in
            { g_res = FuncDef {name; body; atyp; rtyp};
              u_bvars; u_dt; u_us; c_funcs}
        )
      in
      ge
  in
  Cr.with_printer (pr_gr print_stmt) @@
  Cr.dynamic_bind ftyg fg

let goal_gen ?(fdefs = []) ?(tydecls : typedecl list = [])
    name qmd =
  Cr.with_printer (pr_gr print_stmt) @@
  Cr.map
    [expr_gen ~fdefs ~tydecls qmd Tbool]
    ( fun {g_res; u_bvars; u_dt; u_us; c_funcs} ->
        let g_res =
          Goal
            {name; body = quantify g_res}
        in
        {g_res; u_bvars; u_dt; u_us; c_funcs}
    )

let axiom_gen ?(fdefs = []) ?(tydecls : typedecl list = []) name
    amd =
  Cr.with_printer (pr_gr print_stmt) @@
  Cr.map
    [expr_gen ~fdefs ~tydecls amd Tbool]
    ( fun {g_res; u_bvars; u_dt; u_us; c_funcs} ->
        let g_res =
          Axiom
            {name; body = quantify g_res}
        in
        {g_res; u_bvars; u_dt; u_us; c_funcs}
    )

(********************************************************************)
let dk_gen = Cr.choose [Cr.const FD; Cr.const AxD; Cr.const GD]

let stmt_gen
    ?(fdefs = []) ?(tydecls: typedecl list = []) ?(name = "") kind =
  match kind with
  | FD ->
    fdef_gen ~fdefs ~tydecls name (Foptions.get_fmd ())
  | AxD ->
    axiom_gen ~fdefs ~tydecls name (Foptions.get_amd ())
  | GD ->
    goal_gen ~fdefs ~tydecls name (Foptions.get_qmd ())

(********************************************************************)
let axid, gid, fid = ref 0, ref 0, ref 0

let mk_gen :
  fd_info list -> typedecl list -> stmtkind -> stmt gen_res Cr.gen =
  fun fdefs tydecls sk ->
  let name =
    match sk with
    | GD -> "goal_"^string_of_int(incr gid; !gid)
    | AxD -> "ax_"^string_of_int(incr axid; !axid)
    | FD -> "udf_"^string_of_int(incr fid; !fid)
  in
  Cr.map [
    stmt_gen ~fdefs ~tydecls ~name sk
  ] (fun gres -> gres)

let mk_fd_info fn (vs: tvar list) rtyp =
  { fn; rtyp;
    params =
      List.rev_map (fun x -> x.vty) (List.rev vs)
  }

let update_fdis : fd_info list -> stmt gen_res -> fd_info list =
  fun fdefs  {g_res; _} ->
  match g_res with
  | FuncDef {name; atyp; rtyp; _} ->
    if (List.for_all (fun x -> x.vty == TDummy)) atyp
    then fdefs
    else
      let fdi = mk_fd_info name atyp rtyp in
      List.rev_append fdefs [fdi]
  | _ -> fdefs

(********************************************************************)

let rec iter :
  ?fds:fd_info list -> ?adtl:typedecl list -> ?ocfs:SS.t ->
  ?acc:stmt_c list -> stmtkind list -> stmt_c list Cr.gen =
  fun ?(fds = []) ?(adtl = []) ?(ocfs = SS.empty) ?(acc = []) kndl ->
  match kndl with
  | [knd] ->
    liter fds adtl ocfs knd acc
  | h :: t ->
    let a : stmt gen_res Cr.gen = mk_gen fds adtl h in
    let b : stmt gen_res -> stmt_c list Cr.gen =
      fun ({g_res = stmt; u_dt; u_us; c_funcs; _} as gr) ->
        let tds =
          SS.fold (
            fun str acc ->
              let nt =
                List.find (
                  fun (Adt_decl (n, _) | Record_decl (n, _)) ->
                    n = str
                ) adtl
              in
              TDS.add nt acc
          ) u_dt TDS.empty
        in
        let nfds = update_fdis fds gr in
        let ncfs = SS.union c_funcs ocfs in
        let stmtc = {stmt; tds; uss = u_us} in
        iter ~fds:nfds ~adtl ~ocfs:ncfs ~acc:(stmtc :: acc) t
    in
    Cr.dynamic_bind a b
  | _ -> assert false

and liter :
  fd_info list -> typedecl list -> SS.t ->
  stmtkind -> stmt_c list -> stmt_c list Cr.gen =
  fun fds adtl ocfs knd acc ->
  let a : stmt gen_res Cr.gen =
    mk_gen fds adtl knd
  in
  let b : stmt gen_res -> stmt_c list Cr.gen =
    fun {g_res = stmt; u_dt; u_us; c_funcs; _} ->
      let tds =
        SS.fold (
          fun str acc ->
            let nt =
              List.find (
                fun (Adt_decl (n, _) | Record_decl (n, _)) ->
                  n = str
              ) adtl
            in
            TDS.add nt acc
        ) u_dt TDS.empty
      in
      let ncfs = SS.union c_funcs ocfs in
      let stmtc = {stmt; tds; uss = u_us} in

      let stmtcl = stmtc :: acc in
      let stmtcs =
        List.fold_left (
          fun acc {stmt; tds; uss} ->
            match stmt with
            | FuncDef ({name; atyp; _} as f) ->
              if SS.mem name ncfs
              then
                let stmt =
                  FuncDef
                    { f with
                      atyp =
                        List.filter (
                          fun {vty; _} ->
                            match vty with
                            | TDummy -> false
                            | _ -> true
                        ) atyp
                    }
                in {stmt; tds; uss} :: acc
              else acc
            | _ -> {stmt; tds; uss} :: acc
        ) [] stmtcl
      in
      Cr.const stmtcs
  in
  Cr.dynamic_bind a b

(********************************************************************)

let aux_stmts_gen =
  let aux ?(nb_tds = 2) ?(nb_dks = 4) () =
    let agl = aux_gen_list in
    let a =
      agl (List.init nb_tds (fun _ -> adt_gen ())) (
        fun adtl ->
          agl
            (List.init nb_dks (fun _ -> dk_gen))
            (fun kndl -> iter ~adtl kndl)
      )
    in Cr.dynamic_bind a (fun x -> Cr.dynamic_bind x (fun x -> x))
  in
  let f =
    Cr.map [
      Cr.range ~min:Foptions.ntd_i.lb
        (Foptions.ntd_i.ub + 1);
      Cr.range ~min:Foptions.nst_i.lb
        (Foptions.nst_i.ub + 1);
    ] (fun nb_tds nb_dks -> nb_tds, nb_dks)
  in
  Cr.dynamic_bind f (
    fun (nb_tds, nb_dks) ->
      aux ~nb_tds ~nb_dks ()
  )

let fopts_gen =
  Cr.map [
    Cr.range
      ~min:Foptions.qmd_i.lb
      (Foptions.qmd_i.ub + 1);
    Cr.range
      ~min:Foptions.amd_i.lb
      (Foptions.amd_i.ub + 1);
    Cr.range
      ~min:Foptions.fmd_i.lb
      (Foptions.fmd_i.ub + 1);
    Cr.range
      ~min:Foptions.nuv_i.lb
      (Foptions.nuv_i.ub + 1);
    Cr.range
      ~min:Foptions.nqv_i.lb
      (Foptions.nqv_i.ub + 1);
  ] (
    fun qmd amd fmd nuv nqv ->
      Foptions.set_qmd qmd;
      Foptions.set_amd amd;
      Foptions.set_fmd fmd;
      Foptions.set_nuv nuv;
      Foptions.set_nqv nqv
  )

let firstcall = ref true

let stmts_gen () =
  if !firstcall = true then
    let _ =
      Cr.dynamic_bind fopts_gen (fun () -> Cr.const ())
    in firstcall := false
  else ();
  aux_stmts_gen
