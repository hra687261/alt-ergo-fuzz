open Ast
module Cr = Crowbar 

(************************************************************)
let query_max_depth = ref 3
let axiom_max_depth = ref 3
let func_max_depth = ref 3

let nb_us_vars = ref 5
let nb_q_vars = ref 1

(************************************************************)
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

let pr_gr pr fmt {g_res; u_args; u_bvars; u_dt; u_us; c_funcs} =
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
  Format.fprintf fmt "  u_args = %a;@." 
    pr_vs u_args;
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
    [Cr.range !nb_us_vars] 
    ( fun pos -> 
        let v =
          get_u_tvar pos ty 
        in 
        let g_res = Var v in 
        { g_res; 
          u_args = VS.empty;
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
    fun {fn = fname; params = (p1, p2, p3, p4, p5); rtyp} ->
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
              fun ((l, vs, bv, ud, uus, fcs) as acc) b -> 
                match b.g_res with 
                | Dummy -> acc 
                | _ ->
                  b.g_res :: l, 
                  VS.union b.u_args vs,
                  VS.union b.u_bvars bv,
                  SS.union b.u_dt ud,
                  tcm_union b.u_us uus,
                  SS.union b.c_funcs fcs
            ) ([], VS.empty, VS.empty, SS.empty, TCM.empty, SS.empty) 
              [a5; a4; a3; a2; a1]
          in
          let atyp = 
            List.filter 
              (fun x -> x <> TDummy) 
              [p1; p2; p3; p4; p5]
          in 
          let fc = 
            {fname; fk = USF; atyp; rtyp; args} 
          in
          let typc = 
            F {tag = typc_tag {atyp; rtyp}; atyp; rtyp}
          in
          { g_res = FunCall fc; 
            u_args;
            u_bvars;
            u_dt;  
            u_us = 
              TCM.add typc ( 
                match TCM.find_opt typc u_us with
                | Some s -> SS.add fname s
                | None -> SS.add fname SS.empty
              ) u_us; 
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
        [Cr.bool; Cr.range !nb_q_vars] 
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
  List.fold_left (
    fun acc x ->
      if ty = x.vty
      then 
        Cr.const (
          mk_empty_gen_res (Var x)
        ) :: acc 
      else acc
  ) [] (List.rev args)

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
            fun ((l, vs, bv, ud, uus, fcs) as acc) gr -> 
              match gr.g_res with 
              | Dummy -> acc 
              | _ ->
                ( gr.g_res :: l, 
                  VS.union gr.u_args vs, 
                  VS.union gr.u_bvars bv,  
                  SS.union gr.u_dt ud,  
                  tcm_union gr.u_us uus,
                  SS.union gr.c_funcs fcs)
          ) ( [], VS.empty, VS.empty, SS.empty, 
              TCM.empty, SS.add fname SS.empty)
            [a5; a4; a3; a2; a1]
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
                u_args = VS.union fa.u_args i.u_args;
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
                u_args = VS.union x.u_args y.u_args; 
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
          u_args = VS.union e.u_args b.u_args;
          u_bvars = VS.union e.u_bvars b.u_bvars;
          u_dt = SS.union e.u_dt b.u_dt;
          u_us = tcm_union e.u_us b.u_us;
          c_funcs = SS.union e.c_funcs b.c_funcs;
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
        Format.sprintf "adt_%d" id, [d1; d2]
    )

let pm_gen expr_gen (adtn, pattrns: adt) (fuel: int) (valty: typ) =
  let pattern_gen (fuel: int) (pty : patt_ty) : patt gen_res Cr.gen =
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
        let patts, u_args, u_bvars, u_dt, u_us, c_funcs = 
          List.fold_left (
            fun (rpl, ua, ub, ud, uus, cf)
              {g_res; u_args; u_bvars; u_dt; u_us; c_funcs} -> 
              g_res :: rpl, 
              VS.union u_args ua, 
              VS.union u_bvars ub, 
              SS.union u_dt ud, 
              tcm_union u_us uus,
              SS.union c_funcs cf     
          ) ( [], g.u_args, g.u_bvars, 
              SS.add adtn g.u_dt, g.u_us, g.c_funcs)  
            [p2; p1]
        in
        { g_res = PMatching {mtchdv = g.g_res; patts; valty};
          u_args; u_bvars; u_dt; u_us; c_funcs}
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
    | [(n1, t1); (n2, t2)] -> 
      Cr.map [ 
        app_expr_gen expr_gen (fuel - 1) t1;
        app_expr_gen expr_gen (fuel - 1) t2
      ] (
        fun a1 a2 -> 
          let tmp = 
            List.filter 
              (fun (_, a) -> if a.g_res = Dummy then false else true)
              [(n2, a2); (n1, a1)]
          in
          let n, {g_res; u_args; u_bvars; u_dt; u_us; c_funcs} =
            List.hd tmp 
          in
          let params, u_args, u_bvars, u_dt, u_us, c_funcs = 
            List.fold_left (
              fun (pl, ua, ub, ud, uus, cf) 
                (n, {g_res; u_args; u_bvars; u_dt; u_us; c_funcs}) -> 
                (n, g_res) :: pl, 
                VS.union u_args ua, 
                VS.union u_bvars ub, 
                SS.union u_dt ud, 
                tcm_union u_us uus,
                SS.union c_funcs cf
            ) ( [n, g_res], u_args, u_bvars, SS.add adtn u_dt, 
                u_us, c_funcs) (List.tl tmp)
          in
          let cty = tadt_typ in 
          let cname = dstrn in 
          { g_res = Cstr {cname; cty; params};
            u_args; u_bvars; u_dt; u_us; c_funcs}
      ) 
    | _ -> assert false 
  in 
  let _, pls = adt in 
  Cr.dynamic_bind (Cr.choose (List.rev_map Cr.const pls))
    (aux (Tadt adt))

(********************************************************************)
let expr_gen ?(isform = false) ?(qvars = true) ?(args = []) 
    ?(fdefs: fd_info list = []) ?(adts : adt list = []) max_depth ty =
  ignore isform;
  let rec ag_aux ?(bvars = VS.empty) fuel ty = 
    if fuel <= 0
    then
      let gl =
        usymv_gen ty :: qv_gen qvars ty
      in
      let gl = List.rev_append gl (get_arg_gens ty args) in
      let gl =
        match ty with
        | TFArray _ | Tadt _ -> gl
        | _ -> cst_gen ty :: gl
      in
      Cr.choose gl
    else
      let gl =
        usymv_gen ty ::
        usymf_genl ty ag_aux fuel ::
        ite_gen ag_aux fuel ty ::
        letin_gen ag_aux bvars fuel ty ::
        (qv_gen qvars ty)
      in
      let gl =
        match ty with
        | TFArray _ | Tadt _ -> gl
        | _ -> cst_gen ty :: gl
      in
      let tmp =
        ( match ty with
          | Tint ->
            List.rev_map
              (fun bop -> binop_gen ty fuel bop ag_aux)
              [ IAdd; ISub; IMul; IDiv; IMod(*; IPow*)]
          | Treal ->
            List.rev_map
              (fun bop -> binop_gen ty fuel bop ag_aux)
              [ RAdd; RSub; RMul; RDiv(*; RPow*)]
          | Tbool ->
            let l1 =
              List.rev_map
                (fun bop -> binop_gen ty fuel bop ag_aux)
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
          | TBitV len ->
            get_bv_gens ag_aux fuel len
          | TFArray {ti; tv} -> 
            [ get_fa_update ag_aux fuel ti tv ]
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
        if isform 
        then gl 
        else get_fa_access ag_aux fuel ty :: gl
      in
      let gl =
        if adts = []
        then gl
        else
          Cr.dynamic_bind
            (Cr.choose (List.rev_map Cr.const adts))
            ( fun adt ->
                pm_gen (ag_aux ~bvars) adt fuel ty
            ) :: gl
      in
      Cr.choose gl
  in 
  ag_aux max_depth ty

(********************************************************************)
let fdef_gen ?(fdefs = []) ?(adts : adt list = []) 
    name func_max_depth = 
  let ag () = 
    Cr.map [
      typ_gen (); typ_gen (); typ_gen ();
      typ_gen (); typ_gen (); typ_gen ();
    ] (
      fun t1 t2 t3 t4 t5 rtyp ->
        (t1, t2, t3, t4, t5), rtyp
    )
  in
  let fg () = 
    fun ((t1, t2, t3, t4, t5), rtyp) ->
      let a1, a2, a3, a4, a5 = 
        mk_tvar "a1" t1 ARG,
        mk_tvar "a2" t2 ARG,
        mk_tvar "a3" t3 ARG,
        mk_tvar "a4" t4 ARG,
        mk_tvar "a5" t5 ARG
      in 
      let gen =
        expr_gen ~qvars:false ~args:[a1; a2; a3; a4; a5] 
          ~fdefs ~adts func_max_depth rtyp
      in

      let ge =
        Cr.map [gen] ( 
          fun {g_res = body; u_args; u_bvars; u_dt; u_us; c_funcs} ->
            let atyp = 
              List.rev_map (
                fun v -> 
                  if VS.mem v u_args
                  then v
                  else {vname = ""; vty = TDummy; vk = US; id = 0}
              ) [a5; a4; a3; a2; a1]
            in
            { g_res = FuncDef {name; body; atyp; rtyp}; 
              u_args; u_bvars; u_dt; u_us; c_funcs}
        )
      in 
      ge
  in
  Cr.with_printer (pr_gr print_stmt) @@
  Cr.dynamic_bind (ag ()) (fg ())

let goal_gen ?(fdefs = []) ?(adts : adt list = []) 
    name query_max_depth =
  Cr.with_printer (pr_gr print_stmt) @@
  Cr.map 
    [expr_gen ~isform:true ~fdefs ~adts query_max_depth Tbool]
    ( fun {g_res; u_args; u_bvars; u_dt; u_us; c_funcs} ->
        let g_res =
          Goal 
            {name; body = quantify g_res}
        in 
        {g_res; u_args; u_bvars; u_dt; u_us; c_funcs}
    )

let axiom_gen ?(fdefs = []) ?(adts : adt list = []) name
    axiom_max_depth =
  Cr.with_printer (pr_gr print_stmt) @@
  Cr.map 
    [expr_gen ~isform:true ~fdefs ~adts axiom_max_depth Tbool]
    ( fun {g_res; u_args; u_bvars; u_dt; u_us; c_funcs} ->
        let g_res =
          Axiom 
            {name; body = quantify g_res}
        in 
        {g_res; u_args; u_bvars; u_dt; u_us; c_funcs}
    )

(********************************************************************)
let dk_gen =
  Cr.choose [Cr.const FD; Cr.const AxD]

let stmt_gen 
    ?(fdefs = []) ?(adts : adt list = []) ?(name = "") kind =
  match kind with 
  | FD ->
    fdef_gen ~fdefs ~adts name !func_max_depth
  | AxD ->
    axiom_gen ~fdefs ~adts name !axiom_max_depth 
  | GD ->
    goal_gen ~fdefs ~adts name !query_max_depth 

(********************************************************************)
let axid, gid, fid = ref 0, ref 0, ref 0

let mk_gen : 
  fd_info list -> adt list -> stmtkind -> stmt gen_res Cr.gen =
  fun fdefs adts sk ->
  let name = 
    match sk with 
    | GD -> "goal_"^string_of_int(incr gid; !gid)
    | AxD -> "ax_"^string_of_int(incr axid; !axid)  
    | FD -> "udf_"^string_of_int(incr fid; !fid) 
  in
  Cr.map [
    stmt_gen ~fdefs ~adts ~name sk
  ] (fun gres -> gres) 

let mk_fd_info fn (vs: tvar list) rtyp =
  { fn; rtyp; 
    params = 
      begin 
        match List.rev_map (fun x -> x.vty) vs with 
        | [v5; v4; v3; v2; v1] -> (v1, v2, v3, v4, v5)
        | _ -> assert false
      end
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
  fd_info list -> typedecl list -> SS.t -> 
  stmtkind list -> stmt_c list -> stmt_c list Cr.gen =
  fun fds adtl ocfs kndl acc ->
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
                List.find (fun (n, _) -> n = str) adtl
              in 
              TDS.add nt acc 
          ) u_dt TDS.empty 
        in
        let nfds = update_fdis fds gr in 
        let ncfs = SS.union c_funcs ocfs in 
        let stmtc = {stmt; tds; uss = u_us} in
        iter nfds adtl ncfs t (stmtc :: acc)
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
              List.find (fun (n, _) -> n = str) adtl
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
                          fun v -> not (is_dummy_tvar v)
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
  ) (
    fun ((adt1, adt2), (e1, e2)) -> 
      iter [] [adt1; adt2] SS.empty 
        [e1; e2; GD] []
  )
