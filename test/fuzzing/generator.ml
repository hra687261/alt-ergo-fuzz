open Ast
module Cr = Crowbar 

type ast_gen_res = 
  { gast : ast; 
    u_args : VS.t; 
    u_bvars : VS.t; 
    c_funcs : SS.t}

type decl_gen_res = 
  { gdecl : decl; 
    c_funcs : SS.t}

type declkind = (* declaration kind *) 
  | FD (* function declaration *)
  | AxD (* axiom declaration *)
  | GD (* goal declaration *)

let mk_empty_agr gast =
  { gast; 
    u_args = VS.empty;
    u_bvars = VS.empty; 
    c_funcs = SS.empty}

let pr_gar fmt {gast; _} =
  print fmt gast

let pr_gcr fmt {gdecl; c_funcs} =
  Format.fprintf fmt "{";  
  Format.fprintf fmt "\n  gdecl = \n    %a;" print_decl gdecl;
  Format.fprintf fmt "\n  cldf = \n    %a;" 
    (fun fmt e -> 
       SS.iter (Format.fprintf fmt "(%s)") e ) c_funcs;
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
  { gast = Dummy; 
    u_args = VS.empty; 
    u_bvars = VS.empty; 
    c_funcs = SS.empty}

let dest_gen (typ_gen: typ Cr.gen) id num =
  let dname = 
    Format.sprintf "E%d_%d" id num
  in
  Cr.map 
    [typ_gen; typ_gen; typ_gen; typ_gen; typ_gen]
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

let adt_gen typ_gen =
  let id = incr adt_id; !adt_id in
  Cr.map 
    [ dest_gen typ_gen id 1;
      dest_gen typ_gen id 2;
      dest_gen typ_gen id 3;
      dest_gen typ_gen id 4;
      dest_gen typ_gen id 5]
    ( fun d1 d2 d3 d4 d5 ->
        Format.sprintf "adt_%d" id, 
        [d1; d2; d3; d4; d5]
    )

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
         { gast; 
           u_args = VS.empty;
           u_bvars = VS.empty; 
           c_funcs = SS.empty})
  | Treal -> 
    Cr.map 
      [Cr.float] 
      (fun x -> 
         let gast = 
           Cst (CstR (
               if Float.is_nan x then 0. else x)) 
         in 
         mk_empty_agr gast)
  | Tbool -> 
    Cr.map 
      [Cr.bool] 
      (fun x -> 
         let gast =
           Cst (CstB x) in 
         mk_empty_agr gast)
  | TBitV n -> 
    Cr.map 
      [Cr.range ((pow 2 n)-1)] 
      (fun x -> 
         let gast =
           Cst (CstBv (int_to_bitv ~wl:n x)) 
         in 
         mk_empty_agr gast)
  | _ -> assert false

let binop_gen : 'a -> int -> binop -> 
  (int -> typ -> ast_gen_res Cr.gen) -> ast_gen_res Cr.gen =
  fun ty fuel bop gen ->
  Cr.map 
    [ gen (fuel - 1) ty;
      gen (fuel - 1) ty] 
    ( fun x y -> 
        let gast = 
          mk_binop bop x.gast y.gast in 
        { gast; 
          u_args = VS.union x.u_args y.u_args; 
          u_bvars = VS.union x.u_bvars y.u_bvars; 
          c_funcs = SS.union x.c_funcs y.c_funcs})   

let usymv_gen ty = 
  Cr.map 
    [Cr.range nb_us_vars] 
    ( fun pos -> 
        let gast =
          get_uvar_ast pos ty 
        in 
        mk_empty_agr gast)

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
          let args, u_args, u_bvars, c_funcs = 
            List.fold_left (
              fun (l, vs, bv, fcs) b -> 
                if is_dummy b.gast
                then l, vs, bv, fcs
                else 
                  b.gast :: l, 
                  VS.union b.u_args vs,
                  VS.union b.u_bvars bv,
                  SS.union b.c_funcs fcs
            ) ([], VS.empty, VS.empty, SS.empty) 
              [a1; a2; a3; a4; a5]
          in
          { gast = FunCall {
                fname = fn; fk = USF; 
                atyp = 
                  List.filter 
                    (fun x -> x <> TDummy) 
                    [p1; p2; p3; p4; p5];
                rtyp; args}; 
            u_args;
            u_bvars;  
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
            let gast =
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
            mk_empty_agr gast
        )
    ]
  else []

let get_arg_gens ty args =
  let l = List.filter (fun x -> ty = x.vty) args in
  if List.length l > 0 then 
    List.map 
      ( fun x -> 
          Cr.const (
            { gast = Var x; 
              u_args = VS.add x VS.empty; 
              u_bvars = VS.empty; 
              c_funcs = SS.empty})
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
        let args, u_args, u_bvars, c_funcs = 
          List.fold_left (
            fun (l, vs, bv,fcs) b -> 
              if is_dummy b.gast
              then 
                l, vs, bv, fcs
              else 
                ( b.gast :: l, 
                  VS.union b.u_args vs, 
                  VS.union b.u_bvars bv,  
                  SS.union b.c_funcs fcs)
          ) 
            ([], VS.empty, VS.empty, SS.add fname SS.empty) 
            [a1; a2; a3; a4; a5]
        in
        { gast = FunCall {
              fname; fk = UDF; 
              atyp = 
                List.filter 
                  (fun x -> x <> TDummy) 
                  [p1; p2; p3; p4; p5];
              rtyp; 
              args}; 
          u_args; 
          u_bvars;
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
    typ_gen
    ( fun ti -> 
        Cr.map 
          [ gen (fuel - 1) (TFArray {ti;tv});
            gen (fuel - 1) ti]
          ( fun fa i ->
              let gast =
                Unop (Access {ty = ti, tv; fa = fa.gast}, i.gast)
              in 
              { gast;
                u_args = 
                  VS.union fa.u_args i.u_args;
                u_bvars = 
                  VS.union fa.u_bvars i.u_bvars;
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
        let gast = 
          FAUpdate {
            ty = (ti, tv); 
            fa = fa.gast; 
            i = i.gast; 
            v = v.gast}
        in
        { gast; 
          u_args = 
            VS.union fa.u_args 
              (VS.union i.u_args v.u_args); 
          u_bvars = 
            VS.union fa.u_bvars 
              (VS.union i.u_bvars v.u_bvars); 
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
            (fun b -> 
               let gast = 
                 Unop (Extract {l; r}, b.gast)
               in
               { gast; 
                 u_args = b.u_args; 
                 u_bvars = b.u_bvars;
                 c_funcs = b.c_funcs}
            ));
    (* Concat *)
    Cr.dynamic_bind 
      (Cr.range ~min:1 (len - 1))
      (fun n -> 
         Cr.map 
           [ gen (fuel - 1) (TBitV n);
             gen (fuel - 1) (TBitV (len - n))] 
           (fun x y -> 
              let gast =
                Binop (Concat len, x.gast, y.gast)
              in
              {  gast; 
                 u_args = 
                   VS.union x.u_args y.u_args; 
                 u_bvars = 
                   VS.union x.u_bvars y.u_bvars; 
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
        { gast = 
            ITE {ty; 
                 cond = cond.gast; 
                 cons = cons.gast; 
                 alt = alt.gast}; 
          u_args = 
            VS.union cond.u_args 
              (VS.union cons.u_args alt.u_args); 
          u_bvars = 
            VS.union cond.u_bvars 
              (VS.union cons.u_bvars alt.u_bvars); 
          c_funcs =
            SS.union cond.c_funcs 
              (SS.union cons.c_funcs alt.c_funcs); 
        }
    )

let letin_gen : (?bvars:VS.t -> int -> typ -> ast_gen_res Cr.gen) ->
  VS.t -> int -> typ -> ast_gen_res Cr.gen =
  fun gen bvars fuel ty -> 
  let nv = mk_bound_var ty in
  Cr.map 
    [ gen ~bvars (fuel - 1) ty;
      gen ~bvars:(VS.add nv bvars) (fuel - 1) ty]
    (
      fun e b -> 
        { gast = 
            if VS.mem nv b.u_bvars 
            then b.gast 
            else LetIn (nv , e.gast, b.gast); 
          u_args = 
            VS.union e.u_args b.u_args; 
          u_bvars = 
            VS.union e.u_bvars b.u_bvars;
          c_funcs =
            SS.union e.c_funcs b.c_funcs; 
        }
    )

let pm_gen ast_gen (adts: adt list) fuel valty =
  let aux adt = 
    let _, ipts = adt in 
    let pts = 
      List.map (
        fun ((n, l): string * (string * typ) list) ->
          n, 
          List.map (
            fun (s, t) -> mk_tvar s t BLI
          ) l
      ) ipts
    in
    Cr.map [ 
      ast_gen fuel (Tadt adt); 
      ast_gen fuel (Tadt adt); 
      ast_gen fuel (Tadt adt); 
      ast_gen fuel (Tadt adt); 
      ast_gen fuel (Tadt adt); 
      ast_gen fuel (Tadt adt)
    ] (
      fun {gast = mtchdv; u_args; u_bvars; c_funcs} 
        pt1 pt2 pt3 pt4 pt5 ->
        let pbs = 
          [pt1; pt2; pt3; pt4; pt5]
        in
        let asts, u_args, u_bvars, c_funcs =
          List.fold_left (
            fun (asts, ua, ub, cf) 
              {gast; u_args; u_bvars; c_funcs} ->
              gast :: asts, 
              VS.union ua u_args,
              VS.union ub u_bvars,
              SS.union cf c_funcs
          ) ([], u_args, u_bvars, c_funcs) pbs
        in
        let patts =
          List.map2 (
            fun (destrn, pattparams) mbody ->
              {destrn; pattparams; mbody}
          ) pts asts
        in
        let gast = PMatching
            { mtchdv; 
              patts; 
              valty}
        in
        {gast; u_args; u_bvars; c_funcs}
    )
  in
  List.map aux adts

(********************************************************************)
let generate_ast ?(isform = false) ?(qvars = true) ?(args = []) 
    ?(fdefs: fd_info list = []) ?(adts : adt list = []) max_depth ty =
  ignore isform;
  let rec ag_aux ?(bvars = VS.empty) fuel ty = 
    if fuel <= 0 
    then
      begin 
        let gl =
          qv_gen qvars ty @
          usymv_gen ty ::
          get_arg_gens ty args
        in
        Cr.choose (
          match ty with 
          | TFArray _ -> gl

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
          | TFArray _ -> gl1
          | _ -> cst_gen ty :: gl1
        in
        let gl3 =
          get_arg_gens ty args
        in
        let gl4 =
          func_call_gen ag_aux fuel ty fdefs
        in
          (*
          ( if isform 
            then []  
            else [get_fa_access ag_aux fuel ty]) @
          *)
        let gl5 =
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
            (*
            | TFArray {ti; tv} -> 
              [ 
                get_fa_update ag_aux fuel ti tv
              ]
            *)
            | _ -> assert false
          )
        in
        let pm_gens = 
          if adts = []
          then []
          else pm_gen (ag_aux ~bvars) adts fuel ty
        in
        let gl6 =
          if pm_gens = []
          then []
          else [Cr.choose pm_gens]
        in
        let tmp = gl6 @ gl5 in
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
        generate_ast ~qvars:false ~args:atyp ~fdefs ~adts 
          func_max_depth rtyp
      in

      let ge =
        Cr.map [gen] ( 
          fun {gast = body; u_args; c_funcs; _} ->
            let atyp = 
              List.map (
                fun v -> 
                  if VS.mem v u_args
                  then v
                  else {vname = ""; vty = TDummy; vk = US; id = 0}
              ) atyp
            in
            {gdecl = FuncDef {name; body; atyp; rtyp}; c_funcs}
        )
      in 
      ge
  in
  Cr.with_printer pr_gcr @@
  Cr.dynamic_bind ag fg

let goal_gen ?(fdefs = []) ?(adts : adt list = []) 
    query_max_depth =
  Cr.with_printer pr_gcr @@
  Cr.map 
    [generate_ast ~isform:true ~fdefs ~adts query_max_depth Tbool]
    ( fun x ->
        let gdecl =
          Goal {
            name = "goal_" ^ (incr gid; string_of_int !gid);
            body = quantify x.gast}
        in 
        {gdecl; c_funcs = x.c_funcs}
    )

let axiom_gen ?(fdefs = []) ?(adts : adt list = []) 
    axiom_max_depth =
  Cr.with_printer pr_gcr @@
  Cr.map 
    [generate_ast ~isform:true ~fdefs ~adts axiom_max_depth Tbool]
    ( fun x ->
        let gdecl =
          Axiom {
            name = "ax_" ^ (incr axid; string_of_int !axid);
            body = quantify x.gast}
        in 
        {gdecl; c_funcs = x.c_funcs}
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

let generate_decl 
    ?(fdefs = []) ?(adts : adt list = []) ?(name = "") kind =
  match kind with 
  | FD ->
    fdef_gen ~fdefs ~adts name func_max_depth
  | AxD ->
    axiom_gen ~fdefs ~adts axiom_max_depth 
  | GD ->
    goal_gen ~fdefs ~adts query_max_depth 

(********************************************************************)
let mk_gen : fd_info list -> adt list -> declkind -> decl_gen_res Cr.gen =
  fun fdefs adts e ->
  Cr.map [
    generate_decl ~fdefs ~adts 
      ~name:("udf_"^string_of_int(incr fid; !fid)) e
  ] (fun gres -> gres) 

let mk_fd_info fn (vs: tvar list) rtyp =
  { params = begin 
        match List.map (fun x -> x.vty) vs with 
        | [v1; v2; v3; v4; v5] -> (v1, v2, v3, v4, v5)
        | _ -> assert false
      end; fn; rtyp
  }

let get_fdis : fd_info list -> decl_gen_res -> fd_info list =
  fun fdefs  {gdecl; _} ->
  match gdecl with
  | FuncDef {name; atyp; rtyp; _} -> 
    if (List.for_all (fun x -> x.vty == TDummy)) atyp 
    then fdefs
    else 
      let fdi = mk_fd_info name atyp rtyp in 
      fdefs @ [fdi]
  | _ -> fdefs

let rec iter : 
  fd_info list -> adt list -> SS.t -> declkind list -> 
  decl_gen_res list -> decl list Cr.gen =
  fun fds adts cfs el acc ->
  match el with 
  | h :: t ->
    let a : decl_gen_res Cr.gen = mk_gen fds [] h in 
    let b : decl_gen_res -> decl list Cr.gen = 
      fun x -> 
        let fds = get_fdis fds x in 
        let cfs = SS.union cfs x.c_funcs in
        iter fds adts cfs t (x :: acc)
    in
    Cr.dynamic_bind a b
  | _ -> liter fds adts cfs acc

and liter : 
  fd_info list -> adt list -> SS.t -> decl_gen_res list -> decl list Cr.gen =
  fun fds adts cfs acc -> 
  Cr.dynamic_bind (mk_gen fds adts GD) 
    ( fun fg ->
        let cfs = SS.union cfs fg.c_funcs in 
        let decls = 
          List.fold_right (
            fun {gdecl; _} acc ->
              match gdecl with 
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
              | _ -> gdecl :: acc
          ) (List.rev (fg :: acc)) []
        in 
        Cr.const decls
    )

(********************************************************************)

let _ = ignore (get_fa_update, get_fa_access, adt_gen, pm_gen)

(*
(* the more decls are generated the slower the fuzzing will be *)
let gen_decls = 
  Cr.dynamic_bind (
    Cr.map 
      [dk_gen; dk_gen; dk_gen; dk_gen; dk_gen] 
      (fun e1 e2 e3 e4 e5 -> e1, e2, e3, e4, e5)
  ) @@ (
    fun (e1, e2, e3, e4, e5) -> 
      iter [] SS.empty [e1; e2; e3; e4; e5; GD] []
  )
*)

let gen_decls = 
  Cr.dynamic_bind (
    Cr.map 
      [ dk_gen; 
        dk_gen] 
      (fun e1 e2 -> e1, e2)
  ) @@ (
    fun (e1, e2) -> 
      iter [] [] SS.empty [e1; e2] []
  )
