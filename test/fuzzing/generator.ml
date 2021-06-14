open Ast
module Cr = Crowbar 
module FCS = Set.Make(String)

type ast_gen_res = 
  { gast : ast; 
    usedargs : VS.t; 
    calledfuncs: FCS.t}
type cmd_gen_res = 
  { gcmd : cmd; 
    calledfuncs: FCS.t}

type declkind = (* declaration kind *) 
  | FD (* function declaration *)
  | AxD (* axiom declaration *)
  | GD (* goal declaration *)

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
  { gast = Dummy; 
    usedargs = VS.empty; 
    calledfuncs = FCS.empty}

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
                ) ty EQ
            in  
            {gast; usedargs = VS.empty; calledfuncs = FCS.empty}
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
                usedargs = 
                  VS.union fa.usedargs i.usedargs;
                calledfuncs = 
                  FCS.union fa.calledfuncs i.calledfuncs}
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
          usedargs = 
            VS.union fa.usedargs 
              (VS.union i.usedargs v.usedargs); 
          calledfuncs = 
            FCS.union fa.calledfuncs 
              (FCS.union i.calledfuncs v.calledfuncs); }
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
                 usedargs = b.usedargs; 
                 calledfuncs = b.calledfuncs}
            ));
    (* Concat *)
    Cr.dynamic_bind 
      (Cr.range ~min:1 (len - 1))
      (fun n -> 
         Cr.map 
           [ gen (fuel - 1) (TBitV n);
             gen (fuel - 1) (TBitV (len - n))] 
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

(********************************************************************)
let generate_ast ?(isform = false) ?(qvars = true) ?(args = []) 
    ?(fdefs: fd_info list = []) max_depth ty =
  let rec ag_aux fuel ty = 
    if fuel <= 0 
    then
      begin 
        Cr.choose (
          ( match ty with 
            | TFArray _ -> []
            | _ -> [cst_gen ty]
          ) @ ( 
            usymv_gen ty ::
            qv_gen qvars ty @  
            get_arg_gens ty args
          )
        )
      end
    else 
      Cr.choose (
        ( match ty with 
          | TFArray _ -> []
          | _ -> [cst_gen ty]
        ) @ ( 
          usymv_gen ty :: 

          usymf_genl ty ag_aux fuel ::

          ( if isform 
            then []  
            else [get_fa_access ag_aux fuel ty]) @

          (qv_gen qvars ty) @

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
              get_bv_gens ag_aux fuel len
            | TFArray {ti; tv} -> 
              [ 
                get_fa_update ag_aux fuel ti tv
              ]
            | TDummy -> assert false 
          )
        )
      )
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
        generate_ast ~qvars:false ~args:atyp ~fdefs func_max_depth rtyp
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

let goal_gen ?(fdefs = []) query_max_depth =
  Cr.with_printer pr_gcr @@
  Cr.map 
    [generate_ast ~isform:true ~fdefs query_max_depth Tbool]
    ( fun x ->
        let gcmd =
          Goal {
            name = "goal_" ^ (incr gid; string_of_int !gid);
            body = quantify x.gast}
        in 
        {gcmd; calledfuncs = x.calledfuncs}
    )

let axiom_gen ?(fdefs = []) axiom_max_depth =
  Cr.with_printer pr_gcr @@
  Cr.map 
    [generate_ast ~isform:true ~fdefs axiom_max_depth Tbool]
    ( fun x ->
        let gcmd =
          Axiom {
            name = "ax_" ^ (incr axid; string_of_int !axid);
            body = quantify x.gast}
        in 
        {gcmd; calledfuncs = x.calledfuncs}
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

let generate_decl ?(fdefs = []) ?(name = "") kind =
  match kind with 
  | FD ->
    fdef_gen ~fdefs name func_max_depth
  | AxD ->
    axiom_gen ~fdefs axiom_max_depth 
  | GD ->
    goal_gen ~fdefs query_max_depth 

(********************************************************************)
let mk_gen : fd_info list -> declkind -> cmd_gen_res Cr.gen =
  fun fdefs e ->
  Cr.map [
    generate_decl ~fdefs 
      ~name:("udf_"^string_of_int(incr fid; !fid)) e
  ] (fun gres -> gres) 

let mk_fd_info fn (vs: tvar list) rtyp =
  { params = begin 
        match List.map (fun x -> x.vty) vs with 
        | [v1; v2; v3; v4; v5] -> (v1, v2, v3, v4, v5)
        | _ -> assert false
      end; fn; rtyp
  }

let get_fdis : fd_info list -> cmd_gen_res -> fd_info list =
  fun fdefs  {gcmd; _} ->
  match gcmd with
  | FuncDef {name; atyp; rtyp; _} -> 
    if (List.for_all (fun x -> x.vty == TDummy)) atyp 
    then fdefs
    else 
      let fdi = mk_fd_info name atyp rtyp in 
      fdefs @ [fdi]
  | _ -> fdefs

let rec iter : 
  fd_info list -> FCS.t -> declkind list -> cmd_gen_res list 
  -> cmd list Cr.gen =
  fun fds cfs el acc ->
  match el with 
  | h :: t ->
    let a : cmd_gen_res Cr.gen = mk_gen fds h in 
    let b : cmd_gen_res -> cmd list Cr.gen = 
      fun x -> 
        let fds = get_fdis fds x in 
        let cfs = FCS.union cfs x.calledfuncs in
        iter fds cfs t (x :: acc)
    in
    Cr.dynamic_bind a b
  | _ -> liter fds cfs acc

and liter : 
  fd_info list -> FCS.t -> cmd_gen_res list -> cmd list Cr.gen =
  fun fds cfs acc -> 
  Cr.dynamic_bind 
    (mk_gen fds GD) 
    ( fun fg ->
        let cfs = FCS.union cfs fg.calledfuncs in 
        let decls = 
          List.fold_right (
            fun {gcmd; _} acc ->
              match gcmd with 
              | FuncDef ({name; atyp; _} as f) -> (
                  if FCS.mem name cfs
                  then 
                    FuncDef 
                      {f with 
                       atyp = 
                         List.filter 
                           (fun v -> 
                              not (is_dummy_tvar v)) atyp} :: acc
                  else acc
                )
              | _ -> gcmd :: acc
          ) (List.rev (fg :: acc)) []
        in 
        Cr.const decls
    )

(********************************************************************)
let gen_decls = 
  Cr.dynamic_bind (
    Cr.map 
      [dk_gen; dk_gen; dk_gen; dk_gen; dk_gen] 
      (fun e1 e2 e3 e4 e5 -> e1, e2, e3, e4, e5)
  ) @@ (
    fun (e1, e2, e3, e4, e5) -> 
      iter [] FCS.empty [e1; e2; e3; e4; e5; GD] []
  )
