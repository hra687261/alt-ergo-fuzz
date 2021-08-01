(* Translation to Alt-Ergo Expressions *)

open AltErgoLib
open Ast

module Sy = Symbols 
module ES = Expr.Set

type t = Commands.sat_tdecl

let rec typ_to_ty typ = 
  match typ with  
  | Tint -> Ty.Tint
  | Treal -> Ty.Treal
  | Tbool -> Ty.Tbool
  | TBitV n -> Ty.Tbitv n 
  | TFArray {ti; tv} ->
    Ty.Tfarray (typ_to_ty ti, typ_to_ty tv)
  | TDummy -> assert false
  (* we suppose that the algebraic data type is defined *)
  | Tadt (name, origb) -> 
    let body = 
      Some (
        List.map (
          fun (x, l) ->
            x,
            List.map (
              fun (y, fldt) -> 
                y, typ_to_ty fldt
            ) l
        ) origb
      )
    in 
    Ty.t_adt ~body name []

(* quantifier Id to give each quantifier a unique name *)
let ite_id = ref 0
let reset_cnt () = 
  ite_id := 0

(* from AltErgoParsers.Psmt2_to_alt_ergo *)
let better_num_of_string s =
  begin match String.split_on_char '.' s with
    | [n] | [n;""] -> Num.num_of_string n
    | [n; d] ->
      let l = String.length d in
      let n = if (String.length n) = 0 then Num.Int 0
        else Num.num_of_string n in
      let d = Num.num_of_string d in
      let e = Num.power_num (Num.Int 10) (Num.Int l) in
      Num.add_num n (Num.div_num d e)
    | _ -> assert false
  end

(** Translates an expr to an Expr.t *)
let name_tag = ref 0 

let rec translate_expr ?(name_base = "") ?(vars = VM.empty) ?(toplevel = false) ~stmtkind expr = 
  match expr with 
  | Cst (CstI x) -> 
    Expr.int (Int.to_string x)
  | Cst (CstR x) -> 
    Expr.real (
      Num.string_of_num (
        better_num_of_string (
          Float.to_string x
        )
      )
    )
  | Cst (CstB true) ->
    Expr.vrai
  | Cst (CstB false) ->
    Expr.faux

  | Cst (CstBv b) ->
    Expr.bitv b.bits (Ty.Tbitv b.length)

  | Unop (Neg, x) -> translate_expr ~vars ~stmtkind x
  | Unop (Not, x) -> 
    Expr.neg (translate_expr ~vars ~stmtkind x)

  | Unop (Access { ty = _, tv; fa}, i) -> 
    let fa' = translate_expr fa ~vars ~stmtkind in
    let i' = translate_expr i ~vars ~stmtkind in
    Expr.mk_term 
      (Sy.Op Sy.Get) 
      [fa'; i'] 
      (typ_to_ty tv)

  | FAUpdate { ty = (ti, tv); fa; i; v}-> 
    let fa' = translate_expr fa ~vars ~stmtkind in
    let i' = translate_expr i ~vars ~stmtkind in
    let v' = translate_expr v ~vars ~stmtkind in
    Expr.mk_term 
      (Sy.Op Sy.Set) 
      [fa'; i'; v'] 
      (typ_to_ty (TFArray {ti; tv}))

  | Binop (Concat n, x, y) ->
    let x' = translate_expr ~vars ~stmtkind x in 
    let y' = translate_expr ~vars ~stmtkind y in 
    Expr.mk_term (Sy.Op Sy.Concat) [x'; y'] (Ty.Tbitv n)

  | Unop (Extract {l; r}, b) -> 
    let l' = Expr.int (Int.to_string l) in 
    let r' = Expr.int (Int.to_string r) in 
    let b' = translate_expr ~vars ~stmtkind b in 
    Expr.mk_term (Sy.Op Sy.Extract) [b'; l'; r'] (Ty.Tbitv (r-l))

  | Binop (((And | Or | Xor) as op), x, y) ->
    let x' = translate_expr ~vars ~stmtkind x in 
    let y' = translate_expr ~vars ~stmtkind y in 
    begin 
      match op with 
      | And -> Expr.mk_and x' y' false 0 
      | Or -> Expr.mk_or x' y' false 0
      | Xor -> Expr.mk_xor x' y' 0
      | _-> assert false 
    end

  | Binop (Imp, x, y) ->
    Expr.mk_imp 
      (translate_expr ~vars ~stmtkind x) 
      (translate_expr ~vars ~stmtkind y) 0
  | Binop (Iff, x, y) ->
    Expr.mk_eq ~iff:true 
      (translate_expr ~vars ~stmtkind x) 
      (translate_expr ~vars ~stmtkind y)

  | Binop ((Lt | Le | Gt | Ge) as op , x, y) ->
    let x' = translate_expr ~vars ~stmtkind x in 
    let y' = translate_expr ~vars ~stmtkind y in 
    let sy, x'', y'' = 
      begin
        match op with 
        | Lt -> Sy.LT, x', y'
        | Le -> Sy.LE, x', y'
        | Gt -> Sy.LT, y', x'
        | Ge -> Sy.LE, y', x'
        | _ -> assert false 
      end
    in 
    Expr.mk_builtin ~is_pos:true sy [x''; y'']

  | Binop (Eq, x, y) ->
    Expr.mk_eq ~iff:true
      (translate_expr ~vars ~stmtkind x) 
      (translate_expr ~vars ~stmtkind y)
  | Binop (Neq, x, y) ->
    Expr.mk_distinct ~iff:true 
      [ translate_expr ~vars ~stmtkind x; 
        translate_expr ~vars ~stmtkind y]

  | Binop (((IAdd | ISub | IMul | IDiv | IPow | IMod) as op), x, y) ->
    let x' = translate_expr ~vars ~stmtkind x in 
    let y' = translate_expr ~vars ~stmtkind y in 
    Expr.mk_term
      begin
        match op with 
        | IAdd -> (Sy.Op Sy.Plus)
        | ISub -> (Sy.Op Sy.Minus)
        | IMul -> (Sy.Op Sy.Mult)
        | IDiv -> (Sy.Op Sy.Div)
        | IPow -> (Sy.Op Sy.Pow)
        | IMod -> (Sy.Op Sy.Modulo)
        | _ -> assert false 
      end
      [x'; y'] Ty.Tint

  | Binop (((RAdd | RSub | RMul | RDiv | RPow) as op), x, y) ->
    let x' = translate_expr ~vars ~stmtkind x in 
    let y' = translate_expr ~vars ~stmtkind y in 
    Expr.mk_term
      begin
        match op with 
        | RAdd -> (Sy.Op Sy.Plus)
        | RSub -> (Sy.Op Sy.Minus)
        | RMul -> (Sy.Op Sy.Mult)
        | RDiv -> (Sy.Op Sy.Div)
        | RPow -> (Sy.Op Sy.Pow)
        | _ -> assert false 
      end
      [x'; y'] Ty.Treal

  | FunCall {fname; rtyp; args; _} ->
    Expr.mk_term 
      (Sy.Name (Hstring.make fname, Sy.Other)) 
      (List.map (translate_expr ~vars ~stmtkind) args)
      (typ_to_ty rtyp)

  | Var {vname; vty; vk = US; _} -> 
    Expr.mk_term 
      (Sy.Name (Hstring.make vname, Sy.Other)) 
      [] (typ_to_ty vty)

  | Var ({vk = (ARG | EQ | UQ | BLI); _ } as v) -> 
    let sy, ty = VM.find v vars in 
    Expr.mk_term sy [] ty

  | Exists {qvars = vs; body; _} 
  | Forall {qvars = vs; body; _} ->
    let qvars, vars = 
      VS.fold ( 
        fun v (vl,vm) -> 
          let ty = typ_to_ty v.vty in
          let hsv = Hstring.make v.vname in 
          let var = Var.of_hstring hsv in
          let sy = Sy.Var var in 
          (sy, ty) :: vl,
          VM.add v (sy,ty) vm
      ) vs ([], vars)
    in
    let qve = 
      List.fold_left 
        ( fun acc (sy, ty) ->
            let e = Expr.mk_term sy [] ty in
            ES.add e acc
        ) ES.empty qvars
    in 

    let binders = Expr.mk_binders qve in 
    let triggers = [] (* ??? *) in
    let qbody = Expr.purify_form @@
      translate_expr ~vars ~stmtkind body
    in
    let name =
      let n = !name_tag in 
      incr name_tag;
      if n = 0 then name_base
      else 
        Format.sprintf "#%s#sub-%d" name_base n
    in
    (*
    Format.printf "\nX{%d %s}X@." !name_tag name;
    *)
    begin 
      match expr with 
      | Forall _ -> Expr.mk_forall
      | Exists _ -> Expr.mk_exists
      | _ -> assert false 
    end
      name
      Loc.dummy 
      binders
      triggers 
      qbody 
      (-42) 
      ~toplevel
      ~decl_kind:stmtkind

  | ITE {cond; cons; alt; _} ->
    let cond' = 
      translate_expr ~vars ~stmtkind:Expr.Daxiom cond 
    in 
    let cons' = translate_expr ~vars ~stmtkind cons in 
    let alt' = translate_expr ~vars ~stmtkind alt in 
    Expr.mk_ite cond' cons' alt' (incr ite_id; !ite_id)

  | LetIn (v, e, b) ->
    let rec get_bindings acc expr = 
      match expr with
      | LetIn (v, e, b) -> get_bindings ((v, e) :: acc) b
      | _ -> acc, expr 
    in
    let bds, rb = get_bindings [v, e] b in 

    let binders, vars = 
      List.fold_left (
        fun (bindings, vars) (v, e) -> 
          let sy = Sy.var (Var.of_string v.vname) in 
          let ty = typ_to_ty v.vty in
          let vars = VM.add v (sy, ty) vars in 
          let expr = 
            translate_expr ~vars ~stmtkind e
          in 
          (sy, expr) :: bindings, vars
      ) ([], vars) (List.rev bds)
    in 
    List.fold_left
      (fun acc (sy, e) ->
         Expr.mk_let sy e acc 0
      )
      (translate_expr ~vars ~stmtkind rb) 
      binders

  | PMatching {mtchdv; patts; _} ->
    let e = translate_expr ~vars ~stmtkind mtchdv in
    let pats = 
      List.rev_map (fun {destrn; pattparams; mbody} ->
          let args, vars = 
            List.fold_left ( 
              fun (acc1, acc2) v ->
                match v with 
                | Some {vname; vty; vk; id} ->
                  let hs = Hstring.make vname in 
                  let var = Var.of_hstring hs in
                  let sy = Sy.Var var in 
                  let ty = typ_to_ty vty in
                  (var, hs, ty) :: acc1, 
                  VM.add {vname; vty; vk; id} (sy, ty) acc2
                | None ->
                  (acc1, acc2)
            ) ([], vars) pattparams
          in
          let x = 
            Typed.Constr 
              {name = Hstring.make destrn; args}
          in
          let te =
            translate_expr ~vars ~stmtkind mbody
          in
          x, te
        ) (List.rev patts)
    in 
    Expr.mk_match e pats

  | Cstr {cname; cty; params} ->
    let sy = 
      Sy.Op (Sy.Constr (Hstring.make cname))
    in
    let exprs =
      List.rev_map (
        fun (_, a) ->
          translate_expr ~vars ~stmtkind a
      ) (List.rev params)
    in
    Expr.mk_term sy exprs (typ_to_ty cty)

  | Dummy -> assert false 

(** Translates a stmt to a Commands.sat_tdecl *)
let translate_stmt stmt = 
  name_tag := 0;
  match stmt with 
  | Axiom {name; body} ->
    let ff = 
      translate_expr ~name_base:name ~toplevel:true 
        ~stmtkind:Expr.Daxiom body
    in 
    assert (Sy.Map.is_empty (Expr.free_vars ff Sy.Map.empty));
    let ff = Expr.purify_form ff in
    let ff = 
      if Ty.Svty.is_empty (Expr.free_type_vars ff) 
      then ff
      else
        let id = Expr.id ff in
        Expr.mk_forall 
          name Loc.dummy Symbols.Map.empty [] ff 
          id ~toplevel:true ~decl_kind:Expr.Daxiom
    in 
    Commands.{ 
      st_loc = Loc.dummy;
      st_decl = Assume (name, ff, true)}

  | Goal {name; body} ->
    let ff = 
      begin 
        let rec rm_root_uqs ?(vars = VM.empty) body =
          match body with 
          | Forall {qvars; body; _} -> 
            let vars =
              VS.fold (
                fun x vm -> 
                  let ty = typ_to_ty x.vty in
                  let hsv = Hstring.make x.vname in 
                  let sy = Sy.Name (hsv, Sy.Other) in 
                  VM.add x (sy,ty) vm
              ) qvars vars
            in 
            rm_root_uqs body ~vars
          | _ -> body, vars
        in
        let body, vars = rm_root_uqs body in 
        translate_expr ~vars ~toplevel:true ~stmtkind:Expr.Dgoal 
          (Unop (Not, body))
      end
    in 
    assert (Sy.Map.is_empty (Expr.free_vars ff Sy.Map.empty));
    let ff = Expr.purify_form ff in
    let ff = 
      if Ty.Svty.is_empty (Expr.free_type_vars ff) 
      then ff
      else
        let id = Expr.id ff in
        Expr.mk_forall "" Loc.dummy Symbols.Map.empty [] ff id ~toplevel:true ~decl_kind:Expr.Dgoal
    in
    Commands.{ 
      st_loc = Loc.dummy;
      st_decl = Query (name, ff, Typed.Thm)}

  | FuncDef fdef ->
    let mk_assume name e = 
      Commands.Assume (name, e, true) 
    in 
    let mk_preddef name e = 
      Commands.PredDef (e, name) 
    in 

    (*Function signature *)
    let fsy = Sy.Name (Hstring.make fdef.name, Sy.Other) in
    let fty = typ_to_ty fdef.rtyp in 
    let vars, es, xs_ =
      List.fold_left 
        ( fun (vs, es, exps) v -> 
            let var = Var.of_string v.vname in 
            let vsy = Sy.Var var in
            let ty = typ_to_ty v.vty in 
            let exp = Expr.mk_term vsy [] ty in
            VM.add v (vsy, ty) vs, 
            ES.add exp es,
            exp :: exps
        )
        (VM.empty, ES.empty, [])
        fdef.atyp 
    in

    let xs = List.rev xs_ in 
    let fsign = Expr.mk_term fsy xs fty in 

    let stmtkind, mk_func = 
      begin 
        match fdef.rtyp with 
        | Tbool -> Expr.Dpredicate fsign, mk_preddef
        | _ -> Expr.Dfunction fsign, mk_assume
      end
    in 

    let fbody =
      translate_expr ~vars ~toplevel:true ~stmtkind fdef.body
    in 

    let lem = Expr.mk_eq ~iff:true fsign fbody in
    let binders = Expr.mk_binders es in

    let ret = 
      Expr.mk_forall 
        fdef.name Loc.dummy binders [] lem (-42) 
        ~toplevel:true ~decl_kind:stmtkind
    in
    assert (Sy.Map.is_empty (Expr.free_vars ret Sy.Map.empty));
    let ff = Expr.purify_form ret in
    let ret = 
      if Ty.Svty.is_empty (Expr.free_type_vars ff) 
      then ff
      else
        let id = Expr.id ff in
        Expr.mk_forall fdef.name Loc.dummy binders [] ff id ~toplevel:true ~decl_kind:stmtkind
    in

    Commands.{
      st_loc = Loc.dummy;
      st_decl = mk_func fdef.name ret}

(** Printing a list of declarations in Alt-Ergo's native format *)

let print_binop fmt binop =
  Format.fprintf fmt (
    match binop with 
    | And -> "and"   | Or -> "or"     | Xor -> "xor"
    | Imp -> "->"    | Iff-> "<->"

    | Lt -> "<"      | Le -> "<="     | Gt -> ">"
    | Ge -> ">="     | Eq -> "="      | Neq -> "<>" 
    | IPow -> "**"   | RPow -> "**."  | IMod -> "%%"    

    | RAdd | IAdd -> "+"   
    | RSub | ISub -> "-"   
    | RMul | IMul -> "*" 
    | RDiv | IDiv -> "/"

    | Concat _ -> "%@")

let rec print_expr fmt expr = 
  match expr with 
  | Cst (CstI x) ->
    Format.fprintf fmt "%s" 
      (Int.to_string x)
  | Cst (CstR x) ->
    Format.fprintf fmt "%s" (Float.to_string x)
  | Cst (CstB x) ->
    Format.fprintf fmt "%b" x
  | Cst (CstBv x) ->
    Format.fprintf fmt "[|%s|]" x.bits

  | Var {vname; _} ->
    Format.fprintf fmt "%s" vname
  | Unop (Neg, expr) ->
    Format.fprintf fmt "(- %a)" print_expr expr 
  | Unop (Not, expr) ->
    Format.fprintf fmt "(not %a)" print_expr expr 
  | Unop (Extract {l;r}, expr) ->
    Format.fprintf fmt "%a^{%d,%d}" 
      print_expr expr l r 
  | Unop (Access {ty = _; fa} , i) -> 
    Format.fprintf fmt "%a[%a]" print_expr fa print_expr i

  | Binop (binop, x, y) ->
    Format.fprintf fmt "(%a %a %a)" print_expr x print_binop binop print_expr y 

  | ITE {cond; cons; alt; _} -> 
    Format.fprintf fmt 
      "(if %a then %a else %a)" 
      print_expr cond print_expr cons print_expr alt 

  | LetIn ({vname;_}, e, b) -> 
    Format.fprintf fmt 
      "(let %s = %a in %a)" 
      vname print_expr e print_expr b 

  | FAUpdate {ty = _; fa; i; v} -> 
    Format.fprintf fmt "%a[%a <- %a]" 
      print_expr fa print_expr i print_expr v;

  | FunCall {fname; args; _} -> 
    Format.fprintf fmt "%s(" fname;
    begin match args with 
      | h :: t ->
        Format.fprintf fmt "%a" print_expr h;
        List.iter (
          fun x ->
            Format.fprintf fmt ", %a" print_expr x
        ) t
      | [] -> ()
    end;
    Format.fprintf fmt ")";

  | Forall {qvars; body; _} ->
    Format.fprintf fmt "(forall ";

    let _ = 
      VS.fold (
        fun {vname; vty; _} notfirst ->
          if notfirst 
          then (
            Format.fprintf fmt ", %s: %a"
              vname print_typ vty; true
          ) else (
            Format.fprintf fmt "%s: %a"
              vname print_typ vty; true
          )
      ) qvars false 
    in
    Format.fprintf fmt ". %a)" print_expr body

  | Exists {qvars; body; _} -> 
    Format.fprintf fmt "(exists ";

    let _ = 
      VS.fold (
        fun {vname; vty; _} notfirst ->
          if notfirst 
          then (
            Format.fprintf fmt ", %s: %a"
              vname print_typ vty; true
          ) else (
            Format.fprintf fmt "%s: %a"
              vname print_typ vty; true
          )
      ) qvars false 
    in
    Format.fprintf fmt ". %a)" print_expr body

  | PMatching {mtchdv; patts; _} -> 
    Format.fprintf fmt "(match %a with%a\nend)" print_expr mtchdv
      ( fun fmt patts ->
          List.iter (
            fun {destrn; pattparams; mbody} ->
              Format.fprintf fmt "\n\t| %s%a -> %a" 
                destrn
                ( fun fmt l ->
                    match l with 
                    | [] -> ()
                    | Some {vname; _}::t -> 
                      Format.fprintf fmt " (%s" vname;
                      List.iter (
                        fun v ->
                          match v with 
                          | Some {vname; _} ->
                            Format.fprintf fmt ", %s" vname
                          | None -> 
                            Format.fprintf fmt ", _"
                      ) t;
                      Format.fprintf fmt ")"
                    | None::t -> 
                      Format.fprintf fmt " (_";
                      List.iter (
                        fun v ->
                          match v with 
                          | Some {vname; _} ->
                            Format.fprintf fmt ", %s" vname
                          | None -> 
                            Format.fprintf fmt ", _"
                      ) t;
                      Format.fprintf fmt ")"
                ) pattparams
                print_expr mbody
          ) patts
      ) patts

  | Cstr {cname; params; _} ->
    Format.fprintf fmt "(%s%a)"
      cname
      ( fun fmt l ->
          match l with 
          | [] -> ()
          | (_, a)::t -> 
            Format.fprintf fmt " (%a" print_expr a;
            List.iter (
              fun (_, a) ->
                Format.fprintf fmt ", %a" print_expr a
            ) t;
            Format.fprintf fmt ")"
      ) params

  | Dummy -> assert false

let print_ss fmt ss = 
  ignore (
    SS.fold (
      fun s notfst ->
        if notfst 
        then (Format.fprintf fmt ", %s" s; true)
        else (Format.fprintf fmt "%s" s; true)
    ) ss false
  )

let print_tcm fmt (tcm: SS.t TCM.t) = 
  TCM.iter (
    fun gt ss -> 
      if (not (SS.is_empty ss)) then 
        Format.fprintf fmt "logic %a: %a@."
          print_ss ss
          print_typc gt
  ) tcm 

let print_tvar_list fmt atyp =
  match atyp with
  | {vname; vty; _} :: t ->
    Format.fprintf fmt "%s: %a" vname print_typ vty;
    List.iter (
      fun {vname; vty; _} -> 
        Format.fprintf fmt ", %s: %a"
          vname print_typ vty
    ) t
  | [] -> assert false

let print_typedecl fmt (tyd: typedecl) = 
  let pr_aux = 
    fun fmt prl ->
      match prl with 
      | [] -> ()
      | (n, typ) :: t -> 
        Format.fprintf fmt 
          "of {%s: %a"
          n print_typ typ;
        List.iter (
          fun (n, typ) -> 
            Format.fprintf fmt 
              "; %s: %a"
              n print_typ typ;
        ) t;
        Format.fprintf fmt "}"
  in
  let tyn, ptrns = tyd in 
  Format.fprintf fmt "type %s = %a@." tyn
    ( fun fmt ptrns ->
        match ptrns with 
        | (pn, prl) :: t ->
          Format.fprintf fmt "\n  %s %a" 
            pn pr_aux prl;
          List.iter (
            fun (pn, prl) -> 
              Format.fprintf fmt "\n  | %s %a" 
                pn pr_aux prl
          ) t
        | _ -> assert false
    ) ptrns

let print_typedecls fmt (tydecls: TDS.t) = 
  TDS.iter (
    fun td -> 
      print_typedecl fmt td
  ) tydecls

let print_stmt fmt (stmt: stmt) =
  match stmt with 
  | Axiom {name; body} ->
    Format.fprintf fmt "axiom %s:\n%a@." name print_expr body
  | Goal {name; body} ->
    Format.fprintf fmt "goal %s:\n%a@." name print_expr body
  | FuncDef {name; body; atyp; rtyp} -> 
    match rtyp with 
    | Tbool -> 
      Format.fprintf fmt "predicate %s(%a) =\n%a@."
        name
        print_tvar_list atyp
        print_expr body
    | _ -> 
      Format.fprintf fmt "function %s(%a):%a =\n%a@." 
        name
        print_tvar_list atyp
        print_typ rtyp
        print_expr body

let print_stmts fmt (scs: stmt_c list) =
  ignore @@
  List.fold_left (
    fun (dtds, duss) {stmt; tds; uss} ->
      let atds, tptds = 
        TDS.fold (
          fun td (atds, tptds) ->
            if TDS.mem td atds 
            then (atds, tptds)
            else (TDS.add td atds, TDS.add td tptds)
        ) tds (dtds, TDS.empty)
      in 
      let auss, tpuss =
        TCM.fold (
          fun tc s (atcm, tpuss) ->
            let nass, ntpss = 
              match TCM.find_opt tc atcm with 
              | Some ss -> 
                SS.union s ss, 
                SS.filter (fun n -> not (SS.mem n ss)) s
              | None -> s, s
            in
            TCM.add tc nass atcm, 
            TCM.add tc ntpss tpuss
        ) uss (duss, TCM.empty)
      in 
      if not (TDS.is_empty tptds) then 
        Format.fprintf fmt "\n%a@."
          print_typedecls tptds;
      if not (TCM.is_empty tpuss) then 
        Format.fprintf fmt "\n%a@."
          print_tcm tpuss;
      Format.fprintf fmt "\n%a@." 
        print_stmt stmt;
      atds, auss
  ) (TDS.empty, TCM.empty) scs
