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
    let body = Some (
        List.map (
          fun (x, l) ->
            x,
            List.map (
              fun (y, fldt) -> 
                y, typ_to_ty fldt
            ) l
        ) origb)
    in 
    Ty.t_adt ~body name []

(** Translates an ast to an Expr.t *)
let rec translate_ast ?(vars = VM.empty) ?(toplevel = false) ~decl_kind ast = 
  match ast with 
  | Cst (CstI x) -> 
    Expr.int (Int.to_string x)
  | Cst (CstR x) -> 
    Expr.real (Float.to_string x)
  | Cst (CstB true) ->
    Expr.vrai
  | Cst (CstB false) ->
    Expr.faux

  | Cst (CstBv b) ->
    Expr.bitv b.bits (Ty.Tbitv b.length)

  | Unop (Neg, x) -> translate_ast ~vars ~decl_kind x
  | Unop (Not, x) -> 
    Expr.neg (translate_ast ~vars ~decl_kind x)

  | Unop (Access { ty = _, tv; fa}, i) -> 
    let fa' = translate_ast fa ~vars ~toplevel ~decl_kind in
    let i' = translate_ast i ~vars ~toplevel ~decl_kind in
    Expr.mk_term 
      (Sy.Op Sy.Get) 
      [fa'; i'] 
      (typ_to_ty tv)

  | FAUpdate { ty = (ti, tv); fa; i; v}-> 
    let fa' = translate_ast fa ~vars ~toplevel ~decl_kind in
    let i' = translate_ast i ~vars ~toplevel ~decl_kind in
    let v' = translate_ast v ~vars ~toplevel ~decl_kind in
    Expr.mk_term 
      (Sy.Op Sy.Set) 
      [fa'; i'; v'] 
      (typ_to_ty (TFArray {ti; tv}))

  | Binop (Concat n, x, y) ->
    let x' = translate_ast ~vars ~decl_kind x in 
    let y' = translate_ast ~vars ~decl_kind y in 
    Expr.mk_term (Sy.Op Sy.Concat) [x'; y'] (Ty.Tbitv n)

  | Unop (Extract {l; r}, b) -> 
    let l' = Expr.int (Int.to_string l) in 
    let r' = Expr.int (Int.to_string r) in 
    let b' = translate_ast ~vars ~decl_kind b in 
    Expr.mk_term (Sy.Op Sy.Extract) [b'; l'; r'] (Ty.Tbitv (r-l))

  | Binop (((And | Or | Xor) as op), x, y) ->
    let x' = translate_ast ~vars ~decl_kind x in 
    let y' = translate_ast ~vars ~decl_kind y in 
    begin 
      match op with 
      | And -> Expr.mk_and x' y' false 0 
      | Or -> Expr.mk_or x' y' false 0
      | Xor -> Expr.mk_xor x' y' 0
      | _-> assert false 
    end

  | Binop (Imp, x, y) ->
    Expr.mk_imp 
      (translate_ast ~vars ~decl_kind x) (translate_ast ~vars ~decl_kind y) 0
  | Binop (Iff, x, y) ->
    Expr.mk_eq ~iff:true 
      (translate_ast ~vars ~decl_kind x) (translate_ast ~vars ~decl_kind y)

  | Binop ((Lt | Le | Gt | Ge) as op , x, y) ->
    let x' = translate_ast ~vars ~decl_kind x in 
    let y' = translate_ast ~vars ~decl_kind y in 
    let is_pos, sy = 
      begin
        match op with 
        | Lt -> true, Sy.LT
        | Le -> true, Sy.LE
        | Gt -> false, Sy.LE
        | Ge -> false, Sy.LT
        | _ -> assert false 
      end
    in 
    Expr.mk_builtin ~is_pos sy [x'; y']

  | Binop (Eq, x, y) ->
    Expr.mk_eq ~iff:true
      (translate_ast ~vars ~decl_kind x) (translate_ast ~vars ~decl_kind y)
  | Binop (Neq, x, y) ->
    Expr.mk_distinct ~iff:true 
      [translate_ast ~vars ~decl_kind x; translate_ast ~vars ~decl_kind y]

  | Binop (((IAdd | ISub | IMul | IDiv | IPow | IMod) as op), x, y) ->
    let x' = translate_ast ~vars ~decl_kind x in 
    let y' = translate_ast ~vars ~decl_kind y in 
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
    let x' = translate_ast ~vars ~decl_kind x in 
    let y' = translate_ast ~vars ~decl_kind y in 
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
      (List.map (translate_ast ~vars ~decl_kind) args)
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
    let qbody = 
      translate_ast ~vars ~toplevel ~decl_kind body
    in
    begin 
      match ast with 
      | Forall _ -> Expr.mk_forall
      | Exists _ -> Expr.mk_exists
      | _ -> assert false 
    end
      ("q_"^(incr qid; string_of_int !qid))
      Loc.dummy 
      binders
      triggers 
      qbody 
      (-42) 
      ~toplevel
      ~decl_kind

  | ITE {cond; cons; alt; _} ->
    let cond' = 
      translate_ast ~vars 
        ~toplevel:false 
        ~decl_kind:Expr.Daxiom cond 
    in 
    let cons' = translate_ast ~vars ~toplevel ~decl_kind cons in 
    let alt' = translate_ast ~vars ~toplevel ~decl_kind alt in 
    Expr.mk_ite cond' cons' alt' 0

  | LetIn (v, e, b) ->
    let rec get_bindings acc ast = 
      match ast with
      | LetIn (v, e, b) -> get_bindings ((v, e) :: acc) b
      | _ -> acc, ast 
    in
    let bds, rb = get_bindings [v, e] b in 

    let binders, vars = 
      List.fold_right (
        fun (v, e) (bindings, vars) -> 
          let sy = Sy.var (Var.of_string v.vname) in 
          let ty = typ_to_ty v.vty in
          let vars = VM.add v (sy, ty) vars in 
          let expr = 
            translate_ast ~vars ~toplevel ~decl_kind e
          in 
          (sy, expr) :: bindings, vars
      ) bds ([], vars)
    in 
    List.fold_left
      (fun acc (sy, e) ->
         Expr.mk_let sy e acc 0
      )
      (translate_ast ~vars ~toplevel ~decl_kind rb) 
      binders

  | PMatching {mtchdv; patts; _} ->
    let e = translate_ast ~vars ~toplevel ~decl_kind mtchdv in
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
            Typed.Constr { 
              name = Hstring.make destrn; 
              args
            }
          in
          let te =
            translate_ast ~vars ~toplevel ~decl_kind mbody
          in
          x, te
        ) (List.rev patts)
    in 
    Expr.mk_match e pats

  | Cstr {cname; cty; params} ->
    let sy = 
      Sy.destruct ~guarded:true cname
    in
    let exprs =
      List.rev_map (
        fun (_, a) ->
          translate_ast ~vars ~toplevel ~decl_kind a
      ) (List.rev params)
    in
    Expr.mk_term sy exprs (typ_to_ty cty)

  | Dummy -> assert false 

(** Translates a decl to a Commands.sat_tdecl *)
let translate_decl decl = 
  match decl with 
  | Axiom {name; body} ->
    let decl_kind = Expr.Daxiom in
    let toplevel = true in 
    let ff = 
      translate_ast ~toplevel ~decl_kind body
    in 
    assert (Sy.Map.is_empty (Expr.free_vars ff Sy.Map.empty));
    let ff = Expr.purify_form ff in
    let ff = 
      if Ty.Svty.is_empty (Expr.free_type_vars ff) 
      then 
        ff
      else
        let id = Expr.id ff in
        Expr.mk_forall 
          name Loc.dummy Symbols.Map.empty [] ff 
          id ~toplevel ~decl_kind
    in 
    Commands.{ 
      st_loc = Loc.dummy;
      st_decl = Assume (name, ff, true)}

  | Goal {name; body} ->
    let decl_kind = Expr.Dgoal in
    let toplevel = true in 
    let ff = 
      begin 
        let rec rm_root_uqs ?(vars = VM.empty) body =
          match body with 
          | Forall {qvars; body; _} -> 
            let vars =
              VS.fold
                ( fun x vm -> 
                    let ty = typ_to_ty x.vty in
                    let hsv = Hstring.make x.vname in 
                    let sy = Sy.Name (hsv, Sy.Other) in 
                    VM.add x (sy,ty) vm)
                qvars vars
            in 
            rm_root_uqs body ~vars
          | _ -> body, vars
        in
        let body, vars = rm_root_uqs body in 
        translate_ast ~vars ~toplevel ~decl_kind (Unop (Not, body))
      end
    in 
    assert (Sy.Map.is_empty (Expr.free_vars ff Sy.Map.empty));
    let ff = Expr.purify_form ff in
    let ff = 
      if Ty.Svty.is_empty (Expr.free_type_vars ff) 
      then ff
      else
        let id = Expr.id ff in
        Expr.mk_forall name Loc.dummy Symbols.Map.empty [] ff id ~toplevel ~decl_kind
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

    let toplevel = true in 
    let decl_kind, mk_func = 
      begin 
        match fdef.rtyp with 
        | Tbool -> Expr.Dpredicate fsign, mk_preddef
        | _ -> Expr.Dfunction fsign, mk_assume
      end
    in 

    let fbody =
      translate_ast ~vars ~toplevel ~decl_kind fdef.body
    in 

    let lem = Expr.mk_eq ~iff:true fsign fbody in
    let binders = Expr.mk_binders es in

    let ret = 
      Expr.mk_forall 
        fdef.name Loc.dummy binders [] lem (-42) 
        ~toplevel ~decl_kind
    in
    assert (Sy.Map.is_empty (Expr.free_vars ret Sy.Map.empty));
    let ff = Expr.purify_form ret in
    let ret = 
      if Ty.Svty.is_empty (Expr.free_type_vars ff) 
      then ff
      else
        let id = Expr.id ff in
        Expr.mk_forall fdef.name Loc.dummy binders [] ff id ~toplevel:true ~decl_kind
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

let rec print_ast fmt ast = 
  match ast with 
  | Cst (CstI x) ->
    Format.fprintf fmt "%s" 
      (Int.to_string x)
  | Cst (CstR x) ->
    Format.fprintf fmt "%s" 
      (Float.to_string x)
  | Cst (CstB x) ->
    Format.fprintf fmt "%b" x
  | Cst (CstBv x) ->
    Format.fprintf fmt "[|%s|]" x.bits

  | Var {vname; _} ->
    Format.fprintf fmt "%s" vname
  | Unop (Neg, ast) ->
    Format.fprintf fmt "(- %a)" print_ast ast 
  | Unop (Not, ast) ->
    Format.fprintf fmt "(not %a)" print_ast ast 
  | Unop (Extract {l;r}, ast) ->
    Format.fprintf fmt "%a^{%d,%d}" 
      print_ast ast l r 
  | Unop (Access {ty = _; fa} , i) -> 
    Format.fprintf fmt "%a[%a]" print_ast fa print_ast i

  | Binop (binop, x, y) ->
    Format.fprintf fmt "(%a %a %a)" print_ast x print_binop binop print_ast y 

  | ITE {cond; cons; alt; _} -> 
    Format.fprintf fmt 
      "(if %a then %a else %a)" 
      print_ast cond print_ast cons print_ast alt 

  | LetIn ({vname;_}, e, b) -> 
    Format.fprintf fmt 
      "(let %s = %a in %a)" 
      vname print_ast e print_ast b 

  | FAUpdate {ty = _; fa; i; v} -> 
    Format.fprintf fmt "%a[%a <- %a]" 
      print_ast fa print_ast i print_ast v;

  | FunCall {fname; args; _} -> 
    Format.fprintf fmt "%s(" fname;
    begin match List.rev args with 
      | h :: t ->
        Format.fprintf fmt "%a" print_ast h;
        List.iter (
          fun x ->
            Format.fprintf fmt ", %a" print_ast x
        ) t
      | [] -> ()
    end;
    Format.fprintf fmt ")";

  | Forall {qvars; body; _} ->
    Format.fprintf fmt "forall ";

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
    Format.fprintf fmt ". %a " print_ast body

  | Exists {qvars; body; _} -> 
    Format.fprintf fmt "exists ";

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
    Format.fprintf fmt ". %a " print_ast body

  | PMatching {mtchdv; patts; _} -> 
    Format.fprintf fmt "(match %a with%a\nend)" print_ast mtchdv
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
                print_ast mbody
          ) patts
      ) patts

  | Cstr {cname; params; _} ->
    Format.fprintf fmt "(%s%a)"
      cname
      ( fun fmt l ->
          match l with 
          | [] -> ()
          | (_, a)::t -> 
            Format.fprintf fmt " (%a" print_ast a;
            List.iter (
              fun (_, a) ->
                Format.fprintf fmt ", %a" print_ast a
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

let print_gtm fmt (gtm: SS.t GTM.t) = 
  GTM.iter (
    fun gt ss -> 
      Format.fprintf fmt "logic %a: %a@."
        print_ss ss
        print_gtyp gt
  ) gtm 

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

let print_decl fmt (gtm: SS.t GTM.t) (decl: decl) =
  match decl with 
  | Axiom {name; body} ->
    let gtl = get_usyms body in 
    let ngtm, gtm = get_ngtm gtm gtl in  
    Format.fprintf fmt "\n%a@." print_gtm ngtm;
    Format.fprintf fmt "axiom %s:\n%a@." name print_ast body; 
    gtm
  | Goal {name; body} ->
    let gtl = get_usyms body in 
    let ngtm, gtm = get_ngtm gtm gtl in  
    Format.fprintf fmt "\n%a@." print_gtm ngtm;
    Format.fprintf fmt "goal %s:\n%a@." name print_ast body; 
    gtm
  | FuncDef {name; body; atyp; rtyp} -> 
    let gtl = get_usyms body in 
    let ngtm, gtm = get_ngtm gtm gtl in  
    Format.fprintf fmt "\n%a@." print_gtm ngtm;

    match rtyp with 
    | Tbool -> 
      Format.fprintf fmt "predicate %s(%a) =\n%a@."
        name
        print_tvar_list atyp
        print_ast body; 
      gtm
    | _ -> 
      Format.fprintf fmt "function %s(%a):%a =\n%a@." 
        name
        print_tvar_list atyp
        print_typ rtyp
        print_ast body; 
      gtm

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

let print_typedecls fmt (tydecls: typedecl list) = 
  List.iter (
    fun td -> 
      print_typedecl fmt td
  ) tydecls

let print_decls fmt (tydecls, decls: typedecl list * decl list) =
  print_typedecls fmt tydecls;
  ignore @@
  List.fold_left (print_decl fmt) GTM.empty decls

