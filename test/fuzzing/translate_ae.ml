(* Translation to Alt-Ergo Expressions *)

open AltErgoLib
open Ast
module Sy = Symbols 
module ES = Expr.Set


(** VM is used to map tvar ids to the tvar's Expr.t   
    representation as a variable or an uninterpreted symbol *)
module VM = Map.Make(
  struct
    type t = int 
    let compare = Int.compare
  end)


let rec typ_to_ty typ = 
  match typ with  
  | Tint -> Ty.Tint
  | Treal -> Ty.Treal
  | Tbool -> Ty.Tbool
  | TBitV n -> Ty.Tbitv n 
  | TFArray {ti; tv} ->
    Ty.Tfarray (typ_to_ty ti, typ_to_ty tv)
  | TDummy -> assert false

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

  | FunCall {fname; rtyp; args} ->
    Expr.mk_term 
      (Sy.Name (Hstring.make fname, Sy.Other)) 
      (List.map (translate_ast ~vars ~decl_kind) args)
      (typ_to_ty rtyp)

  | Var {vname; vty; vk = US; _} -> 
    Expr.mk_term 
      (Sy.Name (Hstring.make vname, Sy.Other)) 
      [] (typ_to_ty vty)

  | Var {vk = (ARG | EQ | UQ | BLI); id; _ } -> 
    let sy, ty = VM.find id vars in 
    Expr.mk_term sy [] ty

  | Exists {qvars = vs; body; _} 
  | Forall {qvars = vs; body; _} ->
    let qvars, vars = 
      VS.fold ( 
        fun x (vl,vm) -> 
          let ty = typ_to_ty x.vty in
          let hsv = Hstring.make x.vname in 
          let v = Var.of_hstring hsv in
          let sy = Sy.Var v in 
          (sy, ty) :: vl,
          VM.add x.id (sy,ty) vm
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
        fun ({vname; vty; id; _}, e) (bindings, vars) -> 
          let sy = Sy.var (Var.of_string vname) in 
          let ty = typ_to_ty vty in
          let vars = VM.add id (sy, ty) vars in 
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

  | Dummy -> assert false 

(** Translates a cmd to a Commands.sat_tdecl *)
let translate_decl cmd = 
  match cmd with 
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
                    VM.add x.id (sy,ty) vm)
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
        ( fun (vs, es, exps) var -> 
            let v = Var.of_string var.vname in 
            let vsy = Sy.Var v in
            let ty = typ_to_ty var.vty in 
            let exp = Expr.mk_term vsy [] ty in
            VM.add var.id (vsy, ty) vs, 
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
    Commands.{
      st_loc = Loc.dummy;
      st_decl = mk_func fdef.name ret}

