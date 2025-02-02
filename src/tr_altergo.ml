(* Translation to Alt-Ergo Expressions *)

open AltErgoLib
open Ast

module E = Expr
module Sy = Symbols
module ES = E.Set

module VM = Map.Make(
  struct
    type t = tvar
    let compare v1 v2 =
      let r =
        Ast.typ_compare v1.vty v2.vty
      in
      if r <> 0 then r
      else compare v1.vname v2.vname
  end)

type t = Commands.sat_tdecl

let rec typ_to_ty ty =
  match ty with
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

(** Translates an expr to an E.t *)
let name_tag = ref 0

(** Approximating a float with a rational number
    Taken from src/parsers/native_lexer.mll
*)
let mk_num i f exp sign =
  let n_zero = Num.Int 0 in
  let n_ten = Num.Int 10 in
  let decimal_number s =
    let r = ref n_zero in
    for i=0 to String.length s - 1 do
      r := Num.add_num (Num.mult_num n_ten !r)
          (Num.num_of_int (Char.code s.[i] - Char.code '0'))
    done;
    !r
  in
  let v =
    match exp,sign with
    | Some exp,Some "-" ->
      Num.div_num (decimal_number (i^f))
        (Num.power_num (Num.Int 10) (decimal_number exp))
    | Some exp,_ ->
      Num.mult_num (decimal_number (i^f))
        (Num.power_num (Num.Int 10) (decimal_number exp))
    | None,_ -> decimal_number (i^f)
  in
  let v =
    Num.div_num v
      (Num.power_num (Num.Int 10) (Num.num_of_int (String.length f)))
  in
  v

let float_to_num f =
  if f = 0. || Float.is_nan f || Float.is_infinite f
  then Num.num_of_int 0
  else if f < 0.
  then
    match String.split_on_char '.' (Float.to_string (-. f)) with
    | [x; y] ->
      Num.minus_num (mk_num x y None None)
    | _ -> assert false
  else
    match String.split_on_char '.' (Float.to_string f) with
    | [x; y] ->
      mk_num x y None None
    | _ -> assert false

let rec translate_expr ?(name_base = "") ?(vars = VM.empty) ?(toplevel = false) ~stmtkind expr =
  match expr with
  | Cst (CstI x) ->
    E.int (Int.to_string x)
  | Cst (CstR x) ->
    let rstr =
      Num.string_of_num (
        float_to_num x
      )
    in
    E.real rstr
  | Cst (CstB true) ->
    E.vrai
  | Cst (CstB false) ->
    E.faux

  | Cst (CstBv b) ->
    E.bitv b.bits (Ty.Tbitv b.length)

  | Unop (Neg, x) ->
    translate_expr ~name_base ~vars ~stmtkind x
  | Unop (Not, x) ->
    E.neg (translate_expr ~name_base ~vars ~stmtkind x)

  | Unop (Access { ty = _, tv; fa}, i) ->
    let fa' = translate_expr ~name_base ~vars ~stmtkind fa in
    let i' = translate_expr ~name_base ~vars ~stmtkind i in
    E.mk_term
      (Sy.Op Get)
      [fa'; i']
      (typ_to_ty tv)

  | FAUpdate { ty = (ti, tv); fa; i; v}->
    let fa' = translate_expr ~name_base ~vars ~stmtkind fa in
    let i' = translate_expr ~name_base ~vars ~stmtkind i in
    let v' = translate_expr ~name_base ~vars ~stmtkind v in
    E.mk_term
      (Sy.Op Set)
      [fa'; i'; v']
      (typ_to_ty (TFArray {ti; tv}))

  | Binop (Concat n, x, y) ->
    let x' = translate_expr ~name_base ~vars ~stmtkind x in
    let y' = translate_expr ~name_base ~vars ~stmtkind y in
    E.mk_term (Sy.Op Concat) [x'; y'] (Ty.Tbitv n)

  | Unop (Extract {l; r}, b) ->
    let b' = translate_expr ~name_base ~vars ~stmtkind b in
    E.mk_term (Sy.Op (Sy.Extract (l, r))) [b'] (Ty.Tbitv (r-l))

  | Binop (((And | Or | Xor) as op), x, y) ->
    let x' = translate_expr ~name_base ~vars ~stmtkind x in
    let y' = translate_expr ~name_base ~vars ~stmtkind y in
    begin
      match op with
      | And -> E.mk_and x' y' false
      | Or -> E.mk_or x' y' false
      | Xor -> E.mk_xor x' y'
      | _-> assert false
    end

  | Binop (Imp, x, y) ->
    let x' = translate_expr ~name_base ~vars ~stmtkind x in
    let y' = translate_expr ~name_base ~vars ~stmtkind y in
    E.mk_imp x' y'

  | Binop (Iff, x, y) ->
    let x' = translate_expr ~name_base ~vars ~stmtkind x in
    let y' = translate_expr ~name_base ~vars ~stmtkind y in
    E.mk_eq ~iff:true x' y'

  | Binop ((Lt | Le | Gt | Ge) as op , x, y) ->
    let x' = translate_expr ~name_base ~vars ~stmtkind x in
    let y' = translate_expr ~name_base ~vars ~stmtkind y in
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
    E.mk_builtin ~is_pos:true sy [x''; y'']

  | Binop (Eq, x, y) ->
    let x' = translate_expr ~name_base ~vars ~stmtkind x in
    let y' = translate_expr ~name_base ~vars ~stmtkind y in
    E.mk_eq ~iff:true x' y'

  | Binop (Neq, x, y) ->
    let x' = translate_expr ~name_base ~vars ~stmtkind x in
    let y' = translate_expr ~name_base ~vars ~stmtkind y in
    E.mk_distinct ~iff:true [x'; y']

  | Binop (((ISub | IMul | IDiv | IPow | IMod | IAdd) as op), x, y) ->
    let x' = translate_expr ~name_base ~vars ~stmtkind x in
    let y' = translate_expr ~name_base ~vars ~stmtkind y in
    E.mk_term
      begin
        match op with
        | IAdd -> (Sy.Op Plus)
        | ISub -> (Sy.Op Minus)
        | IMul -> (Sy.Op Mult)
        | IDiv -> (Sy.Op Div)
        | IPow -> (Sy.Op Pow)
        | IMod -> (Sy.Op Modulo)
        | _ -> assert false
      end
      [x'; y'] Ty.Tint

  | Binop (((RSub | RMul | RDiv | RPow | RAdd) as op), x, y) ->
    let x' = translate_expr ~name_base ~vars ~stmtkind x in
    let y' = translate_expr ~name_base ~vars ~stmtkind y in
    E.mk_term
      begin
        match op with
        | RAdd -> (Sy.Op Plus)
        | RSub -> (Sy.Op Minus)
        | RMul -> (Sy.Op Mult)
        | RDiv -> (Sy.Op Div)
        | RPow -> (Sy.Op Pow)
        | _ -> assert false
      end
      [x'; y'] Ty.Treal

  | FunCall {fname; rtyp; args; _} ->
    let s =
      Sy.Name (Hstring.make fname, Sy.Other, false)
    in
    let l =
      List.map (translate_expr ~name_base ~vars ~stmtkind) args
    in
    let ty = typ_to_ty rtyp in
    E.mk_term s l ty

  | Var {vname; vty; vk = US; _} ->
    E.mk_term
      (Sy.Name (Hstring.make vname, Sy.Other, false))
      [] (typ_to_ty vty)

  | Var ({vk = (ARG | EQ | UQ | BLI); _ } as v) ->
    let sy, ty = VM.find v vars in
    E.mk_term sy [] ty

  | Exists {qvars = vs; body; _}
  | Forall {qvars = vs; body; _} ->
    let n = !name_tag in
    incr name_tag;
    let name =
      if n = 0 then name_base
      else
        Format.sprintf "#%s#sub-%d" name_base n
    in
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
            let e = E.mk_term sy [] ty in
            ES.add e acc
        ) ES.empty qvars
    in

    let binders = E.mk_binders qve in
    let triggers = [] (* ??? *) in
    let qbody = E.purify_form @@
      translate_expr  ~name_base ~vars ~stmtkind body
    in
    begin
      match expr with
      | Forall _ -> E.mk_forall
      | Exists _ -> E.mk_exists
      | _ -> assert false
    end
      name
      Loc.dummy
      binders
      triggers
      qbody
      ~toplevel
      ~decl_kind:stmtkind

  | ITE {cond; cons; alt; _} ->
    let cond' =
      translate_expr ~name_base ~vars ~stmtkind:E.Daxiom cond
    in
    let cons' =
      translate_expr ~name_base ~vars ~stmtkind cons
    in
    let alt' =
      translate_expr ~name_base ~vars ~stmtkind alt
    in
    E.mk_ite cond' cons' alt'

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
            translate_expr ~name_base ~vars ~stmtkind e
          in
          (sy, expr) :: bindings, vars
      ) ([], vars) (List.rev bds)
    in
    List.fold_left
      (fun acc (sy, e) ->
         E.mk_let sy e acc
      )
      (translate_expr ~name_base ~vars ~stmtkind rb)
      binders

  | PMatching {mtchdv; patts; _} ->
    let e = translate_expr ~name_base ~vars ~stmtkind mtchdv in
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
            translate_expr ~name_base ~vars ~stmtkind mbody
          in
          x, te
        ) (List.rev patts)
    in
    E.mk_match e pats

  | Cstr {cname; cty; params} ->
    let sy =
      Sy.Op (Sy.Constr (Hstring.make cname))
    in
    let exprs =
      List.rev_map (
        fun (_, a) ->
          translate_expr ~name_base ~vars ~stmtkind a
      ) (List.rev params)
    in
    E.mk_term sy exprs (typ_to_ty cty)

  | Dummy -> assert false

(** Translates a stmt to a Commands.sat_tdecl *)
let translate_stmt stmt =
  name_tag := 0;
  match stmt with
  | Axiom {name; body} ->
    let ff =
      translate_expr ~name_base:name ~toplevel:true
        ~stmtkind:E.Daxiom body
    in
    assert (Sy.Map.is_empty (E.free_vars ff Sy.Map.empty));
    let ff = E.purify_form ff in
    let ff =
      if Ty.Svty.is_empty (E.free_type_vars ff)
      then ff
      else
        E.mk_forall
          name Loc.dummy Symbols.Map.empty [] ff
          ~toplevel:true ~decl_kind:E.Daxiom
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
                  let sy = Sy.Name (hsv, Sy.Other, false) in
                  VM.add x (sy,ty) vm
              ) qvars vars
            in
            rm_root_uqs body ~vars
          | _ -> body, vars
        in
        let body, vars = rm_root_uqs body in
        translate_expr ~vars ~toplevel:true ~stmtkind:E.Dgoal
          (Unop (Not, body))
      end
    in
    assert (Sy.Map.is_empty (E.free_vars ff Sy.Map.empty));
    let ff = E.purify_form ff in
    let ff =
      if Ty.Svty.is_empty (E.free_type_vars ff)
      then ff
      else
        E.mk_forall "" Loc.dummy Symbols.Map.empty [] ff ~toplevel:true ~decl_kind:E.Dgoal
    in
    Commands.{
      st_loc = Loc.dummy;
      st_decl = Query (name, ff, Ty.Thm)}

  | FuncDef fdef ->
    let mk_assume name e =
      Commands.Assume (name, e, true)
    in
    let mk_preddef name e =
      Commands.PredDef (e, name)
    in

    (*Function signature *)
    let fsy = Sy.Name (Hstring.make fdef.name, Sy.Other, false) in
    let fty = typ_to_ty fdef.rtyp in
    let vars, es, xs_ =
      List.fold_left
        ( fun (vs, es, exps) v ->
            let var = Var.of_string v.vname in
            let vsy = Sy.Var var in
            let ty = typ_to_ty v.vty in
            let exp = E.mk_term vsy [] ty in
            VM.add v (vsy, ty) vs,
            ES.add exp es,
            exp :: exps
        )
        (VM.empty, ES.empty, [])
        fdef.atyp
    in

    let xs = List.rev xs_ in
    let fsign = E.mk_term fsy xs fty in

    let stmtkind, mk_func =
      begin
        match fdef.rtyp with
        | Tbool -> E.Dpredicate fsign, mk_preddef
        | _ -> E.Dfunction fsign, mk_assume
      end
    in

    let fbody =
      translate_expr ~vars ~toplevel:true ~stmtkind fdef.body
    in

    let lem = E.mk_eq ~iff:true fsign fbody in
    let binders = E.mk_binders es in

    let ret =
      E.mk_forall
        fdef.name Loc.dummy binders [] lem
        ~toplevel:true ~decl_kind:stmtkind
    in
    assert (Sy.Map.is_empty (E.free_vars ret Sy.Map.empty));
    let ff = E.purify_form ret in
    let ret =
      if Ty.Svty.is_empty (E.free_type_vars ff)
      then ff
      else
        E.mk_forall fdef.name Loc.dummy binders [] ff ~toplevel:true ~decl_kind:stmtkind
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
  | Cst (CstI i) ->
    let istr =
      if i < 0
      then Format.sprintf "(- %s)"
          ( let istr = string_of_int i in
            String.sub istr 1 (String.length istr - 1))
      else Format.sprintf "%i" i
    in
    Format.fprintf fmt "%s" istr

  | Cst (CstR r) ->
    let rstr = float_to_string r
    in
    Format.fprintf fmt "%s" rstr

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

let print_rcrd_b fmt (rcrd_b: (string * ty) list) =
  match rcrd_b with
  | [] -> assert false
  | (n, ty) :: t ->
    Format.fprintf fmt
      "{%s: %a"
      n print_typ ty;
    List.iter (
      fun (n, ty) ->
        Format.fprintf fmt
          "; %s: %a"
          n print_typ ty;
    ) t;
    Format.fprintf fmt "}"

let print_adt fmt (ptrns: rcrd_ty list) =
  let pr_aux fmt =
    function
    | (n, []) ->
      Format.fprintf fmt "%s" n
    | (n, rcrd) ->
      Format.fprintf fmt "%s of %a"
        n print_rcrd_b rcrd
  in
  match ptrns with
  | p :: t ->
    Format.fprintf fmt "\n  %a" pr_aux p;
    List.iter (
      fun p ->
        Format.fprintf fmt "\n  | %a" pr_aux p
    ) t
  | _ -> assert false

let print_typedecl fmt (tyd: typedecl) =
  match tyd with
  | Adt_decl (n, adt) ->
    Format.fprintf fmt "type %s = %a@."
      n print_adt adt
  | Record_decl (n, rcrd) ->
    Format.fprintf fmt "type %s = %a@."
      n print_rcrd_b rcrd

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
