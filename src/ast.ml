type ty =
  | Tint | Treal | Tbool
  | TBitV of int
  | TFArray of {ti: ty; tv: ty}
  | Tadt of adt
  | TDummy
and adt =
  string * rcrd_ty list
and rcrd_ty = string * (string * ty) list

type typedecl =
  | Adt_decl of adt
  | Record_decl of rcrd_ty

type ftyp = {atyp: ty list; rtyp: ty}

type typc =
  | A of {tag: string; ty: ty}
  | F of {tag: string; atyp: ty list; rtyp: ty}

type vkind =
  | EQ (* Exisancially quantified *)
  | UQ (* Universally quantified *)
  | US (* Uninterpreted symbol *)
  | ARG (* Function/predicate argument *)
  | BLI (* Bound to a "let-in" *)

type fkind =
  | UDF (* user defined function *)
  | USF (* uninterpreted function*)

type tvar =
  { vname: string; vty: ty;
    vk: vkind; id: int}

let rec typ_tag ty =
  match ty with
  | Tint -> "i"
  | Treal -> "r"
  | Tbool -> "b"
  | TBitV n -> Format.asprintf "bv[%d]" n
  | TFArray {ti; tv} ->
    Format.asprintf "(%s,%s)fa"
      (typ_tag ti) (typ_tag tv)
  | TDummy -> "d"
  | Tadt (n, _) -> Format.asprintf "{%s}" n

let typc_tag {atyp; rtyp} =
  let tmp = List.rev atyp in
  let tmp = rtyp :: tmp in
  let tmp = List.rev_map typ_tag tmp in
  let tmp = String.concat ";" tmp in
  Format.sprintf "[%s]" tmp

let typ_compare t1 t2 =
  compare (typ_tag t1) (typ_tag t2)

let get_tctag (A {tag; _} | F {tag; _}) = tag

let typc_compare a b =
  compare (get_tctag a) (get_tctag b)

module VS = Set.Make(
  struct
    type t = tvar
    let compare x y =
      let r = typ_compare x.vty y.vty in
      if r <> 0
      then r
      else compare x.vname y.vname
  end
  )

type cst =
  | CstI of int
  | CstR of float
  | CstB of bool
  | CstBv of bitv
and bitv = {length : int; bits : string}

type expr =
  | Cst of cst
  | Var of tvar
  | Unop of unop * expr
  | Binop of binop * expr * expr
  | ITE of {ty: ty; cond: expr; cons: expr; alt: expr}
  | LetIn of tvar * expr * expr
  | FAUpdate of {ty: ty * ty; fa: expr; i: expr; v: expr}
  | FunCall of fcall
  | Forall of quant
  | Exists of quant
  | PMatching of pm
  | Cstr of constr
  | Dummy
and pm =
  {mtchdv: expr; patts: patt list; valty: ty}
and patt =
  {destrn: string; pattparams: tvar option list; mbody: expr}
and constr =
  {cname: string; cty: ty; params: (string * expr) list}

and binop =
  | And | Or | Xor | Imp | Iff
  | Lt | Le | Gt | Ge | Eq | Neq
  | RAdd | RSub | RMul | RDiv | RPow
  | IAdd | ISub | IMul | IDiv | IMod | IPow
  | Concat of int
and unop =
  | Neg | Not
  | Extract of {l: int; r: int}
  | Access of {ty: ty * ty; fa: expr}

and quant = {
  qvars: VS.t;
  trgs: expr list;
  body: expr
}

and fcall = {
  fname: string;
  fk: fkind;
  atyp: ty list;
  rtyp: ty;
  args: expr list
}

type stmt =
  | Axiom of {name: string; body: expr}
  | Goal of {name: string; body: expr}
  | FuncDef of fdef
and fdef =
  { name: string; body: expr;
    atyp: tvar list; rtyp : ty}

type fd_info =
  {fn: string; params: ty list; rtyp: ty}

type stmtkind = (* statement kind *)
  | FD (* function statement *)
  | AxD (* axiom statement *)
  | GD (* goal statement *)

(* userfull declarations *)

module SS = Set.Make(String)

module TDS = Set.Make(
  struct
    type t = typedecl
    let compare td1 td2 =
      let get_name td =
        match td with
        | Adt_decl (n, _) -> n
        | Record_decl (n, _) -> n
      in
      compare (get_name td1) (get_name td2)
  end
  )

module TCM = Map.Make(
  struct
    type t = typc
    let compare = typc_compare
  end
  )

type stmt_c = {
  stmt : stmt;
  tds : TDS.t;
  uss : SS.t TCM.t
}

(* Pretty printing *)

let rec print_typ fmt ty =
  match ty with
  | Tint -> Format.fprintf fmt "int"
  | Treal -> Format.fprintf fmt "real"
  | Tbool -> Format.fprintf fmt "bool"
  | TBitV n -> Format.fprintf fmt "bitv[%d]" n
  | TFArray {ti = Tint; tv} ->
    Format.fprintf fmt
      "%a farray"
      print_typ tv
  | TFArray {ti; tv} ->
    Format.fprintf fmt
      "(%a, %a) farray"
      print_typ ti
      print_typ tv
  | TDummy -> Format.fprintf fmt "dummy"
  | Tadt (n, _) -> Format.fprintf fmt "%s" n

let pr_tvar fmt {vname; vty; vk; id} =
  Format.fprintf fmt "{";
  Format.fprintf fmt "vname = %s; " vname;
  Format.fprintf fmt "vty = %a; " print_typ vty;
  Format.fprintf fmt "vk = %s; "
    begin match vk with
      | EQ -> "EQ"
      | UQ -> "UQ"
      | US -> "US"
      | ARG -> "ARG"
      | BLI -> "BLI"
    end;
  Format.fprintf fmt "id = %d" id;
  Format.fprintf fmt "}"

let print_patt_ty fmt (dn, prms: rcrd_ty) =
  Format.fprintf fmt "| %s%a" dn
    ( fun fmt l ->
        match l with
        | [] -> ()
        | (pn, ty) :: t ->
          Format.fprintf fmt "(%s: %a" pn print_typ ty;
          List.iter (
            fun (pn, ty) ->
              Format.fprintf fmt ", %s: %a" pn print_typ ty;
          ) t;
          Format.fprintf fmt ")"
    ) prms

let print_adt fmt (adt: adt) =
  let name, dstrs = adt in
  Format.fprintf fmt " type %s =\n" name;
  List.iter (
    fun (dn, prms) ->
      Format.fprintf fmt "%a\n" print_patt_ty (dn, prms)
  ) (dstrs: rcrd_ty list)

let print_ftyp fmt ({atyp; rtyp; _}: ftyp) =
  match atyp with
  | h :: t ->
    Format.fprintf fmt "%a" print_typ h;
    List.iter (
      fun x ->
        Format.fprintf fmt ", %a" print_typ x
    ) t;
    Format.fprintf fmt " -> %a" print_typ rtyp
  | [] -> assert false

let print_typc fmt typc =
  match typc with
  | A {ty; _} ->
    Format.fprintf fmt "%a" print_typ ty
  | F {atyp; rtyp; _} ->
    print_ftyp fmt {atyp; rtyp}

let pr_fdi fmt {fn; params; rtyp} =
  Format.fprintf fmt "{";
  Format.fprintf fmt "\n  fn = %s;" fn;
  Format.fprintf fmt "\n  params (";
  List.iter (
    Format.fprintf fmt " %a," print_typ;
  ) params;
  Format.fprintf fmt ")";
  Format.fprintf fmt "\n  rtyp = %a" print_typ rtyp;
  Format.fprintf fmt "\n}"

let print_bitv fmt bitv =
  if bitv.length <= 8 then
    Format.fprintf fmt "[|%s|]" bitv.bits
  else
    Format.fprintf fmt "[|size=%d|]" bitv.length

let print_binop fmt binop =
  Format.fprintf fmt (
    match binop with
    | And -> "&&"    | Or -> "||"     | Xor -> "xor"
    | Imp -> "->"    | Iff-> "<->"

    | Lt -> "<"      | Le -> "<="     | Gt -> ">"
    | Ge -> ">="     | Eq -> "="      | Neq -> "<>"

    | RAdd -> "+."   | RSub -> "-."   | RMul -> "*."
    | RDiv -> "/."   | RPow -> "**."

    | IAdd -> "+"    | ISub -> "-"    | IMul -> "*"
    | IDiv -> "/"    | IMod -> "%%"   | IPow -> "**"
    | Concat _ -> "%@")

let rec print fmt expr =
  match expr with
  | Cst (CstI x) ->
    Format.fprintf fmt "%s"
      (Int.to_string x)
  | Cst (CstR x) ->
    Format.fprintf fmt "%s"
      (Float.to_string x)
  | Cst (CstB x) ->
    Format.fprintf fmt "%b" x
  | Cst (CstBv x) ->
    Format.fprintf fmt "%a" print_bitv x

  | Var {vname; _} ->
    Format.fprintf fmt "%s" vname

  | Unop (Neg, expr) ->
    Format.fprintf fmt "- (%a)" print expr
  | Unop (Not, expr) ->
    Format.fprintf fmt "not (%a)" print expr
  | Unop (Extract {l;r}, expr) ->
    Format.fprintf fmt "(%a)^{%d,%d}"
      print expr l r
  | Unop (Access {ty = _; fa} , i) ->
    Format.fprintf fmt "%a[%a]" print fa print i

  | Binop (binop, x, y) ->
    Format.fprintf fmt "(%a %a %a)" print x print_binop binop print y

  | ITE {cond; cons; alt; _} ->
    Format.fprintf fmt
      "if (%a) then (%a) else (%a)"
      print cond print cons print alt

  | LetIn ({vname;_}, e, b) ->
    Format.fprintf fmt
      "let %s = (%a) in (%a)"
      vname print e print b

  | FAUpdate {ty = _; fa; i; v} ->
    Format.fprintf fmt "%a[%a <- %a]"
      print fa print i print v;

  | FunCall {fname; args; _} ->
    Format.fprintf fmt "%s(" fname;
    let _ =
      List.fold_left
        ( fun isfist x ->
            if isfist
            then Format.fprintf fmt "%a" print x
            else Format.fprintf fmt ", %a" print x;
            false)
        true args in
    Format.fprintf fmt ")";

  | Forall {qvars; trgs; body} ->
    Format.fprintf fmt "∀ ";
    VS.iter
      ( fun {vname; _} ->
          Format.fprintf fmt "%s " vname)
      qvars;
    Format.fprintf fmt ".{";
    List.iter (Format.fprintf fmt " %a;" print) trgs;
    Format.fprintf fmt "} %a" print body

  | Exists {qvars; trgs; body} ->
    Format.fprintf fmt "∃ ";
    VS.iter
      ( fun {vname; _} ->
          Format.fprintf fmt "%s " vname)
      qvars;
    Format.fprintf fmt ".{";
    List.iter (Format.fprintf fmt " %a;" print) trgs;
    Format.fprintf fmt "} %a" print body

  | Cstr {cname; params; _} ->
    Format.fprintf fmt "%s %a"
      cname
      ( fun fmt l ->
          match l with
          | [] -> Format.fprintf fmt ""
          | (name, a)::t ->
            Format.fprintf fmt "(%s: %a"
              name print a;
            List.iter (
              fun (name, a) ->
                Format.fprintf fmt ", %s: %a"
                  name print a
            ) t;
            Format.fprintf fmt ")"
      ) params

  | PMatching {mtchdv; patts; _} ->
    Format.fprintf fmt "match %a with" print mtchdv;
    List.iter (
      fun {destrn; pattparams; mbody} ->
        Format.fprintf fmt
          "\n| %s%a -> %a"
          destrn
          ( fun fmt l ->
              match l with
              | [] -> Format.fprintf fmt ""
              | Some {vname; _}::t ->
                Format.fprintf fmt "(%s" vname;
                List.iter (
                  fun x ->
                    match x with
                    | Some {vname; _} ->
                      Format.fprintf fmt ", %s" vname
                    | None ->
                      Format.fprintf fmt ", _"
                ) t;
                Format.fprintf fmt ")"
              | None ::t ->
                Format.fprintf fmt "(_";
                List.iter (
                  fun x ->
                    match x with
                    | Some {vname; _} ->
                      Format.fprintf fmt ", %s" vname
                    | None ->
                      Format.fprintf fmt ", _"
                ) t;
                Format.fprintf fmt ")"
          ) pattparams
          print mbody
    ) patts

  | Dummy -> assert false

let print_stmt fmt stmt =
  match stmt with
  | FuncDef {name; body; atyp; rtyp} ->
    Format.fprintf fmt
      "FuncDef %s %a -> %a :\n%a" name
      ( fun fmt args ->
          let rec pr_iter args =
            match args with
            | hd :: [] ->
              Format.fprintf fmt "(%d, %s: %a)"
                hd.id hd.vname print_typ hd.vty
            | hd :: tl ->
              Format.fprintf fmt "(%d, %s: %a) -> "
                hd.id hd.vname print_typ hd.vty;
              pr_iter tl
            | [] -> ()
          in
          pr_iter args
      ) atyp
      ( fun fmt rty ->
          Format.fprintf fmt "%a" print_typ rty
      ) rtyp
      print body

  | Axiom {name; body} ->
    Format.fprintf fmt "Axiom(%s):\n%a" name print body
  | Goal {name; body} ->
    Format.fprintf fmt "Goal(%s):\n%a" name print body

let print_stmtc fmt {stmt; tds; uss} =
  let pr_tds fmt e =
    Format.fprintf fmt "{";
    TDS.iter (
      fun (Adt_decl (n, _) | Record_decl (n, _)) ->
        Format.fprintf fmt "%s; " n
    ) e;
    Format.fprintf fmt "}"
  in
  let pr_ss fmt e =
    Format.fprintf fmt "{";
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
  Format.fprintf fmt "  stmt = \n%a@."
    print_stmt stmt;
  Format.fprintf fmt "  u_dt = %a;@."
    pr_tds tds;
  Format.fprintf fmt "  u_us = %a;@."
    pr_tcm uss;
  Format.fprintf fmt "}@."

(* Auxiliary functions *)

let float_to_string f =
  if f = 0. || Float.is_nan f || Float.is_infinite f
  then "0.0"
  else if f < 0.
  then
    Format.sprintf "(- %s)" (
      Str.replace_first
        (Str.regexp "\\.$") ".0"
        (Float.to_string (-.f)))
  else
    Format.sprintf "%s" (
      Str.replace_first
        (Str.regexp "\\.$") ".0"
        (Float.to_string f))

let rec typ_to_str ty =
  match ty with
  | Tint -> "i"
  | Treal -> "r"
  | Tbool -> "b"
  | TBitV n -> "bv" ^ string_of_int n
  | TFArray {ti; tv} ->
    Format.sprintf "s%s_%se"
      (typ_to_str ti) (typ_to_str tv)
  | Tadt (n, _) -> n
  | TDummy -> assert false

let mk_tvar_b vname vty vk id =
  {vname; vty; vk; id}

let v_id = ref 0

let mk_tvar vname vty vk =
  mk_tvar_b vname vty vk (incr v_id; !v_id)

let mk_var vname vty vk =
  Var (mk_tvar vname vty vk)

let mk_binop bop x y =
  Binop (bop, x, y)

let int_to_bitv ?(wl = 0) n =
  assert (n > 0);
  let rec add_zeros l nbz =
    if nbz = 0
    then l
    else add_zeros (0::l) (nbz-1)
  in
  let rec trim l nbtr =
    if nbtr = 0
    then l
    else
      match l with
      | _ :: t -> trim t (nbtr-1)
      | [] -> []
  in
  let rec aux n =
    if n = 0
    then 0, []
    else
      let m, d = n mod 2, n / 2 in
      let len, l = aux d in
      len + 1, m::l
  in
  let length, lst = aux n in
  let rlst =
    begin
      if (wl > length)
      then
        begin
          add_zeros (List.rev lst) (wl - length)
        end
      else
        begin
          if (wl <> 0 && wl < length)
          then trim (List.rev lst) (length - wl)
          else List.rev lst
        end
    end
  in
  let b = Buffer.create length in
  List.iter
    (fun x -> Buffer.add_char b (Char.chr (48+x)))
    rlst;
  let bits = Buffer.contents b in
  {length; bits}

(* Uninterpreted variables *)

let get_u_tvar num ty =
  let vname =
    match ty with
    | Tint -> "ui_"^ string_of_int num
    | Treal -> "ur_"^ string_of_int num
    | Tbool -> "ub_"^ string_of_int num
    | TBitV n -> "ubv_"^string_of_int n^"_"^ string_of_int num
    | TFArray {ti; tv} ->
      (Format.sprintf "ufa_%s_%s_"
         (typ_to_str ti) (typ_to_str tv))^ string_of_int num
    | Tadt (adtn, _) -> "u_"^adtn^"_"^string_of_int num
    | TDummy -> assert false
  in
  mk_tvar_b vname ty US 0

(* Uninterpreted functions *)

let get_args num =
  match num with
  | 1 -> [Tint; TDummy; TDummy; TDummy; TDummy]
  | 2 -> [Tint; Treal; TDummy; TDummy; TDummy]
  | 3 -> [Tint; Treal; Tbool; TDummy; TDummy]
  | 4 -> [Tint; Treal; Tbool; Tint; TDummy]
  | 5 -> [Tint; Treal; Tbool; Tint; Treal]
  | _ -> assert false

let get_ufunc_expr num rtyp =
  let fn =
    match rtyp with
    | Tint -> "ufi_"^ string_of_int num
    | Treal -> "ufr_"^ string_of_int num
    | Tbool -> "ufb_"^ string_of_int num
    | TBitV n -> "ufbv_"^string_of_int n^"_"^ string_of_int num
    | TFArray {ti; tv} ->
      (Format.sprintf "uffa_%s_%s_"
         (typ_to_str ti) (typ_to_str tv))^ string_of_int num
    | Tadt (adtn, _) -> "uf_"^adtn^"_"^string_of_int num
    | TDummy -> assert false
  in
  let params = get_args num
  in
  {fn; params; rtyp}

(* Bound variables *)
let bid = ref 0

let mk_bound_var ty =
  mk_tvar
    ("bli_"^typ_to_str ty^"_"^string_of_int (incr bid; !bid))
    ty BLI

(* Quantification and Triggers*)

type ptree =
  | Node of tvar list * ptree list
  | Empty

let rec insert_in_ptree path var ptree =
  let rec iin_aux phd ptl stl =
    match stl with
    | h :: t when phd = 0 ->
      insert_in_ptree ptl var h :: t
    | h :: t ->
      h :: iin_aux (phd-1) ptl t
    | [] when phd = 0 ->
      [insert_in_ptree ptl var Empty]
    | [] ->
      Empty :: iin_aux (phd-1) ptl stl
  in
  match ptree with
  | Node (vl, stl) -> (
      match path with
      | h :: t ->
        Node (vl, iin_aux h t stl)
      | [] ->
        Node (var :: vl, stl))
  | Empty ->
    match path with
    | h :: t ->
      Node ([], iin_aux h t [])
    | [] -> Node ([var], [])

(* currently unused *)
let _add_triggers vs expr =
  let rec add_triggers_aux foundv expr =
    let rec setvn nb nv vlist =
      match vlist with
      | (v, _) :: t when nv.id = v.id -> (v, nb) :: t
      | (v, b) :: t -> (v, b) :: setvn nb nv t
      | _ -> []
    in
    let llor_ vbl1 vbl2 =
      List.map2 (fun (a, b1) (_, b2) -> (a, b1 || b2)) vbl1 vbl2
    in
    let lor_ (vbl1, f1) (vbl2, f2) =
      (llor_ vbl1 vbl2, f1 || f2)
    in
    let rec bfold_left f acc l =
      match l with
      | (_, h) :: t ->
        let r = f acc h in
        if r then bfold_left f r t else r
      | _ -> acc
    in
    let is_valid (vbl, f) =
      bfold_left (&&) f vbl
    in
    (* Check that it contains all of the variables quantified
      * by the quantifier, and at least one non constant uninterpreted
      * function symbol *)
    let rec check_trigger (vbl, f) sexpr =
      match sexpr with
      | Var nv -> (setvn true nv vbl, f)
      (* what if it was quantified by an other (external) quantifier? *)
      | Unop (_,  x) -> check_trigger (vbl, f) x
      | Binop (_, x, y) ->
        lor_ (check_trigger (vbl, f) x) (check_trigger (vbl, f) y)
      | FunCall {args; _} -> (
          match args with
          | h::t ->
            List.fold_left
              check_trigger
              (check_trigger (vbl, true) h)
              (* all functions are non constant uninterpreted symbols *)
              t
          | [] -> assert false)
      | Forall {body; _} | Exists {body; _} ->
        check_trigger (vbl, f) body (* ??? *)
      | _ -> (vbl, f)
    in
    let aux expr =
      if is_valid (check_trigger (foundv, false) expr)
      then [expr]
      else []
    in
    match expr with
    | Unop (_, x) -> add_triggers_aux foundv x
    | Binop (_, x, y) ->
      List.rev_append (aux x) (aux y)
    | FunCall _ -> aux expr
    | Forall {body; _} -> aux body
    | Exists {body; _} -> aux body

    | Dummy | Cst _ | Var _
    | ITE _ | LetIn (_, _, _)
    | FAUpdate _ | PMatching _
    | Cstr _ -> assert false
  in
  add_triggers_aux (List.map (fun x -> (x, false)) vs) expr

(** Quantifies all the variables in the expr *)
let quantify expr =
  let rec quantify_aux expr pt =
    let rec aux_call (exprs : expr list) (pths : ptree list) =
      match exprs with
      | h1 :: t1 -> (
          match pths with
          | h2 :: t2 ->
            quantify_aux h1 h2 :: aux_call t1 t2
          | [] -> exprs)
      | [] -> []
    in
    let q_aux_bis vs expr =
      match vs with
      | h :: t ->
        (* Separation of variables by variable kind *)
        let l, k, nvs =
          List.fold_left
            ( fun (cacc, acck, gacc) v ->
                if acck = v.vk
                then (v::cacc, acck, gacc)
                else ([v], v.vk, (cacc, acck)::gacc))
            ([h], h.vk, []) t
        in
        let nnvs = (l, k) :: nvs in

        (* needs to be replace with a recursive call*)
        List.fold_left (
          fun exp (vl, kd) ->
            let vs =
              List.fold_left
                (fun acc x -> VS.add x acc)
                VS.empty vl
            in
            match kd with
            | EQ ->
              Exists {
                qvars = vs;
                trgs = [](*add_triggers vs exp*);
                body = exp}
            | UQ ->
              Forall {
                qvars = vs;
                trgs = [](*add_triggers vs exp*);
                body = exp}
            | US | ARG | BLI -> assert false
        ) expr nnvs

      | [] -> expr
    in

    match pt with
    | Node (vs, []) ->
      q_aux_bis vs expr
    | Node (vs, pstl) ->
      begin
        match expr with
        | Binop (op, x, y) ->
          q_aux_bis vs (
            match pstl with
            | [p] ->
              let x' = quantify_aux x p in
              Binop (op, x', y)
            | [p1; p2] ->
              let x' = quantify_aux x p1 in
              let y' = quantify_aux y p2 in
              Binop (op, x', y')
            | _ -> assert false
          )

        | ITE {ty; cond; cons; alt} ->
          q_aux_bis vs (
            match pstl with
            | [p] ->
              let cond = quantify_aux cond p in
              ITE {ty; cond; cons; alt}
            | [p1; p2] ->
              let cond = quantify_aux cond p1 in
              let cons = quantify_aux cons p2 in
              ITE {ty; cond; cons; alt}
            | [p1; p2; p3] ->
              let cond = quantify_aux cond p1 in
              let cons = quantify_aux cons p2 in
              let alt = quantify_aux alt p3 in
              ITE {ty; cond; cons; alt}
            | _ -> assert false
          )

        | LetIn (v, e, b) ->
          q_aux_bis vs (
            match pstl with
            | [p] ->
              let e' = quantify_aux e p in
              LetIn (v, e', b)
            | [p1; p2] ->
              let e' = quantify_aux e p1 in
              let b' = quantify_aux b p2 in
              LetIn (v, e', b')
            | _ -> assert false
          )

        | Unop (Access _, _) -> (
            match pt with
            | Node (vs, pstl) ->
              assert (List.for_all (fun x -> x == Empty) pstl);
              q_aux_bis vs expr
            | Empty -> expr
          )

        | Unop (op, x) ->
          Unop (op, quantify_aux x pt)

        | FunCall q -> (
            match pt with
            | Node (vs, pstl) ->
              q_aux_bis vs
                (FunCall {q with args = aux_call q.args pstl})
            | Empty -> expr)

        | Forall q ->
          Forall {q with body = quantify_aux q.body pt}
        | Exists q ->
          Exists {q with body = quantify_aux q.body pt}

        | PMatching {mtchdv; patts; valty} ->
          begin
            match pstl with
            | [] -> expr
            | [p] ->
              PMatching {
                mtchdv = quantify_aux mtchdv p;
                patts;
                valty}
            | p :: pl ->
              let rec qpatts (bdl: patt list) (ptl: ptree list) =
                match ptl with
                | pth :: ptt ->
                  begin
                    match bdl with
                    | {destrn; pattparams; mbody} :: bt ->
                      { destrn;
                        pattparams;
                        mbody =
                          quantify_aux mbody pth
                      } :: qpatts bt ptt
                    | [] -> assert false
                  end
                | [] ->
                  bdl
              in
              PMatching {
                mtchdv = quantify_aux mtchdv p;
                patts = qpatts patts pl;
                valty}
          end

        | Cstr {cname; cty; params} -> (
            match pstl with
            | [] -> Cstr {cname; cty; params}
            | pl ->
              let rec qprms pl ptl =
                match ptl with
                | pth :: ptt ->
                  begin
                    match pl with
                    | (n, a) :: bt ->
                      (n, quantify_aux a pth) :: qprms bt ptt
                    | [] -> assert false
                  end
                | [] -> pl
              in
              Cstr {cname; cty; params = qprms params pl}
          )

        | FAUpdate _
        | Dummy | Cst _ | Var _ -> assert false
      end
    | Empty -> expr
  in
  (* takes a list of couples (list of paths, var)
   * and returns a a list of couples (common path prefix, var) *)
  let rec get_q_paths spll =
    (* takes a list of paths and returns their shared prefix*)
    let rec gqp_aux spll =
      let rec get_cpref x y =
        match x, y with
        | h1::t1, h2::t2 when h1 = h2 -> h1 :: get_cpref t1 t2
        | _ -> []
      in
      match spll with
      | h1::h2::t -> gqp_aux ((get_cpref h1 h2)::t)
      | h::[] -> h
      | [] -> []
    in
    match spll with
    | (p, v) :: t -> (gqp_aux p, v) :: get_q_paths t
    | _ -> []
  in
  (* takes an expr returns list of couples (path, var) *)
  let rec q_aux path expr =
    let rec get_vars expr =
      match expr with
      | Binop (_, x, y) ->
        List.rev_append (get_vars x) (get_vars y)

      | Unop (Access {fa; _}, x) ->
        List.rev_append (get_vars fa) (get_vars x)

      | Unop (_, x) -> get_vars x

      | FunCall {args; _} ->
        List.fold_left
          ( fun l x ->
              List.rev_append (get_vars x) l)
          [] args

      | Var ({vk = (EQ|UQ); _} as var) -> [var]

      | FAUpdate {fa; i; v; _} ->
        let tmp = List.rev_append
            (get_vars i) (get_vars v)
        in
        List.rev_append tmp (get_vars fa)

      | ITE {cond; cons; alt; _} ->
        let tmp = List.rev_append
            (get_vars cons) (get_vars alt)
        in
        List.rev_append (get_vars cond) tmp
      | LetIn (_, e, b) ->
        List.rev_append
          (get_vars e) (get_vars b)

      | PMatching {mtchdv; patts; _} ->
        List.fold_left (
          fun acc {mbody; _} ->
            List.rev_append (get_vars mbody) acc
        ) (get_vars mtchdv) patts

      | Cstr {params; _} ->
        List.fold_left (
          fun acc (_, a) ->
            List.rev_append (get_vars a) acc
        ) [] params

      | Dummy | Cst _ | Var _
      | Forall _ | Exists _ -> []
    in

    match expr with
    | Binop (
        ( And | Or | Xor | Imp | Iff |
          Lt | Le | Gt | Ge | Eq | Neq),
        x, y) ->
      List.rev_append
        (q_aux (0::path) x) (q_aux (1::path) y)

    | Binop (_, x, y) ->
      let rpath =
        if path = [] then [] else
          List.rev (List.tl path)
      in
      List.rev_map (fun x -> (rpath, x))
        ( List.rev_append
            (get_vars x) (get_vars y))


    | ITE {cond; cons; alt; ty = Tbool} ->
      let tmp =
        List.rev_append
          (q_aux (1::path) cons)
          (q_aux (2::path) alt)
      in
      List.rev_append
        (q_aux (0::path) cond) tmp


    | ITE {cond; cons; alt; _} ->
      let rpath =
        if path = [] then [] else
          List.rev (List.tl path)
      in
      List.rev_map (fun x -> (rpath, x))
        ( let tmp =
            List.rev_append
              (get_vars cons)
              (get_vars alt)
          in
          List.rev_append
            (get_vars cond) tmp
        )

    | LetIn ({vty = Tbool;_}, e, b) ->
      List.rev_append
        (q_aux (0::path) e)
        (q_aux (1::path) b)

    | LetIn (_, e, b) ->
      let rpath =
        if path = [] then [] else
          List.rev (List.tl path)
      in
      ( List.rev_map (fun x -> rpath, x)
          ( List.rev_append
              (get_vars e) (get_vars b)
          ))

    | FAUpdate  {fa; i; v; _} ->
      let rpath =
        if path = [] then [] else
          List.rev (List.tl path)
      in
      List.rev_map (fun x -> (rpath, x))
        ( let tmp =
            List.rev_append
              (get_vars i) (get_vars v)
          in
          List.rev_append
            (get_vars fa) tmp
        )

    | Unop (Not, x) ->
      q_aux path x

    | Unop (Access {fa; _}, x) ->
      let rpath =
        if path = [] then [] else
          List.rev (List.tl path)
      in
      List.rev_map (fun x -> (rpath, x)) (
        List.rev_append
          (get_vars fa) (get_vars x)
      )

    | Unop (_, x) ->
      let rpath =
        if path = [] then [] else
          List.rev (List.tl path)
      in
      List.rev_map (fun x -> (rpath, x))
        (get_vars x)

    | FunCall {args; rtyp = Tbool; _} ->
      fst @@
      List.fold_left
        ( fun (l, n) x ->
            List.rev_append
              (q_aux (n::path) x) l,
            n + 1)
        ([], 0)
        args

    | FunCall {args; _} ->
      let rpath =
        if path = [] then [] else
          List.rev (List.tl path)
      in
      let vars =
        List.fold_left
          ( fun acc x ->
              let tmp =
                List.rev_map
                  (fun x -> (rpath, x))
                  (get_vars x)
              in
              List.rev_append tmp acc
          ) [] args
      in
      vars

    | Var ({vk = (EQ|UQ); _} as var) ->
      let rpath =
        if path = [] then [] else
          List.rev (List.tl path)
      in
      [(rpath, var)]

    | PMatching {mtchdv; patts; _} ->
      let rpath =
        if path = [] then [] else
          List.rev (List.tl path)
      in
      let vs =
        List.rev_map
          (fun x -> (rpath, x))
          (get_vars mtchdv)
      in
      let vars =
        List.fold_left
          ( fun acc {mbody; _} ->
              let tmp =
                List.rev_map
                  (fun x -> (rpath, x))
                  (get_vars mbody)
              in
              List.rev_append tmp acc)
          vs patts
      in
      vars

    | Cstr {params; _} ->
      let rpath =
        if path = [] then [] else
          List.rev (List.tl path)
      in
      List.fold_left (
        fun acc (_, a) ->
          let l =
            List.rev_map
              (fun x -> (rpath, x))
              (get_vars a)
          in
          List.rev_append l acc
      ) [] params

    | Dummy | Cst _ | Var _
    | Forall _ | Exists _ -> []
  in

  (* sorted list of (path, var) couples by var.id *)
  let cpll =
    List.sort
      (fun (_, x) (_, y) -> Int.compare x.id y.id)
      (q_aux [] expr)
  in
  match cpll with
  | [] -> expr
  | (fh, sh) :: t ->
    let l, v, spll =
      List.fold_left
        ( fun (cacc, accv, gacc) (p, v) ->
            if String.compare v.vname accv.vname = 0
            then (p::cacc, accv, gacc)
            else ([p], v, (cacc, accv)::gacc))
        ([fh], sh, []) t
    in
    let rspll = (l, v) :: spll in
    (* list of couples (longest common prefix of paths, var) *)
    let nspll = get_q_paths rspll in
    (* building a path tree*)
    let pt =
      List.fold_left
        ( fun acc (p, v) ->
            insert_in_ptree p v acc)
        Empty nspll
    in
    quantify_aux expr pt

