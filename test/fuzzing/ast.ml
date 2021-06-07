open AltErgoLib

module Sy = Symbols 

let query_max_depth = 2
let axiom_max_depth = 2
let func_max_depth = 2

let nb_us_vars = 3
let nb_us_funcs = 3
let nb_ud_funcs = 3
let nb_q_vars = 3

let v_id, thmid, axid, gid, qid, fid = 
  ref 0, ref 0, ref 0, ref 0, ref 0, ref 0

type typ = Tint | Treal | Tbool | TBitV of int

type vkind = 
  | EQ (* Exisancially quantified *)
  | UQ (* Universally quantified *)
  | US (* Uninterpreted symbol *)
  | ARG (* Function/predicate argument *)

type cst = 
  | CstI of int 
  | CstR of float 
  | CstB of bool 
  | CstBv of bitv 
and bitv = {length : int; bits : string}

type tvar = 
  { vname: string; vty: typ; 
    vk: vkind; id: int}

module ES = Expr.Set

module VS = Set.Make(
  struct 
    type t = tvar 
    let compare x y = Int.compare x.id y.id
  end 
  )

(** VM is used to map tvar ids to the tvar's Expr.t   
    representation as a variable or an uninterpreted symbol *)
module VM = Map.Make(
  struct
    type t = int 
    let compare = Int.compare
  end)

type ast = 
  | Cst of cst
  | Var of tvar
  | Unop of unop * ast 
  | Binop of binop * ast * ast
  | FunCall of fcall
  | Forall of quant
  | Exists of quant

and binop = 
  | And | Or | Xor | Imp | Iff
  | Lt | Le | Gt | Ge | Eq | Neq
  | RAdd | RSub | RMul | RDiv | RPow
  | IAdd | ISub | IMul | IDiv | IMod | IPow 
  | Concat of int
and unop = Neg | Not | Extract of {l: int; r: int}

and quant =
  {qvars: VS.t; trgs: ast list; body: ast}
and fcall =
  {fname: string; rtyp: typ; args: ast list}

type cmd =
  | Axiom of {name: string; body: ast} 
  | Goal of {name: string; body: ast}
  | FuncDef of fdef
and fdef = 
  { name: string; body: ast; 
    atyp: tvar list; rtyp : typ}

(* Pretty printing *)

let print_bitv fmt bitv = 
  if bitv.length <= 8 then
    Format.fprintf fmt "[|%s|]" bitv.bits 
  else 
    Format.fprintf fmt "[|size=%d|]" bitv.length

let print_typ fmt typ = 
  match typ with 
  | Tint -> Format.fprintf fmt "int"
  | Treal -> Format.fprintf fmt "real"
  | Tbool -> Format.fprintf fmt "bool"
  | TBitV n -> Format.fprintf fmt "bitv[%d]" n 

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

let rec print fmt ast = 
  match ast with 
  | Cst (CstI x) ->
    Format.fprintf fmt "%s" 
      (Hstring.view (Hstring.make (Int.to_string x)))
  | Cst (CstR x) ->
    Format.fprintf fmt "%s" 
      (Hstring.view (Hstring.make (Float.to_string x)))
  | Cst (CstB x) ->
    Format.fprintf fmt "%b" x
  | Cst (CstBv x) ->
    Format.fprintf fmt "%a" print_bitv x 

  | Var {vname; _} ->
    Format.fprintf fmt "%s" vname

  | Unop (Neg, ast) ->
    Format.fprintf fmt "- (%a)" print ast 
  | Unop (Not, ast) ->
    Format.fprintf fmt "not (%a)" print ast 
  | Unop (Extract {l;r}, ast) ->
    Format.fprintf fmt "(%a)^{%d,%d}" 
      print ast l r 

  | Binop (binop, x, y) ->
    Format.fprintf fmt "(%a %a %a)" print x print_binop binop print y 

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

let print_cmd fmt cmd = 
  match cmd with 
  | FuncDef {name; body; atyp; rtyp} -> 
    Format.fprintf fmt 
      "FuncDef %s %a -> %a :\n%a" name 
      ( fun fmt args -> 
          let rec pr_iter args =
            match args with 
            | hd :: [] ->
              Format.fprintf fmt "(%s: %a)" 
                hd.vname print_typ hd.vty
            | hd :: tl ->
              Format.fprintf fmt "(%s: %a) -> " 
                hd.vname print_typ hd.vty;
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

(* Auxiliary functions *)

let typ_to_ty typ = 
  match typ with  
  | Tint -> Ty.Tint
  | Treal -> Ty.Treal
  | Tbool -> Ty.Tbool
  | TBitV n -> Ty.Tbitv n 

let mk_vname pref num = 
  pref ^ string_of_int num

let mk_tvar vname vty vk = 
  {vname; vty; vk; id = (incr v_id; !v_id)}

let mk_var vname vty vk = 
  Var {vname; vty; vk; id = (incr v_id; !v_id)}

let mk_usv vname ty = 
  Var (mk_tvar vname ty US)

let mk_binop bop x y =
  Binop (bop, x, y)

let mk_nsy1 pref ty n = 
  let vname = mk_vname pref n in 
  mk_usv vname ty

let mk_nsy2 pref n = 
  let fname = mk_vname pref n in 
  fname

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

let mk_us_vars nb_us_vars pref ty =
  let tmp = 
    List.init nb_us_vars (fun x -> x+1) 
  in 
  List.map (mk_nsy1 pref ty) tmp    

let i_uvars, r_uvars, b_uvars, 
    bv8_uvs, bv16_uvs, bv24_uvs = 
  let f = mk_us_vars nb_us_vars in   
  f "iuv" Tint, f "ruv" Treal, f "buv" Tbool,
  f "bv8uv" (TBitV 8),
  f "bv16uv" (TBitV 16),
  f "bv24uv" (TBitV 24) 

let get_uvar_ast num ty =
  match ty with
  | Tint -> List.nth i_uvars num
  | Treal -> List.nth r_uvars num
  | Tbool -> List.nth b_uvars num
  | TBitV 8 -> List.nth bv8_uvs num
  | TBitV 16 -> List.nth bv16_uvs num
  | TBitV 24 -> List.nth bv24_uvs num
  | _ -> assert false

(* Uninterpreted functions *)

let mk_us_funcs nb_us_funcs pref =
  let tmp = 
    List.init nb_us_funcs (fun x -> x+1) 
  in 
  List.map (mk_nsy2 pref) tmp

let i_ufuncs, r_ufuncs, b_ufuncs, 
    bv8_ufs, bv16_ufs, bv24_ufs = 
  let f = mk_us_funcs nb_us_funcs in 
  f "iuf_", f "ruf_", f "buf_",
  f "bv8uf_",
  f "bv16uf_",
  f "bv24uf_"

let get_ufunc_ast num rtyp args = 
  let fname = 
    match rtyp with 
    | Tint -> List.nth i_ufuncs num
    | Treal -> List.nth r_ufuncs num
    | Tbool -> List.nth b_ufuncs num
    | TBitV 8 -> List.nth bv8_ufs num
    | TBitV 16 -> List.nth bv16_ufs num
    | TBitV 24 -> List.nth bv24_ufs num
    | _ -> assert false   
  in 
  FunCall {fname; rtyp; args}

(* User-defined functions *)

let mk_udfs nb_ud_funcs pref =
  let tmp = 
    List.init nb_ud_funcs (fun x -> x+1) 
  in 
  List.map (mk_nsy2 pref) tmp

let i_udfs, r_udfs, b_udfs, 
    bv8_udfs, bv16_udfs, bv24_udfs = 
  let f = mk_udfs nb_ud_funcs in
  f "iudf_",
  f "rudf_",
  f "budf_",
  f "bv8udf_",
  f "bv16udf_",
  f "bv24udf_"

let get_udfunc_name num rtyp =
  match rtyp with 
  | Tint -> List.nth i_udfs num
  | Treal -> List.nth r_udfs num
  | Tbool -> List.nth b_udfs num
  | TBitV 8 -> List.nth bv8_udfs num
  | TBitV 16 -> List.nth bv16_udfs num
  | TBitV 24 -> List.nth bv24_udfs num
  | TBitV _ -> 
    Format.printf "%a" print_typ rtyp;
    assert false

let get_udfunc_ast num rtyp args =
  let fname = get_udfunc_name num rtyp in 
  {fname; rtyp; args}

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

let add_triggers vs ast =
  let rec add_triggers_aux foundv ast =
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
    let rec check_trigger (vbl, f) sast = 
      match sast with
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
    let aux ast =
      if is_valid (check_trigger (foundv, false) ast)
      then [ast]
      else []
    in 
    match ast with 
    | Unop (_, x) -> add_triggers_aux foundv x
    | Binop (_, x, y) ->
      aux x @ aux y
    | FunCall _ -> aux ast 
    | Forall {body; _} -> aux body
    | Exists {body; _} -> aux body
    | _ -> []
  in
  add_triggers_aux (List.map (fun x -> (x, false)) vs) ast

(** Quantifies all the variables in the ast *)
let quantify ast = 
  let rec quantify_aux ast pt = 
    let rec aux_call (asts : ast list) (pths : ptree list) = 
      match asts with 
      | h1 :: t1 -> (
          match pths with 
          | h2 :: t2 ->
            quantify_aux h1 h2 :: aux_call t1 t2  
          | [] -> asts)
      | [] -> [] 
    in
    let q_aux_bis vs ast =  
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

        List.fold_left  
          ( fun exp (vl, kd) ->
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
              | US | ARG -> assert false)
          ast nnvs

      | [] -> ast
    in 

    match ast with 
    | Binop (op, x, y) -> (
        match pt with
        | Node (vs, pstl) -> 
          q_aux_bis vs (
            match pstl with 
            | [] -> Binop (op, x, y)
            | [_] ->
              Binop (op, 
                     quantify_aux x (List.hd pstl), y)
            | [_; _] ->
              Binop (op, 
                     quantify_aux x (List.nth pstl 0), 
                     quantify_aux y (List.nth pstl 1))
            | _ -> assert false)
        | Empty -> ast)

    | FunCall q -> (
        match pt with
        | Node (vs, pstl) -> 
          q_aux_bis vs 
            (FunCall {q with args = aux_call q.args pstl})
        | Empty -> ast)

    | Unop (op, x) -> Unop (op, quantify_aux x pt) 
    | Forall q -> 
      Forall {q with body = quantify_aux q.body pt}
    | Exists q -> 
      Exists {q with body = quantify_aux q.body pt}

    | _ -> ast
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
  (* takes an ast returns list of couples (path, var) *)
  let rec q_aux path ast =
    let rec get_vars ast = 
      match ast with 
      | Binop (_, x, y) ->
        get_vars x @ get_vars y 
      | Unop (_, x) -> get_vars x
      | FunCall {args; _} -> 
        List.fold_left 
          ( fun l x -> 
              get_vars x @ l)
          ([])
          args 
      | Var ({vk = (EQ|UQ); _} as var) -> [var]
      | _ -> []
    in 

    match ast with 
    | Binop (
        ( And | Or | Xor | Imp | Iff | 
          Lt | Le | Gt | Ge | Eq | Neq), 
        x, y) -> 
      q_aux (0::path) x @  q_aux (1::path) y
    | Binop (_, x, y) -> 
      let rpath = 
        if path = [] then [] else
          List.rev (List.tl path) 
      in 
      List.map
        ( fun x -> (rpath, x)) 
        ( get_vars x @ get_vars y)

    | Unop (Not, x) -> 
      q_aux path x
    | Unop (_, x) -> 
      let rpath = 
        if path = [] then [] else
          List.rev (List.tl path) 
      in 
      List.map
        ( fun x -> (rpath, x)) 
        ( get_vars x)

    | FunCall {args; rtyp = Tbool; _} -> 
      fst @@
      List.fold_left 
        ( fun (l, n) x -> 
            q_aux (n::path) x @ l, n + 1)
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
              List.map 
                (fun x -> (rpath, x))
                (get_vars x) @ acc) 
          [] args 
      in 
      vars 

    | Var ({vk = (EQ|UQ); _} as var) ->
      let rpath = 
        if path = [] then [] else
          List.rev (List.tl path) 
      in 
      [(rpath, var)]
    | _ -> []
  in 

  (* sorted list of (path, var) couples by var.id *)
  let cpll = 
    List.sort 
      (fun (_, x) (_, y) -> Int.compare x.id y.id) 
      (q_aux [] ast) 
  in
  match cpll with
  | [] -> ast 
  | (fh, sh) :: t -> 
    let l, v, spll = 
      List.fold_left 
        ( fun (cacc, accv, gacc) (p, v) ->
            if v.id = accv.id 
            then (p::cacc, accv, gacc) 
            else ([p], v, (cacc, accv)::gacc))
        ([fh], sh, []) t 
    in 
    let rspll = (l, v) :: spll in 
    (* list of couples (longest common prefix of paths, var) *)
    let nspll = get_q_paths rspll  in 
    (* building a path tree*)
    let pt = 
      List.fold_left 
        ( fun acc (p, v) ->
            insert_in_ptree p v acc)
        Empty nspll
    in 
    quantify_aux ast pt

(* Translation to Alt-Ergo Expressions *)

(** Translates an ast to an Expr.t *)
let rec ast_to_expr ?(vars = VM.empty) ?(toplevel = false) ~decl_kind ast = 
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

  | Binop (Concat n, x, y) ->
    let x' = ast_to_expr ~vars ~decl_kind x in 
    let y' = ast_to_expr ~vars ~decl_kind y in 
    Expr.mk_term (Sy.Op Sy.Concat) [x'; y'] (Ty.Tbitv n)

  | Unop (Extract {l; r}, b) -> 
    let l' = Expr.int (Int.to_string l) in 
    let r' = Expr.int (Int.to_string r) in 
    let b' = ast_to_expr ~vars ~decl_kind b in 
    Expr.mk_term (Sy.Op Sy.Extract) [b'; l'; r'] (Ty.Tbitv (r-l))

  | Binop (((And | Or | Xor) as op), x, y) ->
    let x' = ast_to_expr ~vars ~decl_kind x in 
    let y' = ast_to_expr ~vars ~decl_kind y in 
    begin 
      match op with 
      | And -> Expr.mk_and x' y' false 0 
      | Or -> Expr.mk_or x' y' false 0
      | Xor -> Expr.mk_xor x' y' 0
      | _-> assert false 
    end

  | Binop (Imp, x, y) ->
    Expr.mk_imp 
      (ast_to_expr ~vars ~decl_kind x) (ast_to_expr ~vars ~decl_kind y) 0
  | Binop (Iff, x, y) ->
    Expr.mk_eq ~iff:true 
      (ast_to_expr ~vars ~decl_kind x) (ast_to_expr ~vars ~decl_kind y)

  | Binop ((Lt | Le | Gt | Ge) as op , x, y) ->
    let x' = ast_to_expr ~vars ~decl_kind x in 
    let y' = ast_to_expr ~vars ~decl_kind y in 
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
    Expr.mk_eq ~iff:false
      (ast_to_expr ~vars ~decl_kind x) (ast_to_expr ~vars ~decl_kind y)
  | Binop (Neq, x, y) ->
    Expr.mk_distinct ~iff:false 
      [ast_to_expr ~vars ~decl_kind x; ast_to_expr ~vars ~decl_kind y]

  | Binop (((IAdd | ISub | IMul | IDiv | IPow | IMod) as op), x, y) ->
    let x' = ast_to_expr ~vars ~decl_kind x in 
    let y' = ast_to_expr ~vars ~decl_kind y in 
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
    let x' = ast_to_expr ~vars ~decl_kind x in 
    let y' = ast_to_expr ~vars ~decl_kind y in 
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

  | Unop (Neg, x) -> ast_to_expr ~vars ~decl_kind x
  | Unop (Not, x) -> 
    Expr.neg (ast_to_expr ~vars ~decl_kind x)

  | FunCall {fname; rtyp; args} ->
    Expr.mk_term 
      (Sy.Name (Hstring.make fname, Sy.Other)) 
      (List.map (ast_to_expr ~vars ~decl_kind) args)
      (typ_to_ty rtyp)

  | Var {vname; vty; vk = US; _} -> 
    (*???*)
    Expr.mk_term 
      (Sy.Name (Hstring.make vname, Sy.Other)) 
      [] (typ_to_ty vty)

  | Var {vk = (ARG | EQ | UQ); id; _ } ->
    let sy, ty = VM.find id vars in 
    Expr.mk_term sy [] ty

  | Exists {qvars = vs; body; _} 
  | Forall {qvars = vs; body; _} ->
    let qvars, vars = 
      VS.fold
        ( fun x (vl,vm) -> 
            let ty = typ_to_ty x.vty in
            let hsv = Hstring.make x.vname in 
            let v = Var.of_hstring hsv in
            let sy = Sy.Var v in 
            (sy, ty) :: vl,
            VM.add x.id (sy,ty) vm)
        vs ([], vars)
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
      (ast_to_expr ~vars ~toplevel ~decl_kind body) (-42) 
      ~toplevel
      ~decl_kind

(** Translates a cmd to a Commands.sat_tdecl *)
let cmd_to_commad cmd = 
  match cmd with 
  | Axiom {name; body} ->
    let decl_kind = Expr.Daxiom in
    let toplevel = true in 
    let ff = 
      ast_to_expr ~toplevel ~decl_kind body
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
        ast_to_expr ~vars ~toplevel ~decl_kind (Unop (Not, body))
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
        | Tint | Treal -> Expr.Dfunction fsign, mk_assume
        | Tbool -> Expr.Dpredicate fsign, mk_preddef
        | TBitV _ -> 
          Format.printf "%a" print_typ fdef.rtyp;
          assert false
      end
    in 

    let fbody =
      ast_to_expr ~vars ~toplevel ~decl_kind fdef.body
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