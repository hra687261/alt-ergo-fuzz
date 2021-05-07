open AltErgoLib

module Sy = Symbols 

let query_max_depth = 3
let axiom_max_depth = 3
let func_max_depth = 3

let nb_usym_vars = 3
let nb_usym_funcs = 3
let nb_funs = 3
let nb_q_vars = 3

let v_id, thmid, axid, gid, qid, fid = 
  ref 0, ref 0, ref 0, ref 0, ref 0, ref 0

type typ = Tint | Treal | Tbool 

type vkind = 
  | EQ (* Exisancially quantified *)
  | UQ (* Universally quantified *)
  | US (* Uninterpreted symbol *)
  | ARG (* Function/predicate argument *)

type cst = 
  | CstI of int 
  | CstR of float 
  | CstB of bool 

type binop = 
  | And | Or | Xor | Imp | Iff
  | Lt | Le | Gt | Ge | Eq | Neq
  | RAdd | RSub | RMul | RDiv | RPow
  | IAdd | ISub | IMul | IDiv | IMod | IPow  
  
type unop = Neg | Not 

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

let typ_to_ty typ = 
  match typ with  
  | Tint -> Ty.Tint
  | Treal -> Ty.Treal
  | Tbool -> Ty.Tbool

let print_typ fmt typ = 
  match typ with 
  | Tint -> Format.fprintf fmt "int"
  | Treal -> Format.fprintf fmt "real"
  | Tbool -> Format.fprintf fmt "bool"

let print_binop fmt binop =
  Format.fprintf fmt @@
  match binop with 
  | And ->  "&&"   | Or -> "||"     | Xor -> "xor"
  | Imp -> "->"    | Iff-> "<->"

  | Lt -> "<"      | Le -> "<="     | Gt -> ">"
  | Ge -> ">="     | Eq -> "="      | Neq -> "<>" 

  | RAdd -> "+."   | RSub -> "-."   | RMul -> "*." 
  | RDiv -> "/."   | RPow -> "**."

  | IAdd -> "+"    | ISub -> "-"    | IMul -> "*" 
  | IDiv -> "/"    | IMod -> "%%"   | IPow -> "**" 

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
  
  | Var {vname; _} ->
    Format.fprintf fmt "%s" vname
  
  | Unop (Neg, ast) ->
    Format.fprintf fmt "- (%a)" print ast 
  | Unop (Not, ast) ->
    Format.fprintf fmt "not (%a)" print ast 

  | Binop (binop, x, y) ->
    Format.fprintf fmt "(%a %a %a)" print x print_binop binop print y 

  | FunCall {fname; args; _} -> 
    Format.fprintf fmt "%s(" fname;
    let _ = 
      List.fold_left 
      ( fun isfist x ->
          if isfist
          then (
            Format.fprintf fmt "%a" print x; 
            false)
          else (
            Format.fprintf fmt ", %a" print x; 
            false))
      true 
      args in 
      Format.fprintf fmt ")";

  | Forall {qvars; trgs; body} ->
    Format.fprintf fmt "∀ ";
    VS.iter 
      (fun {vname; _} -> 
        Format.fprintf fmt "%s " vname) 
      qvars;
    Format.fprintf fmt ".{";
    List.iter (Format.fprintf fmt " %a;" print) trgs; 
    Format.fprintf fmt "} %a" print body
  
  | Exists {qvars; trgs; body} -> 
    Format.fprintf fmt "∃ ";
    VS.iter 
      (fun {vname; _} -> 
        Format.fprintf fmt "%s " vname) 
      qvars;
      Format.fprintf fmt ".{";
      List.iter (Format.fprintf fmt " %a;" print) trgs; 
      Format.fprintf fmt "} %a" print body

let print_cmd fmt cmd = 
  match cmd with 
  | FuncDef {name; body; atyp; rtyp} -> 
    Format.fprintf 
      fmt 
      "FuncDef %s %a -> %a :\n%a@." 
      name 
      (fun fmt args -> 
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
          pr_iter args)
      atyp
      (fun fmt rty -> 
        Format.fprintf fmt "%a" print_typ rty)
      rtyp
      print body
  
  | Axiom {name; body} ->
    Format.fprintf fmt "Axiom(%s):\n%a@." name print body
  | Goal {name; body} ->
    Format.fprintf fmt "Goal(%s):\n%a@." name print body

let mk_vname pref num = 
  pref ^ string_of_int num

let mk_tvar vname vty vk = 
  {vname; vty; vk; id = (incr v_id; !v_id)}
  
let mk_var vname vty vk = 
  Var (mk_tvar vname vty vk)

(* Uninterpreted variables *)

let mk_usymvars nb_usym_vars =
  let aux pref ty n = 
    let vname = mk_vname pref n in 
    mk_var vname ty US ,
    Sy.Name (Hstring.make vname, Sy.Other)
  in
  let tmp = 
    List.init nb_usym_vars (fun x -> x+1) 
  in 
    List.map (aux "iuqv" Tint) tmp,
    List.map (aux "ruqv" Treal) tmp,
    List.map (aux "buqv" Tbool) tmp

let i_uvars, r_uvars, b_uvars = mk_usymvars nb_usym_vars

let get_uvar_ast num ty =
  fst
  begin
    match ty with
    | Tint -> List.nth i_uvars num
    | Treal -> List.nth r_uvars num
    | Tbool -> List.nth b_uvars num
  end

let get_uvar_syty num ty =
  let (_, sy), rty = 
    match ty with
    | Tint -> List.nth i_uvars num, Ty.Tint
    | Treal -> List.nth r_uvars num, Ty.Treal
    | Tbool -> List.nth b_uvars num, Ty.Tbool
  in 
    sy, rty

let get_uvar_expr num ty =
  let sy, rty = get_uvar_syty num ty in 
    Expr.mk_term sy [] rty

(* Uninterpreted functions *)

let mk_usymfuncs nb_usym_funcs =
  let aux pref n = 
    let fname = mk_vname pref n in 
      fname,
      Sy.Name (Hstring.make fname, Sy.Other)
  in
  let tmp = 
    List.init nb_usym_funcs (fun x -> x+1) 
  in 
    List.map (aux "iuf_") tmp,
    List.map (aux "ruf_") tmp,
    List.map (aux "buf_") tmp

let i_ufuncs, r_ufuncs, b_ufuncs = mk_usymfuncs nb_usym_funcs

let get_ufunc_ast num ty args = 
  let (fname, _), rtyp = 
    begin 
      match ty with 
      | Tint -> List.nth i_ufuncs num, Tint
      | Treal -> List.nth r_ufuncs num, Treal
      | Tbool -> List.nth b_ufuncs num, Tbool
    end 
  in 
    FunCall {fname; rtyp; args}

let get_ufunc_syty num ty =
  let (_, sy), rty = 
    match ty with 
    | Tint -> List.nth i_ufuncs num, Ty.Tint
    | Treal -> List.nth r_ufuncs num, Ty.Treal
    | Tbool -> List.nth b_ufuncs num, Ty.Tbool
  in 
    sy, rty

let get_ufunc_expr ate vars num ty args =
  let sy, rty = get_ufunc_syty num ty in 
  let rargs = List.map (ate ~vars) args in  
    Expr.mk_term sy rargs rty

(* User-defined function *)

let mk_udfuncs nb_ud_funcs =
  let aux pref n = 
    let fname = mk_vname pref n in 
      fname,
      Sy.Name (Hstring.make fname, Sy.Other)
  in
  let tmp = 
    List.init nb_ud_funcs (fun x -> x+1) 
  in 
    List.map (aux "iudf_") tmp,
    List.map (aux "rudf_") tmp,
    List.map (aux "budf_") tmp

let i_udfs, r_udfs, b_udfs = mk_udfuncs nb_usym_funcs

let get_udfunc_ast num ty args =
  let (fname, _), rtyp = 
    begin 
      match ty with 
      | Tint -> List.nth i_udfs num, Tint
      | Treal -> List.nth r_udfs num, Treal
      | Tbool -> List.nth b_udfs num, Tbool
    end 
  in 
    FunCall {fname; rtyp; args}

let get_udfunc_syty num ty =
  let (_, sy), rty = 
    match ty with
    | Tint -> List.nth i_udfs num, Ty.Tint
    | Treal -> List.nth r_udfs num, Ty.Treal
    | Tbool -> List.nth b_udfs num, Ty.Tbool
  in 
    sy, rty    

let get_udfunc_expr ate vars num ty args =
  let sy, rty = get_udfunc_syty num ty in 
  let rargs = List.map (ate ~vars) args in  
    Expr.mk_term sy rargs rty

let mk_binop bop x y =
  Binop (bop, x, y)

let mk_unop uop x =
  Unop (uop, x)

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
            (fun (cacc, acck, gacc) v -> 
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
                (fun acc x -> 
                  VS.add x acc)
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
          (fun acc x -> 
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
      (fun (cacc, accv, gacc) (p, v) ->
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
        (fun acc (p, v) ->
          insert_in_ptree p v acc)
        Empty nspll
    in 
      quantify_aux ast pt

let rec ast_to_expr ?(vars = VM.empty) ~decl_kind ast = 
  match ast with 
  | Cst (CstI x) -> 
    Expr.int (Int.to_string x)
  | Cst (CstR x) -> 
    Expr.real (Float.to_string x)
  | Cst (CstB true) ->
    Expr.vrai
  | Cst (CstB false) ->
    Expr.faux
  
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
    Expr.mk_term 
      (Sy.Name (Hstring.make vname, Sy.Other)) 
      [] 
      (typ_to_ty vty)

  | Var {vk = (ARG | EQ | UQ); id; _ } ->

    let sy, ty = VM.find id vars in 
      Expr.mk_term sy [] ty

  | Exists {qvars = vs; body; _} 
  | Forall {qvars = vs; body; _} ->
    let qvars, vars = 
      VS.fold
        (fun x (vl,vm) -> 
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
      (ast_to_expr ~vars ~decl_kind body) (-42) 
      ~toplevel:false 
      ~decl_kind

let cmd_to_commad cmd = 
  match cmd with 
  | Axiom {name; body} ->
    let ff = 
      ast_to_expr ~decl_kind:Expr.Daxiom body
    in 
    assert (Sy.Map.is_empty (Expr.free_vars ff Sy.Map.empty));
    let ff = Expr.purify_form ff in
    let ff = 
      if Ty.Svty.is_empty (Expr.free_type_vars ff) 
      then 
        ff
      else
        let id = Expr.id ff in
          Expr.mk_forall name Loc.dummy Symbols.Map.empty [] ff id ~toplevel:true ~decl_kind:Expr.Daxiom
    in 
    Commands.{ 
      st_loc = Loc.dummy;
      st_decl = Assume (name, ff, true)}

  | Goal {name; body} ->
    (*let body = Unop (Not,body) in*) 
    let ff = 
      ast_to_expr ~decl_kind:Expr.Dgoal body
    in 
    assert (Sy.Map.is_empty (Expr.free_vars ff Sy.Map.empty));
    let ff = Expr.purify_form ff in
    let ff = 
      if Ty.Svty.is_empty (Expr.free_type_vars ff) 
      then ff
      else
        let id = Expr.id ff in
          Expr.mk_forall name Loc.dummy Symbols.Map.empty [] ff id ~toplevel:true ~decl_kind:Expr.Dgoal
    in 
    Commands.{ 
      st_loc = Loc.dummy;
      st_decl = Query (name, ff, Typed.Thm)}

  | FuncDef fdef -> 
    (*Function signature *)
    let fsy = Sy.Name (Hstring.make fdef.name, Sy.Other) in
    let fty = typ_to_ty fdef.rtyp in 
    let vars, es, xs_ =
      List.fold_left 
        (fun (vs, es, exps) var -> 
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
    
    (* Function body *)
    let fbody = ast_to_expr ~vars ~decl_kind:(Expr.Dfunction fsign) fdef.body in 

    (* Lemma *)
    let lem = Expr.mk_eq ~iff:true fsign fbody in
    let binders = Expr.mk_binders es in 
      match fdef.rtyp with 
      | Tint | Treal ->
        let ret = 
          Expr.mk_forall 
            fdef.name Loc.dummy binders [] lem (-42) 
            ~toplevel:true ~decl_kind:(Expr.Dfunction fsign)
        in
        Commands.{
          st_loc = Loc.dummy;
          st_decl = Assume (fdef.name, ret, true)}   
      | Tbool -> 
        let ret = 
          Expr.mk_forall 
            fdef.name Loc.dummy binders [] lem (-42) 
            ~toplevel:true ~decl_kind:(Expr.Dpredicate fsign)
        in 
          Commands.{
            st_loc = Loc.dummy;
            st_decl = PredDef (ret, fdef.name)}