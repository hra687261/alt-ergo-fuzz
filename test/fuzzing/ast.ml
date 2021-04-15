open AltErgoLib

module Sy = Symbols 

let max_fuel = 3
let nb_usym_vars = 3
let nb_usym_funcs = 3
let nb_q_vars = 3
let v_id = ref 0 

type ty = Tint | Treal | Tbool 

type vkind = 
  | EQ (* Exisancially quantified *)
  | UQ (* Universally quantified *)
  | US (* Uninterpreted symbol *)

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

type vty = {vname: string; ty: ty; vk: vkind; id: int}

type ast = 
  | Cst of cst
  | Var of vty
  | Unop of unop * ast 
  | Binop of binop * ast * ast
  | Fun of func
  | Forall of quant
  | Exists of quant
and quant =
  {vars: vty list * ast list; body: ast}
and func =
  {fname: string; rty: ty; args: ast list}

let print_binop fmt binop =
  Format.fprintf fmt @@
  match binop with 
  | And ->  "&&"   | Or -> "||"    | Xor -> "xor"
  | Imp -> "->"    | Iff-> "<->"

  | Lt -> "<"      | Le -> "<="    | Gt -> ">"
  | Ge -> ">="     | Eq -> "="     | Neq -> "<>" 

  | RAdd -> "+."   | RSub -> "-."  | RMul -> "*." 
  | RDiv -> "/."   | RPow -> "**." 

  | IAdd -> "+"    | ISub -> "-"   | IMul -> "*" 
  | IDiv -> "/"    | IMod -> "%%"  | IPow -> "**" 

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

  | Fun {fname; args; _} -> 
    Format.fprintf fmt "%s(" fname;
    let _ = 
      List.fold_left 
      ( fun isfist x ->
          if isfist
          then (Format.fprintf fmt "%a" print x; false)
          else (Format.fprintf fmt ", %a" print x; false))
      true 
      args in 
      Format.fprintf fmt ")";

  | Forall {vars = (vl, trs); body} ->
    Format.fprintf fmt "∀ ";
    List.iter 
      (fun {vname; _} -> 
        Format.fprintf fmt "%s " vname) 
      vl;
    Format.fprintf fmt ".{";
    List.iter (Format.fprintf fmt " %a;" print) trs; 
    Format.fprintf fmt "} %a" print body
  
  | Exists {vars = (vl, trs); body} -> 
    Format.fprintf fmt "∃ ";
    List.iter 
      (fun {vname; _} -> 
        Format.fprintf fmt "%s " vname) 
      vl;
      Format.fprintf fmt ".{";
      List.iter (Format.fprintf fmt " %a;" print) trs; 
      Format.fprintf fmt "} %a" print body

let mk_vars pref ty vk=
  let rec aux nb = 
    match nb = 0 with 
    | true -> []
    | false -> 
      let vname = (pref^string_of_int (nb_usym_vars-nb)) in 
        Var {vname; ty; vk; id = (incr v_id; !v_id)} ::aux (nb-1)  
  in 
    aux nb_usym_vars

let mk_binop bop x y =
  Binop (bop, x, y)
let mk_unop uop x =
  Unop (uop, x)

type ptree =  
  | Node of vty list * ptree list
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
      | Fun {args; _} -> ( 
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
    | Fun _ -> aux ast 
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
        ( fun exp (vs, kd) -> 
            match kd with 
            | EQ -> 
              Exists {vars = vs, [](*add_triggers vs exp*); body = exp}
            | UQ -> 
              Forall {vars = vs, [](*add_triggers vs exp*); body = exp}
            | US -> assert false)
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

    | Fun {fname; rty; args} -> (
      match pt with
      | Node (vs, pstl) -> 
        q_aux_bis vs 
          (Fun {fname; rty; args = aux_call args pstl})
      | Empty -> ast)
      
    | Unop (op, x) -> Unop (op, quantify_aux x pt) 
    | Forall {vars; body} -> 
      Forall {vars; body = quantify_aux body pt}
    | Exists {vars; body} -> 
      Exists {vars; body = quantify_aux body pt}
    
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
      | Fun {args; _} -> 
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
      let rpath = List.rev (List.tl path) in 
      List.map
        ( fun x -> (rpath, x)) 
        ( get_vars x @ get_vars y)

    | Unop (Not, x) -> 
      q_aux path x
    | Unop (_, x) -> 
      let rpath = List.rev path in
      List.map
        ( fun x -> (rpath, x)) 
        ( get_vars x)

    | Fun {args; rty = Tbool; _} -> 
      fst @@
      List.fold_left 
        ( fun (l, n) x -> 
            q_aux (n::path) x @ l, n + 1)
        ([], 0)
        args
    | Fun {args; _} -> 
      let rpath = List.rev (List.tl path) in 
      let vars = 
        List.fold_left 
          (fun acc x -> 
            List.map 
              (fun x -> (List.rev rpath, x))
              (get_vars x) @ acc) 
          [] args 
      in 
      vars 

    | Var ({vk = (EQ|UQ); _} as var) ->
      [(List.rev path, var)]
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
      (* print_ptree pt; *)
      quantify_aux ast pt
  
let rec ast_to_expr ast = 
  match ast with 
  | Cst (CstI x) -> 
    Expr.int (Int.to_string x)
  | Cst (CstR x) -> 
    Expr.real (Float.to_string x)
  | Cst (CstB true) ->
    Expr.vrai
  | Cst (CstB false) ->
    Expr.faux
  
  | Binop (And, x, y) ->
    Expr.mk_and 
      (ast_to_expr x) (ast_to_expr y) true 0
  | Binop (Or, x, y) ->
    Expr.mk_or 
      (ast_to_expr x) (ast_to_expr y) true 0
  | Binop (Imp, x, y) ->
    Expr.mk_imp 
      (ast_to_expr x) (ast_to_expr y) 0
  | Binop (Xor, x, y) ->
    Expr.mk_xor 
      (ast_to_expr x) (ast_to_expr y) 0
  | Binop (Iff, x, y) ->
    Expr.mk_eq ~iff:true 
      (ast_to_expr x) (ast_to_expr y)

  | Binop (Lt, x, y) ->
    Expr.mk_builtin ~is_pos:true Sy.LT
      [ast_to_expr x; ast_to_expr y]
  | Binop (Le, x, y) ->
    Expr.mk_builtin ~is_pos:true Sy.LE
      [ast_to_expr x; ast_to_expr y]
  | Binop (Gt, x, y) ->
    Expr.mk_builtin ~is_pos:false Sy.LE
      [ast_to_expr x; ast_to_expr y]
  | Binop (Ge, x, y) ->
    Expr.mk_builtin ~is_pos:false Sy.LT
      [ast_to_expr x; ast_to_expr y]

  | Binop (Eq, x, y) ->
    Expr.mk_eq ~iff:false
      (ast_to_expr x) (ast_to_expr y)
  | Binop (Neq, x, y) ->
    Expr.mk_distinct ~iff:false 
      [ast_to_expr x; ast_to_expr y]
  

  | Binop (IAdd, x, y) ->
    Expr.mk_term (Sy.Op Sy.Plus)
      [ast_to_expr x; ast_to_expr y] Ty.Tint 
  | Binop (ISub, x, y) ->
    Expr.mk_term (Sy.Op Sy.Minus)
      [ast_to_expr x; ast_to_expr y] Ty.Tint 
  | Binop (IMul, x, y) ->
    Expr.mk_term (Sy.Op Sy.Mult)
      [ast_to_expr x; ast_to_expr y] Ty.Tint 
  | Binop (IDiv, x, y) ->
    Expr.mk_term (Sy.Op Sy.Div)
      [ast_to_expr x; ast_to_expr y] Ty.Tint 
  | Binop (IMod, x, y) ->
    Expr.mk_term (Sy.Op Sy.Modulo) 
      [ast_to_expr x; ast_to_expr y] Ty.Tint 
  | Binop (IPow, x, y) ->
    Expr.mk_term (Sy.Op Sy.Pow) 
      [ast_to_expr x; ast_to_expr y] Ty.Tint 
  
  | Binop (RAdd, x, y) ->
    Expr.mk_term (Sy.Op Sy.Plus)
      [ast_to_expr x; ast_to_expr y] Ty.Treal 
  | Binop (RSub, x, y) ->
    Expr.mk_term (Sy.Op Sy.Minus)
      [ast_to_expr x; ast_to_expr y] Ty.Treal 
  | Binop (RMul, x, y) ->
    Expr.mk_term (Sy.Op Sy.Mult)
      [ast_to_expr x; ast_to_expr y] Ty.Treal 
  | Binop (RDiv, x, y) ->
    Expr.mk_term (Sy.Op Sy.Div)
      [ast_to_expr x; ast_to_expr y] Ty.Treal 
  | Binop (RPow, x, y) ->
    Expr.mk_term (Sy.Op Sy.Pow) 
      [ast_to_expr x; ast_to_expr y] Ty.Treal 

  | Var {vname; ty = Tint; vk = US; _} -> 
    Expr.mk_term 
      (Sy.Name (Hstring.make vname, Sy.Other)) 
      [] Ty.Tint 
  | Var {vname; ty = Treal; vk = US; _} -> 
    Expr.mk_term 
      (Sy.Name (Hstring.make vname, Sy.Other)) 
      [] Ty.Treal 
  | Var {vname; ty = Tbool; vk = US; _} -> 
    Expr.mk_term 
      (Sy.Name (Hstring.make vname, Sy.Other)) 
      [] Ty.Tbool 

  | Fun {fname; rty = Tint; args} -> 
    Expr.mk_term 
      (Sy.Name (Hstring.make fname, Sy.Other)) 
      (List.map ast_to_expr args) Ty.Tint
  | Fun {fname; rty = Treal; args} -> 
    Expr.mk_term 
      (Sy.Name (Hstring.make fname, Sy.Other)) 
      (List.map ast_to_expr args) Ty.Treal
  | Fun {fname; rty = Tbool; args} -> 
    Expr.mk_term 
      (Sy.Name (Hstring.make fname, Sy.Other)) 
      (List.map ast_to_expr args) Ty.Tbool
      
  | Unop (Neg, x) -> ast_to_expr x
  | Unop (Not, x) -> 
    Expr.neg (ast_to_expr x)

  | Var {vname; ty=Tint; vk=(EQ|UQ); _} -> 
    Expr.mk_term 
      (Sy.Name (Hstring.make vname, Sy.Other)) 
      [] Ty.Tint
  | Var {vname; ty=Treal; vk=(EQ|UQ); _} -> 
    Expr.mk_term 
      (Sy.Name (Hstring.make vname, Sy.Other)) 
      [] Ty.Treal 
  | Var {vname; ty=Tbool; vk=(EQ|UQ); _} -> 
    Expr.mk_term 
      (Sy.Name (Hstring.make vname, Sy.Other)) 
      [] Ty.Tbool 

  | Forall {vars = (vs, trs); body} -> 
    let binders = 
      List.fold_left 
      ( fun acc v -> 
          let rty = (
            match v.ty with 
            | Tint -> Ty.Tint
            | Treal -> Ty.Treal
            | Tbool -> Ty.Tbool) 
          in
          Sy.Map.add 
            (Sy.Var (Var.of_string v.vname)) 
            (rty, v.id) acc)
      Sy.Map.empty vs
    in
    let triggers = (* ??? *)
      List.fold_left 
        ( fun acc x -> 
            acc @ x)
        []
        @@
      List.map 
        ( fun x -> 
          Expr.make_triggers 
            (ast_to_expr x) 
            binders 
            Expr.Dgoal 
            Util.{ (* ??? *)
              nb_triggers = 1;
              triggers_var = true;
              no_ematching = false;
              greedy = false;
              use_cs = false;
              backward = Util.Normal;
            })
        trs 
    in 
    Expr.mk_forall 
      "" Loc.dummy 
      binders
      triggers (ast_to_expr body) (-42) 
      ~toplevel:false 
      ~decl_kind:Expr.Dgoal
  | Exists {vars = (vs, trs); body} ->
    let binders = 
      List.fold_left 
      ( fun acc v -> 
          let rty = (
            match v.ty with 
            | Tint -> Ty.Tint
            | Treal -> Ty.Treal
            | Tbool -> Ty.Tbool) 
          in
          Sy.Map.add 
            (Sy.Var (Var.of_string v.vname)) 
            (rty, v.id) acc)
          Sy.Map.empty vs
    in
    let triggers = (* ??? *)
      List.fold_left 
        ( fun acc x -> 
            acc @ x)
        []
        @@
      List.map 
        ( fun x -> 
          Expr.make_triggers 
            (ast_to_expr x) 
            binders 
            Expr.Dgoal 
            Util.{ (* ??? *)
              nb_triggers = 1;
              triggers_var = true;
              no_ematching = false;
              greedy = false;
              use_cs = false;
              backward = Util.Normal;
            })
        trs 
    in 
    Expr.mk_exists 
      "" Loc.dummy 
      binders
      triggers 
      (ast_to_expr body) (-42) 
      ~toplevel:false 
      ~decl_kind:Expr.Dgoal

let mk_cmd_query name expr gsty = 
  Commands.{ 
    st_loc = Loc.dummy;
    st_decl = 
      Commands.Query (name, expr, gsty)}

let mk_cmd_assume name expr b =
  Commands.{ 
    st_loc = Loc.dummy;
    st_decl = 
      Commands.Assume (name, expr, b)}

let mk_cmd_preddef name expr =
  Commands.{ 
    st_loc = Loc.dummy;
    st_decl = 
      Commands.PredDef (expr, name)}