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

type vty = {name: string; ty: ty; vk: vkind; id: int}

type ast = 
  | Cst of cst
  | Var of vty
  | Unop of unop * ast 
  | Binop of binop * ast * ast
  | Fun of {name: string; rty: ty; args: ast list}
  | Forall of {vars : vty list * ast list; body : ast}
  | Exists of {vars : vty list * ast list; body : ast}

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
  
  | Var {name; _} ->
    Format.fprintf fmt "%s" name
  
  | Unop (Neg, ast) ->
    Format.fprintf fmt "- (%a)" print ast 
  | Unop (Not, ast) ->
    Format.fprintf fmt "not (%a)" print ast 

  | Binop (binop, x, y) ->
    Format.fprintf fmt "(%a %a %a)" print x print_binop binop print y 

  | Fun {name; args; _} -> 
    Format.fprintf fmt "%s(" name;
    let _ = 
      List.fold_left 
      ( fun isfist x ->
          if isfist
          then (Format.fprintf fmt "%a" print x; false)
          else (Format.fprintf fmt ", %a" print x; false))
      true 
      args in 
      Format.fprintf fmt ")";

  | Forall {vars; body} ->
    Format.fprintf fmt "∀ ";
    List.iter 
      (fun {name; _} -> 
        Format.fprintf fmt "%s " name) 
      (fst vars);
    Format.fprintf fmt ". %a" print body  
  
  | Exists {vars; body} -> 
    Format.fprintf fmt "∃ ";
    List.iter 
      (fun {name; _} -> 
        Format.fprintf fmt "%s " name) 
      (fst vars);
    Format.fprintf fmt ". %a" print body

let mk_vars pref ty vk=
  let rec aux nb = 
    match nb = 0 with 
    | true -> []
    | false -> 
      let name = (pref^string_of_int (nb_usym_vars-nb)) in 
        Var {name; ty; vk; id = (incr v_id; !v_id)} ::aux (nb-1)  
  in 
    aux nb_usym_vars

let mk_binop bop x y =
  Binop (bop, x, y)
let mk_unop uop x =
  Unop (uop, x)

type ptree =  Node of (vty list) * ptree * ptree 
            | Empty

let rec insert_in_ptree p v pt =
  match pt with 
  | Node (vl, lst, rst) -> (
    match p with 
    | h :: t -> (
      if h = 1 
      then Node (vl, lst, insert_in_ptree t v rst)
      else Node (vl, insert_in_ptree t v lst, rst))
    | [] -> Node (v::vl, lst, rst))
  | Empty -> 
    match p with 
    | h :: t -> (
      if h = 1 
      then Node ([], Empty, insert_in_ptree t v Empty)
      else Node ([], insert_in_ptree t v Empty, Empty))
    | [] -> Node ([v], Empty, Empty)

(** Quantifies all the variables in the ast *)
let quantify ast = 
  let rec quantify_aux ast pt = 
    match ast with 
    | Binop (op, x, y) -> (
      match pt with 
      | Node (vs, l, r) -> (
        match vs with 
        | h :: t -> 
          let _, _, nvs = 
            List.fold_left 
              (fun (cacc, acck, gacc) v -> 
                if acck = v.vk 
                then (v::cacc, acck, gacc)
                else ([v], v.vk, (cacc, v.vk)::gacc)
              )
              ([h], h.vk, []) t
          in 
            List.fold_left  
            ( fun exp (vs, kd) -> 
                match kd with 
                | EQ -> 
                  Exists {vars= (vs, []); body = exp}
                | UQ ->
                  Forall {vars= (vs, []); body = exp}
                | US -> assert false)
            ast nvs
        | [] ->
          Binop (op, quantify_aux x l, quantify_aux y r))
      | Empty -> ast)
    | Unop (op, x) -> Unop (op, quantify_aux x pt) 
    | _ -> ast
  in 
  (* takes a list of couples (list of paths, var) 
   * and returns a a list of couples (common path prefixes, var) *)
  let rec get_q_paths spll = 
    let rec intersect x y = 
      match x, y with 
      | h1::t1, h2::t2 when h1 = h2 -> h1 :: intersect t1 t2 
      | _ -> []
    in 
    (* takes a list of paths and returns their shared prefix*)
    let rec gqp_aux spll = 
      match spll with 
      | h1::h2::t -> gqp_aux ((intersect h1 h2)::t) 
      | h::[] -> h 
      | [] -> []
    in
    match spll with 
    | (p, v) :: t -> (gqp_aux p, v) :: get_q_paths t
    | _ -> []
  in 
  (* takes an ast returns list of couples (path, var) *)
  let rec q_aux path ast =
    match ast with 
    | Binop (_, x, y) ->
      let nx = q_aux (0::path) x in 
      let ny = q_aux (0::path) y in
      if nx = []
      then (
        if ny = []
        then []
        else ny)
      else 
        if ny = []
        then nx
        else nx @ ny
    | Unop (_, x) -> q_aux path x
    | Var ({vk = (EQ|UQ); _} as var) ->
      [(List.rev path, var)]
    | _ -> []
  in 
  (* list of couples *)
  let cpll = 
    List.sort 
      (fun (_, x) (_, y) -> Int.compare x.id y.id) 
      (q_aux [] ast) 
  in 
  (* list of couples (list of paths, var)*)
  match cpll with
  | [] -> ast 
  | h :: t -> 
    let _, _, spll = 
      List.fold_left 
      (fun (cacc, accv, gacc) (p, v) ->
        if v.id = accv.id 
        then (p::cacc, accv, gacc) 
        else ([p], v, (cacc, accv)::gacc))
      ([fst h], snd h, [])
      t 
    in 
    let nspll = get_q_paths spll in 
    let pt = List.fold_left 
        (fun acc (p, v) ->
          insert_in_ptree p v acc)
        Empty nspll
    in 
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

  | Var {name; ty = Tint; vk = US; _} -> 
    Expr.mk_term 
      (Sy.Name (Hstring.make name, Sy.Other)) 
      [] Ty.Tint 
  | Var {name; ty = Treal; vk = US; _} -> 
    Expr.mk_term 
      (Sy.Name (Hstring.make name, Sy.Other)) 
      [] Ty.Treal 
  | Var {name; ty = Tbool; vk = US; _} -> 
    Expr.mk_term 
      (Sy.Name (Hstring.make name, Sy.Other)) 
      [] Ty.Tbool 

  | Fun {name; rty = Tint; args} -> 
    Expr.mk_term 
      (Sy.Name (Hstring.make name, Sy.Other)) 
      (List.map ast_to_expr args) Ty.Tint
  | Fun {name; rty = Treal; args} -> 
    Expr.mk_term 
      (Sy.Name (Hstring.make name, Sy.Other)) 
      (List.map ast_to_expr args) Ty.Treal
  | Fun {name; rty = Tbool; args} -> 
    Expr.mk_term 
      (Sy.Name (Hstring.make name, Sy.Other)) 
      (List.map ast_to_expr args) Ty.Tbool
      
  | Unop (Neg, x) -> 
    Expr.neg (ast_to_expr x)
  | Unop (Not, x) -> 
    Expr.neg (ast_to_expr x)

  | Var {name; ty=Tint; vk=(EQ|UQ); _} -> 
    Expr.mk_term 
      (Sy.Name (Hstring.make name, Sy.Other)) 
      [] Ty.Tint
  | Var {name; ty=Treal; vk=(EQ|UQ); _} -> 
    Expr.mk_term 
      (Sy.Name (Hstring.make name, Sy.Other)) 
      [] Ty.Treal 
  | Var {name; ty=Tbool; vk=(EQ|UQ); _} -> 
    Expr.mk_term 
      (Sy.Name (Hstring.make name, Sy.Other)) 
      [] Ty.Tbool 

  | Forall {vars = (vs, trs); body} -> 
    ignore trs; (* no support for triggers yet *)
    Expr.mk_forall 
      "" Loc.dummy 
      ( List.fold_left 
          ( fun acc v -> 
              let rty = (
                match v.ty with 
                | Tint -> Ty.Tint
                | Treal -> Ty.Treal
                | Tbool -> Ty.Tbool) 
              in
              Sy.Map.add 
                (Sy.Var (Var.of_string v.name)) 
                (rty, v.id) acc)
          Sy.Map.empty vs)
      []
      (ast_to_expr body) (-42) 
      ~toplevel:false 
      ~decl_kind:Expr.Dgoal
  | Exists {vars = (vs, trs); body} ->
    ignore trs; (* no support for triggers yet *)
    Expr.mk_exists 
      "" Loc.dummy 
      ( List.fold_left 
          ( fun acc v -> 
              let rty = (
                match v.ty with 
                | Tint -> Ty.Tint
                | Treal -> Ty.Treal
                | Tbool -> Ty.Tbool) 
              in
              Sy.Map.add 
                (Sy.Var (Var.of_string v.name)) 
                (rty, v.id) acc)
          Sy.Map.empty vs)
      []
      (ast_to_expr body) (-42) 
      ~toplevel:false 
      ~decl_kind:Expr.Dgoal
