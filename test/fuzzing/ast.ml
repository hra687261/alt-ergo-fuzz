
type typ = 
  | Tint | Treal | Tbool | TDummy
  | TBitV of int 
  | TFArray of {ti: typ; tv: typ}

type vkind = 
  | EQ (* Exisancially quantified *)
  | UQ (* Universally quantified *)
  | US (* Uninterpreted symbol *)
  | ARG (* Function/predicate argument *)

type tvar = 
  { vname: string; vty: typ; 
    vk: vkind; id: int}


module VS = Set.Make(
  struct 
    type t = tvar 
    let compare x y = Int.compare x.id y.id
  end 
  )

let dpt = 3 
let query_max_depth = dpt
let axiom_max_depth = dpt
let func_max_depth = dpt

let nb_us_vars = 5
let nb_q_vars = 5

let max_nb_fun_args = 5

let v_id, thmid, axid, gid, qid, fid = 
  ref 0, ref 0, ref 0, ref 0, ref 0, ref 0

type cst = 
  | CstI of int 
  | CstR of float 
  | CstB of bool 
  | CstBv of bitv 
and bitv = {length : int; bits : string}

type ast = 
  | Cst of cst
  | Var of tvar
  | Unop of unop * ast 
  | Binop of binop * ast * ast
  | FAUpdate of {ty: typ * typ; fa: ast; i: ast; v: ast}
  | FunCall of fcall
  | Forall of quant
  | Exists of quant
  | Dummy

and binop = 
  | And | Or | Xor | Imp | Iff
  | Lt | Le | Gt | Ge | Eq | Neq
  | RAdd | RSub | RMul | RDiv | RPow
  | IAdd | ISub | IMul | IDiv | IMod | IPow 
  | Concat of int
and unop = 
  | Neg | Not 
  | Extract of {l: int; r: int}
  | Access of {ty: typ * typ; fa: ast}

and quant =
  {qvars: VS.t; trgs: ast list; body: ast}
and fcall =
  {fname: string; rtyp: typ; args: ast list}

(* tuple with the max of arguments a function can have*)
type aty = typ * typ * typ * typ * typ 

type cmd =
  | Axiom of {name: string; body: ast} 
  | Goal of {name: string; body: ast}
  | FuncDef of fdef
and fdef = 
  { name: string; body: ast; 
    atyp: tvar list; rtyp : typ}

type fd_info = 
  {fn: string; params: aty; rtyp: typ}

(* Pretty printing *)

let print_bitv fmt bitv = 
  if bitv.length <= 8 then
    Format.fprintf fmt "[|%s|]" bitv.bits 
  else 
    Format.fprintf fmt "[|size=%d|]" bitv.length

let rec print_typ fmt typ = 
  match typ with 
  | Tint -> Format.fprintf fmt "int"
  | Treal -> Format.fprintf fmt "real"
  | Tbool -> Format.fprintf fmt "bool"
  | TBitV n -> Format.fprintf fmt "bitv[%d]" n 
  | TFArray {ti = Tint; tv} ->
    Format.fprintf fmt 
      "<%a> farray"
      print_typ tv
  | TFArray {ti; tv} ->
    Format.fprintf fmt 
      "(<%a>, <%a>) farray"
      print_typ ti
      print_typ tv
  | TDummy -> Format.fprintf fmt "tdummy"

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

  | Unop (Neg, ast) ->
    Format.fprintf fmt "- (%a)" print ast 
  | Unop (Not, ast) ->
    Format.fprintf fmt "not (%a)" print ast 
  | Unop (Extract {l;r}, ast) ->
    Format.fprintf fmt "(%a)^{%d,%d}" 
      print ast l r 
  | Unop (Access {ty = _; fa} , i) -> 
    Format.fprintf fmt "%a[%a]" print fa print i

  | FAUpdate {ty = _; fa; i; v} -> 
    Format.fprintf fmt "%a[%a <- %a]" 
      print fa print i print v;
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
  | Dummy -> assert false 

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
let rec typ_to_str ty =
  match ty with
  | Tint -> "i"
  | Treal -> "r"
  | Tbool -> "b"
  | TBitV n -> "bv" ^ string_of_int n
  | TFArray {ti; tv} -> 
    Format.sprintf "s%s_%se" 
      (typ_to_str ti) (typ_to_str tv)
  | _ -> assert false

let is_dummy_tvar {vty; _} =
  match vty with 
  | TDummy -> true 
  | _ -> false 

let mk_vname pref num = 
  pref ^ string_of_int num

let mk_tvar_b vname vty vk id = 
  {vname; vty; vk; id}

let mk_tvar vname vty vk = 
  mk_tvar_b vname vty vk (incr v_id; !v_id)

let mk_var vname vty vk = 
  Var {vname; vty; vk; id = (incr v_id; !v_id)}

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

let is_dummy a = 
  match a with 
  | Dummy -> true 
  | _ -> false
(* Uninterpreted variables *)

let get_uvar_ast num ty = 
  let vname =
    match ty with 
    | Tint -> "ui_"^ string_of_int num 
    | Treal -> "ur_"^ string_of_int num
    | Tbool -> "ub_"^ string_of_int num
    | TBitV n -> "ubv_"^string_of_int n^"_"^ string_of_int num
    | TFArray {ti; tv} -> 
      (Format.sprintf "ufa_%s_%s_"
         (typ_to_str ti) (typ_to_str tv))^ string_of_int num
    | _ -> assert false
  in    
  Var (mk_tvar_b vname ty US 0)

(* Uninterpreted functions *)

let get_args num = 
  match num with
  | 1 -> [Tint; TDummy; TDummy; TDummy; TDummy]
  | 2 -> [Tint; Treal; TDummy; TDummy; TDummy]
  | 3 -> [Tint; Treal; Tbool; TDummy; TDummy]
  | 4 -> [Tint; Treal; Tbool; Tint; TDummy]
  | 5 -> [Tint; Treal; Tbool; Tint; Treal]
  | _ -> assert false 

let mk_aty l = 
  match l with 
  | [a; b; c; d; e] -> a, b, c, d, e
  | _ -> assert false

let get_ufunc_ast num rtyp = 
  let fn =
    match rtyp with 
    | Tint -> "ufi_"^ string_of_int num 
    | Treal -> "ufr_"^ string_of_int num
    | Tbool -> "ufb_"^ string_of_int num
    | TBitV n -> "ufbv_"^string_of_int n^"_"^ string_of_int num
    | TFArray {ti; tv} -> 
      (Format.sprintf "uffa_%s_%s_"
         (typ_to_str ti) (typ_to_str tv))^ string_of_int num
    | _ -> assert false
  in
  let params =
    mk_aty (get_args num)
  in
  {fn; params; rtyp}


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

