open Ast 

type sort = typ

type sexp =
  | Atom of string  
  | Expr of sexp Queue.t
  | PExpr of sexp Queue.t

type t = sexp 

let binop_to_str (op: binop) = 
  match op with 
  | And -> "and"
  | Or -> "or"
  | Xor -> "xor"
  | Imp -> "=>"
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="
  | Iff | Eq -> "="
  | Neq -> "distinct"
  | IAdd | RAdd -> "+"
  | ISub | RSub -> "-"
  | IMul | RMul -> "*"
  | IDiv -> "div"
  | RDiv -> "/"
  | IPow | RPow -> "^" 
  | IMod -> "mod"
  | Concat _ -> "concat"

let unop_to_str (op: unop) =
  match op with 
  | Neg -> Atom "-"
  | Not -> Atom "not"
  | Extract {l; r} ->
    let qe = Queue.create () in
    Queue.push (Atom "extract") qe;
    Queue.push (Atom (string_of_int l)) qe;
    Queue.push (Atom (string_of_int r)) qe;
    PExpr qe
  | Access _ -> assert false

let rec translate_sort (s: sort) =
  match s with
  | Tint -> Atom "Int"
  | Treal -> Atom "Real"
  | Tbool -> Atom "Bool"

  | TBitV n ->
    let q = Queue.create () in
    Queue.push (Atom "_") q;
    Queue.push (Atom "BitVec") q;
    Queue.push (Atom (string_of_int n)) q;
    PExpr q

  | TFArray {ti; tv} -> 
    let is = translate_sort ti in 
    let vs = translate_sort tv in 
    let q = Queue.create () in
    Queue.push (Atom "Array") q;
    Queue.push is q;
    Queue.push vs q;
    PExpr q

  | Tadt (adtn, _) -> 
    Atom adtn

  | TDummy -> assert false

let translate_typedecl (adtn, patterns) =
  let tr_pattern 
      (ptrn, prms: string * (string * sort) list) =
    let q = Queue.create () in
    Queue.push (Atom ptrn) q;
    List.iter (
      fun (n, so) -> 
        let tmpq = Queue.create () in
        Queue.push (Atom n) tmpq;
        Queue.push (translate_sort so) tmpq;
        Queue.push (PExpr tmpq) q
    ) prms;
    PExpr q
  in 
  let q = Queue.create () in
  Queue.push (Atom "declare-datatype") q;
  Queue.push (Atom adtn) q;

  let pq = Queue.create () in
  List.iter (
    fun patt -> Queue.push (tr_pattern patt) pq
  ) patterns;
  Queue.push (PExpr pq) q;
  PExpr q

let rec translate_expr (a: expr) = 
  match a with 
  | Cst (CstI i) -> 
    Atom (
      if i < 0 
      then Format.sprintf "(- %i)" (-i)
      else Format.sprintf "%i" i
    )
  | Cst (CstR r) -> 
    let float_to_str f = 
      Str.replace_first 
        (Str.regexp "0+$") "" 
        (Printf.sprintf "%.715f " f) 
    in
    Atom (
      if r < 0. 
      then Format.sprintf "(- %s)" (float_to_str (-.r))
      else Format.sprintf "%s" (float_to_str r)
    )
  | Cst (CstB true) -> Atom "true"
  | Cst (CstB false) -> Atom "false"
  | Cst (CstBv {bits; _}) -> 
    Atom (Format.sprintf "#b%s" bits) 

  | Var {vname; _} -> Atom vname

  | Unop (Access {fa; _}, x) -> 
    let q = Queue.create () in
    Queue.push (Atom "select") q;
    Queue.push (translate_expr fa) q;
    Queue.push (translate_expr x) q;
    PExpr q

  | Unop (uop, x) -> 
    let q = Queue.create () in
    Queue.push (unop_to_str uop) q;
    Queue.push (translate_expr x) q;
    PExpr q

  | Binop (bop, x, y) -> 
    let q = Queue.create () in
    Queue.push (Atom (binop_to_str bop)) q;
    Queue.push (translate_expr x) q;
    Queue.push (translate_expr y) q;
    PExpr q

  | ITE  {cond; cons; alt; _} ->
    let q = Queue.create () in
    Queue.push (Atom "ite") q;
    Queue.push (translate_expr cond) q;
    Queue.push (translate_expr cons) q;
    Queue.push (translate_expr alt) q;
    PExpr q

  | LetIn ({vname; _}, x, y) ->
    let vbq = Queue.create () in
    Queue.push (Atom vname) vbq;
    Queue.push (translate_expr x) vbq;
    let vb = PExpr vbq in

    let vsb = Queue.create () in
    Queue.push (vb) vsb;
    let rvb = PExpr vsb in

    let q = Queue.create () in
    Queue.push (Atom "let") q;
    Queue.push rvb q;
    Queue.push (translate_expr y) q;
    PExpr q

  | FunCall {fname; args; _} -> 
    let q = Queue.create () in
    Queue.push (Atom fname) q;
    List.iter (
      fun a ->
        Queue.push (translate_expr a) q
    ) (List.rev args);
    PExpr q

  | Forall {qvars; body; _}
  | Exists {qvars; body; _} -> 
    let q = Queue.create () in
    Queue.push (Atom (
        match a with 
        | Forall _ -> "forall"
        | Exists _ -> "exists"
        | _ -> assert false
      )) q;
    let vsb = Queue.create () in
    VS.iter (
      fun {vname; vty; _} ->
        let vq = Queue.create () in  
        Queue.push (Atom vname) vq;
        Queue.push (translate_sort vty) vq;
        Queue.push (PExpr vq) vsb
    ) qvars;
    Queue.push (PExpr vsb) q;
    Queue.push (translate_expr body) q;
    PExpr q

  | FAUpdate {fa; i; v; _} -> 
    let q = Queue.create () in
    Queue.push (Atom "store") q;
    Queue.push (translate_expr fa) q;
    Queue.push (translate_expr i) q;
    Queue.push (translate_expr v) q;
    PExpr q

  | PMatching {mtchdv; patts; _} -> 
    let mk_match_case {destrn; pattparams; mbody} =
      let pattern = 
        begin
          let q = Queue.create () in
          Queue.add (Atom destrn) q;
          if pattparams = []
          then 
            Expr q
          else (
            List.iter (
              fun v ->
                match v with 
                | Some {vname; _ } ->
                  Queue.add (Atom vname) q
                | None -> 
                  Queue.add (Atom "_") q
            ) pattparams;
            PExpr q
          )
        end
      in 
      let term = translate_expr mbody in 
      let q = Queue.create () in
      Queue.add pattern q;
      Queue.add term q;
      PExpr q
    in 
    let qmc = Queue.create () in
    List.iter (
      fun p -> 
        Queue.add (mk_match_case p) qmc
    ) patts;

    let q = Queue.create () in
    Queue.push (Atom "match") q;
    Queue.push (translate_expr mtchdv) q;
    Queue.push (PExpr qmc) q;
    PExpr q

  | Cstr {cname; params; _} ->
    ignore (cname, params);
    let q = Queue.create () in
    Queue.push (Atom cname) q;
    List.iter (
      fun (_, a) -> 
        Queue.push (translate_expr a) q;
    ) params;
    PExpr q

  | Dummy -> assert false 

let translate_stmt (d: stmt) =
  match d with 
  | Axiom {name; body} ->
    ignore name;
    let q = Queue.create () in 
    Queue.push (Atom "assert") q;
    Queue.push (translate_expr body) q;
    PExpr q

  | Goal {name; body} -> 
    ignore name;
    let qg = Queue.create () in 
    Queue.push (Atom "assert") qg;
    Queue.push (translate_expr body) qg;
    let q = Queue.create () in
    Queue.push (PExpr qg) q;
    let cs = Queue.create () in 
    Queue.push (Atom "check-sat") cs;
    Queue.push (PExpr cs) q;
    Expr q

  | FuncDef {name; body; atyp; rtyp} ->
    let q = Queue.create () in 
    Queue.push (Atom "define-fun") q;
    Queue.push (Atom name) q;
    let vsq = Queue.create () in 
    List.iter (
      fun {vname; vty; _} -> 
        let vq = Queue.create () in
        Queue.push (Atom vname) vq;
        Queue.push (translate_sort vty) vq;
        Queue.push (PExpr vq) vsq
    ) atyp;
    Queue.push (PExpr vsq) q;
    Queue.push (translate_sort rtyp) q;
    Queue.push (translate_expr body) q;
    PExpr q

let rec print_sexp fmt s =
  match s with 
  | Atom w -> 
    Format.fprintf fmt "%s" w
  | Expr lq -> 
    ignore @@ 
    Queue.fold (
      fun acc se -> 
        Format.fprintf fmt (
          if acc 
          then "%a"
          else " %a"
        ) print_sexp se; false  
    ) true lq 
  | PExpr lq -> 
    Format.fprintf fmt "(%a)" (
      fun fmt lq -> 
        ignore @@ 
        Queue.fold (
          fun acc se -> 
            Format.fprintf fmt (
              if acc 
              then "%a"
              else " %a"
            ) print_sexp se; false  
        ) true lq 
    ) lq

let print_sort fmt s =
  Format.fprintf fmt "%a" print_sexp (translate_sort s)

let print_gtm fmt (gtm: SS.t Ast.GTM.t) = 
  Ast.GTM.iter (
    fun gs ss -> 
      SS.iter (
        fun str -> 
          match gs with 
          | A {ty; _} -> 
            Format.fprintf fmt
              "(declare-const %s %a)@." 
              str print_sort ty
          | F  {atyp; rtyp; _} -> 
            Format.fprintf fmt
              "(declare-fun %s (%a) %a)@." 
              str
              ( fun fmt sl ->
                  match sl with 
                  | h::t ->
                    Format.fprintf fmt
                      "%a"
                      print_sort h;
                    List.iter (
                      fun s ->
                        Format.fprintf fmt " %a"
                          print_sort s;
                    ) t
                  | _ -> ()
              ) atyp
              print_sort rtyp
      ) ss 
  ) gtm 

let print_stmt fmt (gtm: SS.t Ast.GTM.t) stmt = 
  let gtl, stmt = 
    match stmt with 
    | Axiom {body; _} -> get_usyms body, stmt
    | Goal ({body; _} as d) -> 
      get_usyms body, Goal {d with body = Ast.Unop (Not, body)} 
    | FuncDef {body; _} -> get_usyms body, stmt
  in
  let (ngtm, gtm) : (SS.t Ast.GTM.t * SS.t Ast.GTM.t) = get_ngtm gtm gtl in 
  let se = translate_stmt stmt in 
  Format.fprintf fmt "\n%a@." print_gtm ngtm;
  Format.fprintf fmt "\n%a@." print_sexp se; 
  gtm

let print_typedecls fmt tydecls = 
  List.iter (
    fun td ->
      Format.fprintf fmt "%a\n" 
        print_sexp (translate_typedecl td)
  ) tydecls

let print_stmts fmt (tydecls, stmts: typedecl list * stmt list) =
  Format.fprintf fmt "\n(set-logic ALL)@.";
  print_typedecls fmt tydecls;
  ignore @@
  List.fold_left (print_stmt fmt) Ast.GTM.empty stmts  
