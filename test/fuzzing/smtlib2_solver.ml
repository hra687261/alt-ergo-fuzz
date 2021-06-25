
open Ast 

module Make(Solver: Solvers.S) : Translate.T = 
struct 
  type sort = typ

  type sexp =
    | Atom of string  
    | Expr of sexp Queue.t
    | PExpr of sexp Queue.t

  type t = sexp 

  module SS = Set.Make(String)

  module VM = Map.Make(
    struct
      type t = tvar 
      let compare v1 v2 =
        let r = 
          typ_compare v1.vty v2.vty 
        in 
        if r <> 0 then r
        else compare v1.vname v2.vname
    end)

  module GTM = Map.Make(
    struct 
      type t = gtyp 
      let compare = gtyp_compare
    end
    )

  let binop_to_str (op: binop) = 
    match op with 
    | And -> "and"
    | Or -> "or"
    | Xor -> "xor"
    | Imp -> "=>"
    | Iff -> "<=>" 
    | Lt -> "<"
    | Le -> "<="
    | Gt -> ">"
    | Ge -> ">="
    | Eq -> "="
    | Neq -> "distinct"
    | IAdd | RAdd -> "+"
    | ISub | RSub -> "-"
    | IMul | RMul -> "*"
    | IDiv | RDiv -> "div"
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

    | _ -> assert false 

  let rec translate_ast (a: ast) = 
    match a with 
    | Cst (CstI i) -> 
      Atom (
        if i < 0 
        then Format.sprintf "(- %i)" (-i)
        else Format.sprintf "%i" i
      )
    | Cst (CstR r) -> 
      Atom (
        if r < 0. 
        then Format.sprintf "(- %f)" (-.r)
        else Format.sprintf "%f" r
      )
    | Cst (CstB true) -> Atom "true"
    | Cst (CstB false) -> Atom "false"
    | Cst (CstBv {bits; _}) -> 
      Atom (Format.sprintf "#b%s" bits) 

    | Var {vname; _} -> Atom vname

    | Unop (Access {fa; _}, x) -> 
      let q = Queue.create () in
      Queue.push (Atom "select") q;
      Queue.push (translate_ast fa) q;
      Queue.push (translate_ast x) q;
      PExpr q

    | Unop (uop, x) -> 
      let q = Queue.create () in
      Queue.push (unop_to_str uop) q;
      Queue.push (translate_ast x) q;
      PExpr q

    | Binop (bop, x, y) -> 
      let q = Queue.create () in
      Queue.push (Atom (binop_to_str bop)) q;
      Queue.push (translate_ast x) q;
      Queue.push (translate_ast y) q;
      PExpr q

    | ITE  {cond; cons; alt; _} ->
      let q = Queue.create () in
      Queue.push (Atom "ite") q;
      Queue.push (translate_ast cond) q;
      Queue.push (translate_ast cons) q;
      Queue.push (translate_ast alt) q;
      PExpr q

    | LetIn ({vname; _}, x, y) ->
      let vbq = Queue.create () in
      Queue.push (Atom vname) vbq;
      Queue.push (translate_ast x) vbq;
      let vb = PExpr vbq in

      let vsb = Queue.create () in
      Queue.push (vb) vsb;
      let rvb = PExpr vsb in

      let q = Queue.create () in
      Queue.push (Atom "let") q;
      Queue.push rvb q;
      Queue.push (translate_ast y) q;
      PExpr q

    | FunCall {fname; args; _} -> 
      let q = Queue.create () in
      Queue.push (Atom fname) q;
      List.iter (
        fun a ->
          Queue.push (translate_ast a) q
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
      Queue.push (translate_ast body) q;
      PExpr q

    | FAUpdate {fa; i; v; _} -> 
      let q = Queue.create () in
      Queue.push (Atom "store") q;
      Queue.push (translate_ast fa) q;
      Queue.push (translate_ast i) q;
      Queue.push (translate_ast v) q;
      PExpr q
    | Dummy -> assert false 

  let translate_decl (d: decl) =
    match d with 
    | Axiom {name; body} ->
      ignore name;
      let q = Queue.create () in 
      Queue.push (Atom "assert") q;
      Queue.push (translate_ast body) q;
      PExpr q

    | Goal {name; body} -> 
      ignore name;
      let qg = Queue.create () in 
      Queue.push (Atom "assert") qg;
      Queue.push (translate_ast body) qg;
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
      Queue.push (translate_ast body) q;
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
            | A s -> 
              Format.fprintf fmt
                "(declare-const %s %a)@." 
                str print_sort s
            | F  {atyp; rtyp} -> 
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

  let print_decl fmt (gtm: SS.t Ast.GTM.t) d = 
    let gtl = get_usyms (
        match d with 
        | Axiom {body; _} -> body
        | Goal {body; _} -> body
        | FuncDef {body; _} -> body)
    in
    let (ngtm, gtm) : (SS.t Ast.GTM.t * SS.t Ast.GTM.t) = get_ngtm gtm gtl in 
    let se = translate_decl d in 
    Format.fprintf fmt "\n%a@." print_gtm ngtm;
    Format.fprintf fmt "\n%a@." print_sexp se; 
    gtm

  let print_decls fmt (decls: decl list) =
    Format.fprintf fmt "\n(set-logic ALL)@.";
    ignore @@
    List.fold_left (print_decl fmt) Ast.GTM.empty decls  

  let process_decls (decls: Ast.decl list) =
    let rec get_lines (ic: in_channel) =
      try
        input_line ic :: get_lines ic
      with End_of_file ->
        close_in ic; 
        []
    in
    let filename = "_.smt2" in 
    let oc = open_out filename in 
    Format.fprintf
      (Format.formatter_of_out_channel oc) 
      "%a" print_decls decls;
    let ic = 
      Unix.open_process_in 
        (Format.sprintf "%s %s" (Solver.get_exec_str ()) filename)
    in
    let lines = get_lines ic in 
    List.map (
      function 
      | "sat" -> Translate.Sat
      | "unsat" -> Translate.Unsat
      | "unknown" -> Translate.Unknown 
      | _ -> assert false
    ) lines

end
