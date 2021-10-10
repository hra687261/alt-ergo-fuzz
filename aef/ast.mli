
type typ =           
    Tint
  | Treal
  | Tbool
  | TDummy
  | TBitV of int
  | TFArray of { ti: typ; tv: typ; }
  | Tadt of adt

and adt = string * rcrd_ty list

and rcrd_ty = string * (string * typ) list

type typedecl = Adt_decl of adt | Record_decl of rcrd_ty

type ftyp = { atyp: typ list; rtyp: typ; }

type typc =
    A of { tag: string; ty: typ; }
  | F of { tag: string; atyp: typ list; rtyp: typ; }

type vkind = EQ | UQ | US | ARG | BLI

type fkind = UDF | USF

type tvar = { vname: string; vty: typ; vk: vkind; id: int; }

val typ_tag: typ -> string

val typc_tag: ftyp -> string

val typ_compare: typ -> typ -> int

val get_tctag: typc -> string

module VS: Set.S with type elt = tvar

type cst = CstI of int | CstR of float | CstB of bool | CstBv of bitv

and bitv = { length: int; bits: string; }

type expr =
    Cst of cst
  | Var of tvar
  | Unop of unop * expr
  | Binop of binop * expr * expr
  | ITE of { ty: typ; cond: expr; cons: expr; alt: expr; }
  | LetIn of tvar * expr * expr
  | FAUpdate of { ty: typ * typ; fa: expr; i: expr; v: expr; }
  | FunCall of fcall
  | Forall of quant
  | Exists of quant
  | PMatching of pm
  | Cstr of constr
  | Dummy

and pm = { mtchdv: expr; patts: patt list; valty: typ; }

and patt = { destrn: string; pattparams: tvar option list; mbody: expr; }

and constr = { cname: string; cty: typ; params: (string * expr) list; }

and binop =
    And
  | Or
  | Xor
  | Imp
  | Iff
  | Lt
  | Le
  | Gt
  | Ge
  | Eq
  | Neq
  | RAdd
  | RSub
  | RMul
  | RDiv
  | RPow
  | IAdd
  | ISub
  | IMul
  | IDiv
  | IMod
  | IPow
  | Concat of int

and unop =
    Neg
  | Not
  | Extract of { l: int; r: int; }
  | Access of { ty: typ * typ; fa: expr; }

and quant = { qvars: VS.t; trgs: expr list; body: expr; }

and fcall = {
  fname: string;
  fk: fkind;
  atyp: typ list;
  rtyp: typ;
  args: expr list;
}

type stmt =
    Axiom of { name: string; body: expr; }
  | Goal of { name: string; body: expr; }
  | FuncDef of fdef

and fdef = { name: string; body: expr; atyp: tvar list; rtyp: typ; }

type fd_info = { fn: string; params: typ list; rtyp: typ; }

type stmtkind = FD | AxD | GD

module SS: Set.S with type elt = String.t

module TDS: Set.S with type elt = typedecl

module TCM: Map.S with type key = typc

type stmt_c = { stmt: stmt; tds: TDS.t; uss: SS.t TCM.t; }


val print_typ: Format.formatter -> typ -> unit

val pr_tvar: Format.formatter -> tvar -> unit

val print_patt_ty: Format.formatter -> rcrd_ty -> unit

val print_adt: Format.formatter -> adt -> unit

val print_ftyp: Format.formatter -> ftyp -> unit

val print_typc: Format.formatter -> typc -> unit

val pr_fdi: Format.formatter -> fd_info -> unit

val print_bitv: Format.formatter -> bitv -> unit

val print_binop: Format.formatter -> binop -> unit

val print: Format.formatter -> expr -> unit

val print_stmt: Format.formatter -> stmt -> unit

val print_stmtc: Format.formatter -> stmt_c -> unit


val float_to_string: float -> string

val typ_to_str: typ -> string

val mk_tvar: string -> typ -> vkind -> tvar

val mk_var: string -> typ -> vkind -> expr

val mk_binop: binop -> expr -> expr -> expr

val int_to_bitv: ?wl:int -> int -> bitv

val get_u_tvar: int -> typ -> tvar

val get_args: int -> typ list

val get_ufunc_expr: int -> typ -> fd_info

val mk_bound_var: typ -> tvar

val quantify: expr -> expr
