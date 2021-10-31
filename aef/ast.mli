(** The type of types *)
type ty =
  | Tint
  (** Integers *)

  | Treal
  (** Reals *)

  | Tbool
  (** Booleans *)

  | TBitV of int
  (** Bit vectors *)

  | TFArray of { ti: ty; tv: ty; }
  (** [TFArray {ti; tv}] a functional array in which
      [ti] is the type of the indexes and
      [tv] is the type of the stored values *)

  | Tadt of adt
  (** Algebraic data type *)

  | TDummy
  (** Dummy type *)

(** Description of an algebraic data type *)
and adt = string * rcrd_ty list

(** Description of a record type *)
and rcrd_ty = string * (string * ty) list

(** A type declaration *)
type typedecl = Adt_decl of adt | Record_decl of rcrd_ty

(** A funcion's type *)
type ftyp = { atyp: ty list; rtyp: ty; }

(** A type containter, with the a unique tag associated to each type *)
type typc =
    A of { tag: string; ty: ty; }
  | F of { tag: string; atyp: ty list; rtyp: ty; }

(** Variable kinds  *)
type vkind =
    EQ  (** Existantially quantified *)
  | UQ  (** Universally quantified *)
  | US  (** Uninterpreted symbol *)
  | ARG (** Bound as an argument to a function *)
  | BLI (** Bound to a let-in statement *)

(** Function kind *)
type fkind =
  | UDF
  (** A defined function *)

  | USF
  (** A uninterpreted function *)

(** A variable container *)
type tvar = { vname: string; vty: ty; vk: vkind; id: int; }

(** [typ_tag ty] returns the tag of the type [ty]. *)
val typ_tag: ty -> string

(** [typc_tag ftyp] returns the tag of a function type [fty]. *)
val typc_tag: ftyp -> string

(** [typ_compare ty1 ty2] compares two types by comparing their tags.
    Returns [1] if the first is greater than the second,
    [0] if they're equal and [-1] otherwise. *)
val typ_compare: ty -> ty -> int

(** [get_tctag typc] returns the tag of a type container [typc]. *)
val get_tctag: typc -> string

module VS: Set.S with type elt = tvar

(** The type of constants *)
type cst =
  | CstI of int
  (** Integer literal *)

  | CstR of float
  (** Real literal *)

  | CstB of bool
  (** Boolean literal *)

  | CstBv of bitv
  (** Bitvector literal *)

(** A bitvector literal container  *)
and bitv = { length: int; bits: string; }

(** An SMT expression *)
type expr =

  | Cst of cst
  (** Constant *)

  | Var of tvar
  (** Variable *)

  | Unop of unop * expr
  (** [Unop (un, e)] Represents the unary application of [un] on [e] *)

  | Binop of binop * expr * expr
  (** [Binop (bin, e1, e1)] Represents the binary application
      of [bin] on [e1] and [e2] *)

  | ITE of { ty: ty; cond: expr; cons: expr; alt: expr; }
  (** [ITE {ty; cond; expr; cons; alt}] An if-then-else conditional expression
      in which the condition is [cond], the consequences is [cons], and the
      alternative is [alt] and [ty] is the type of the [cons] and [alt]
      expressions *)

  | LetIn of tvar * expr * expr
  (** [LetIn (v,e1,e2)] A let binding in which the variable [v] is bound to the
      expresion [e1] in the expression [e2] *)

  | FAUpdate of { ty: ty * ty; fa: expr; i: expr; v: expr; }
  (** [FAUpdate {ty; fa; i; v}] A write operation on the functional array [fa]
      which is of type [ty], and in which [i] is the index to be written and
      [v] is the value to be stored in [i] *)

  | FunCall of fcall
  (** [FunCall {fname; fk; atyp; rtyp; args}] A call to the function named
      [fname] which is of kind [fk], and has the return type [rtyp], with
      the arguments in [args], which have the types in [atyp] *)

  | Forall of quant
  (** [Forall {qvars; trgs; body}] Universal quantification of the variables
      [qvars] in the expression [body] *)

  | Exists of quant
  (** [Exists {qvars; trgs; body}] Existential quantification of the variables
      [qvars] in the expression [body] *)

  | PMatching of pm
  (** [PMatching {mtchdv; patts; valty}] A pattern matchin in which the
      expresion [mtchdv] is matched on the patterns [patts] which return
      values of type [valty] *)

  | Cstr of constr
  (** [Cstr {cname; cty; params}] A contructor of an algebraic data type named
      [cname] of type [cty] and parametrized with the expressions in [params] *)

  | Dummy
  (** [Dummy] *)

and pm = { mtchdv: expr; patts: patt list; valty: ty; }

and patt = { destrn: string; pattparams: tvar option list; mbody: expr; }

and constr = { cname: string; cty: ty; params: (string * expr) list; }

and binop =
  | And
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
  | Neg
  | Not
  | Extract of { l: int; r: int; }
  | Access of { ty: ty * ty; fa: expr; }

and quant = { qvars: VS.t; trgs: expr list; body: expr; }

and fcall = {
  fname: string;
  fk: fkind;
  atyp: ty list;
  rtyp: ty;
  args: expr list;
}

type stmt =
    Axiom of { name: string; body: expr; }
  | Goal of { name: string; body: expr; }
  | FuncDef of fdef

and fdef = { name: string; body: expr; atyp: tvar list; rtyp: ty; }

type fd_info = { fn: string; params: ty list; rtyp: ty; }

type stmtkind = FD | AxD | GD

module SS: Set.S with type elt = String.t

module TDS: Set.S with type elt = typedecl

module TCM: Map.S with type key = typc

type stmt_c = { stmt: stmt; tds: TDS.t; uss: SS.t TCM.t; }


val print_typ: Format.formatter -> ty -> unit

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

val typ_to_str: ty -> string

val mk_tvar: string -> ty -> vkind -> tvar

val mk_var: string -> ty -> vkind -> expr

val mk_binop: binop -> expr -> expr -> expr

val int_to_bitv: ?wl:int -> int -> bitv

val get_u_tvar: int -> ty -> tvar

val get_args: int -> ty list

val get_ufunc_expr: int -> ty -> fd_info

val mk_bound_var: ty -> tvar

val quantify: expr -> expr
