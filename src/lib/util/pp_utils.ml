
module F = Format

type fmt = F.formatter

type box_type =
  | Pp_hbox of int
  | Pp_vbox of int
  | Pp_hvbox of int
  | Pp_hovbox of int
  | Pp_box of int


let hbox n = Pp_hbox n
let vbox n = Pp_vbox n
let hvbox n = Pp_hvbox n
let hovbox n = Pp_hovbox n
let box n = Pp_box n


let pp_sc_cut ppf () = F.fprintf ppf ";@,"
let pp_sc_spc ppf () = F.fprintf ppf ";@ "
let pp_c_spc ppf () = F.fprintf ppf ",@ "


let dbox = ref (Pp_hovbox 2)
let dsep = ref pp_c_spc
let dpref = ref ""


let set_dbox = (:=) dbox
let get_dbox () = !dbox

let set_dsep = (:=) dsep
let get_dsep () = !dsep

let set_dpref = (:=) dpref
let get_dpref () = !dpref


let add_p ?(p = !dpref) pp_v ppf v =
  F.fprintf ppf "%s[%a]" p pp_v v

let enbox_pp ?(bt = !dbox) pp ppf v =
  match bt with
  | Pp_box n when n > 0 -> F.fprintf ppf "@[<b %d>%a@]" n pp v
  | Pp_box n when n = 0 -> F.fprintf ppf "@[%a@]" pp v
  | Pp_hbox n when n > 0 -> F.fprintf ppf "@[<h %d>%a@]" n pp v
  | Pp_hbox n when n = 0 -> F.fprintf ppf "@[<h>%a@]" pp v
  | Pp_vbox n when n > 0 -> F.fprintf ppf "@[<v %d>%a@]" n pp v
  | Pp_vbox n when n = 0 -> F.fprintf ppf "@[<v>%a@]" pp v
  | Pp_hvbox n when n > 0 -> F.fprintf ppf "@[<hv %d>%a@]" n pp v
  | Pp_hvbox n when n = 0 -> F.fprintf ppf "@[<hv>%a@]" pp v
  | Pp_hovbox n when n > 0 -> F.fprintf ppf "@[<hov %d>%a@]" n pp v
  | Pp_hovbox n when n = 0 -> F.fprintf ppf "@[<hov>%a@]" pp v
  | _ ->
    raise (Invalid_argument "box_type can't have a negative indentation")


let pp_option ?(p = !dpref) pp_v ppf = function
  | None -> F.fprintf ppf "%sNone" p
  | Some v -> F.fprintf ppf "%sSome %a" p pp_v v

let pp_doublet ?(p = !dpref) ?(pp_sep1 = !dsep)
    pp_v1 pp_v2 ppf (v1, v2) =
  F.pp_print_string ppf p;
  F.pp_print_string ppf "(";
  pp_v1 ppf v1; pp_sep1 ppf ();
  pp_v2 ppf v2;
  F.pp_print_string ppf ")"

let pp_triplet ?(p = !dpref) ?(pp_sep1 = !dsep) ?(pp_sep2 = !dsep)
    pp_v1 pp_v2 pp_v3 ppf (v1, v2, v3) =
  F.pp_print_string ppf p;
  F.pp_print_string ppf "(";
  pp_v1 ppf v1; pp_sep1 ppf ();
  pp_v2 ppf v2; pp_sep2 ppf ();
  pp_v3 ppf v3;
  F.pp_print_string ppf ")"

let pp_quadruplet ?(p = !dpref)
    ?(pp_sep1 = !dsep) ?(pp_sep2 = !dsep) ?(pp_sep3 = !dsep)
    pp_v1 pp_v2 pp_v3 pp_v4 ppf (v1, v2, v3, v4) =
  F.pp_print_string ppf p;
  F.pp_print_string ppf "(";
  pp_v1 ppf v1; pp_sep1 ppf ();
  pp_v2 ppf v2; pp_sep2 ppf ();
  pp_v3 ppf v3; pp_sep3 ppf ();
  pp_v4 ppf v4;
  F.pp_print_string ppf ")"

let pp_quintuplet ?(p = !dpref)
    ?(pp_sep1 = pp_c_spc) ?(pp_sep2 = pp_c_spc)
    ?(pp_sep3 = pp_c_spc) ?(pp_sep4 = pp_c_spc)
    pp_v1 pp_v2 pp_v3 pp_v4 pp_v5 ppf (v1, v2, v3, v4, v5) =
  F.pp_print_string ppf p;
  F.pp_print_string ppf "(";
  pp_v1 ppf v1; pp_sep1 ppf ();
  pp_v2 ppf v2; pp_sep2 ppf ();
  pp_v3 ppf v3; pp_sep3 ppf ();
  pp_v4 ppf v4; pp_sep4 ppf ();
  pp_v5 ppf v5;
  F.pp_print_string ppf ")"


let pp_list ?(p = !dpref) ?(pp_sep = !dsep)
    pp_v ppf l =
  F.fprintf ppf "%s[%a]" p (F.pp_print_list ~pp_sep pp_v) l

let pp_array ?(p = !dpref) ?(pp_sep = !dsep) pp_v ppf a =
  let pp_a ppf a =
    let len = Array.length a in
    let cnt = ref 0 in
    Array.iter (
      fun v ->
        pp_v ppf v;
        if !cnt < len - 1 then (pp_sep ppf (); incr cnt)
    ) a
  in
  F.fprintf ppf "%s[|%a|]" p pp_a a

let pp_queue ?(p = !dpref) ?(pp_sep = !dsep) pp_v ppf q =
  let pp_q ppf q =
    let len = Queue.length q in
    let cnt = ref 0 in
    Queue.iter (
      fun v ->
        pp_v ppf v;
        if !cnt < len - 1 then (pp_sep ppf (); incr cnt)
    ) q
  in
  F.fprintf ppf "%s{|%a|}" p pp_q q

let pp_stack ?(p = !dpref) ?(pp_sep = !dsep) pp_v ppf s =
  let pp_s ppf s =
    let len = Stack.length s in
    let cnt = ref 0 in
    Stack.iter (
      fun v ->
        pp_v ppf v;
        if !cnt < len - 1 then (pp_sep ppf (); incr cnt)
    ) s
  in
  F.fprintf ppf "%s{|%a|}" p pp_s s

let pp_set (type a t) ?(p = !dpref) ?(pp_sep = !dsep)
    (module S: Set.S with type elt = a and type t = t) pp_v ppf s =
  let pp_s ppf s =
    let card = S.cardinal s in
    let cnt = ref 0 in
    S.iter (
      fun v ->
        pp_v ppf v;
        if !cnt < card - 1 then (pp_sep ppf (); incr cnt)
    ) s
  in
  F.pp_print_string ppf p;
  F.pp_print_string ppf "{";
  pp_s ppf s;
  F.pp_print_string ppf "}"


module type MS = sig
  type key
  type 'a t
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val cardinal : 'a t -> int
end

module MapPrinter(M: MS):
sig
  val pp:
    ?p:string ->
    ?pp_kv_sep:(fmt -> unit -> unit) ->
    ?pp_sep:(fmt -> unit -> unit) ->
    (fmt -> M.key -> unit) ->
    (fmt -> 'a -> unit) ->
    fmt -> 'a M.t -> unit
end =
struct
  let pp ?(p = !dpref) ?(pp_kv_sep = !dsep) ?(pp_sep = !dsep)
      pp_k pp_v ppf m =
    let pp_m ppf m =
      let card = M.cardinal m in
      let cnt = ref 0 in
      M.iter (
        fun k v ->
          (pp_doublet ~pp_sep1:pp_kv_sep pp_k pp_v) ppf (k, v);
          if !cnt < card - 1 then pp_sep ppf ();
          incr cnt
      ) m
    in
    F.pp_print_string ppf p;
    F.pp_print_string ppf "{";
    pp_m ppf m;
    F.pp_print_string ppf "}"
end

module type HTS = sig
  type key
  type 'a t
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val length : 'a t -> int
end

module HTPrinter(M: HTS) :
sig
  val pp:
    ?p:string ->
    ?pp_kv_sep:(fmt -> unit -> unit) ->
    ?pp_sep:(fmt -> unit -> unit) ->
    (fmt -> M.key -> unit) ->
    (fmt -> 'a -> unit) ->
    fmt -> 'a M.t -> unit
end =
struct
  let pp ?(p = !dpref) ?(pp_kv_sep = !dsep) ?(pp_sep = !dsep)
      pp_k pp_v ppf m =
    let pp_m ppf m =
      let l = M.length m in
      let cnt = ref 0 in
      M.iter (
        fun k v ->
          pp_doublet ~pp_sep1:pp_kv_sep pp_k pp_v ppf (k, v);
          if !cnt < l - 1 then pp_sep ppf ();
          incr cnt
      ) m
    in
    F.pp_print_string ppf p;
    F.pp_print_string ppf "{";
    pp_m ppf m;
    F.pp_print_string ppf "}"
end
