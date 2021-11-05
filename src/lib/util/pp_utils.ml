
module F = Format

type fmt = F.formatter

let pp_sc_cut fmt () =
  F.fprintf fmt ";@,"

let pp_sc_spc fmt () =
  F.fprintf fmt ";@ "

let pp_c_spc fmt () =
  F.fprintf fmt ",@ "

let pp_sc_brk fmt () =
  F.fprintf fmt ";@;<1 2>"


let pp_list ?(p = "") ?(pp_sep = pp_sc_spc) pp_v ppf l =
  F.fprintf ppf "%s[%a]" p (F.pp_print_list ~pp_sep pp_v) l

let add_p ?(p = "") pp_v ppf v =
  F.fprintf ppf "%s[%a]" p pp_v v

let pp_array ?(p = "") ?(pp_sep = pp_sc_spc) pp_v ppf a =
  let pp_a ppf a =
    let len = Array.length a in
    let _ =
      Array.fold_left (
        fun acc v ->
          pp_v ppf v;
          if acc < len - 1 then pp_sep ppf ();
          acc + 1
      ) 0 a
    in ()
  in
  F.fprintf ppf "%s[|%a|]" p pp_a a


let pp_queue ?(p = "") ?(pp_sep = pp_sc_spc) pp_v ppf q =
  let pp_q ppf q =
    let len = Queue.length q in
    let _ =
      Queue.fold (
        fun acc v ->
          pp_v ppf v;
          if acc < len - 1 then pp_sep ppf ();
          acc + 1
      ) 0 q
    in ()
  in
  F.fprintf ppf "%s{|%a|}" p pp_q q


let pp_stack ?(p = "") ?(pp_sep = pp_sc_spc) pp_v ppf s =
  let pp_s ppf s =
    let len = Stack.length s in
    let _ =
      Stack.fold (
        fun acc v ->
          pp_v ppf v;
          if acc < len - 1 then pp_sep ppf ();
          acc + 1
      ) 0 s
    in ()
  in
  F.fprintf ppf "%s{|%a|}" p pp_s s


let pp_option ?(p = "") pp_v ppf = function
  | None -> F.fprintf ppf "%sNone" p
  | Some v -> F.fprintf ppf "%sSome %a" p pp_v v


let pp_doublet ?(boxed = false) ?(p = "") ?(pp_sep1 = pp_c_spc)
    pp_v1 pp_v2 ppf (v1, v2) =
  ignore pp_sep1;
  F.fprintf ppf (if boxed then "@[<hov 2>%s(%a, %a)@]" else "%s(%a, %a)") p
    pp_v1 v1 (* pp_sep1 () *) pp_v2 v2


let pp_triplet ?(p = "")
    ?(pp_sep1 = pp_c_spc) ?(pp_sep2 = pp_c_spc)
    pp_v1 pp_v2 pp_v3 ppf (v1, v2, v3) =
  ignore (pp_sep1, pp_sep2);
  F.fprintf ppf "%s(%a, %a, %a)" p
    pp_v1 v1 (* pp_sep1 () *) pp_v2 v2 (* pp_sep2 () *) pp_v3 v3


let pp_quadruplet ?(p = "")
    ?(pp_sep1 = pp_c_spc) ?(pp_sep2 = pp_c_spc) ?(pp_sep3 = pp_c_spc)
    pp_v1 pp_v2 pp_v3 pp_v4 ppf (v1, v2, v3, v4) =
  ignore (pp_sep1, pp_sep2, pp_sep3);
  F.fprintf ppf "%s(%a, %a, %a, %a)" p
    pp_v1 v1 (* pp_sep1 () *)
    pp_v2 v2 (* pp_sep2 () *)
    pp_v3 v3 (* pp_sep3 () *)
    pp_v4 v4


let pp_quintuplet ?(p = "")
    ?(pp_sep1 = pp_c_spc) ?(pp_sep2 = pp_c_spc)
    ?(pp_sep3 = pp_c_spc) ?(pp_sep4 = pp_c_spc)
    pp_v1 pp_v2 pp_v3 pp_v4 pp_v5 ppf (v1, v2, v3, v4, v5) =
  ignore (pp_sep1, pp_sep2, pp_sep3, pp_sep4);
  F.fprintf ppf "%s(%a, %a, %a, %a, %a)" p
    pp_v1 v1 (* pp_sep1 () *)
    pp_v2 v2 (* pp_sep2 () *)
    pp_v3 v3 (* pp_sep3 () *)
    pp_v4 v4 (* pp_sep4 () *)
    pp_v5 v5


let pp_set (type a t) ?(p = "") ?(pp_sep = pp_sc_spc)
    (module S: Set.S with type elt = a and type t = t) pp_v ppf s =
  let pp_s ppf s =
    let card = S.cardinal s in
    let _ =
      S.fold (
        fun v acc ->
          pp_v ppf v;
          if acc < card - 1 then pp_sep ppf ();
          acc + 1
      ) s 0
    in ()
  in
  F.fprintf ppf "%s{%a}" p pp_s s


module type MS = sig
  type key
  type +'a t

  val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val cardinal: 'a t -> int
end

module type HS = sig
  type key
  type 'a t

  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val length : 'a t -> int
end


module MapPrinter(M: MS):
sig
  val pp:
    ?boxed:bool -> ?p:string ->
    ?pp_kv_sep:(fmt -> unit -> unit) ->
    ?pp_sep:(fmt -> unit -> unit) ->
    (fmt -> M.key -> unit) ->
    (fmt -> 'a -> unit) ->
    fmt -> 'a M.t -> unit
end =
struct
  let pp ?(boxed = false) ?(p = "") ?(pp_kv_sep = pp_c_spc) ?(pp_sep = pp_sc_spc)
      pp_k pp_v ppf m =
    let pp_m ppf m =
      let card = M.cardinal m in
      let _ =
        M.fold (
          fun k v acc ->
            F.fprintf ppf "%a"
              (pp_doublet ~pp_sep1:pp_kv_sep pp_k pp_v) (k, v);
            if acc < card - 1 then pp_sep ppf ();
            acc + 1
        ) m 0
      in ()
    in
    F.fprintf ppf (if boxed then "@[<hov 2>%s{%a}@]" else "%s{%a}") p pp_m m
end

module HTPrinter(M: HS) :
sig
  val pp:
    ?boxed:bool -> ?p:string ->
    ?pp_kv_sep:(fmt -> unit -> unit) ->
    ?pp_sep:(fmt -> unit -> unit) ->
    (fmt -> M.key -> unit) ->
    (fmt -> 'a -> unit) ->
    fmt -> 'a M.t -> unit
end =
struct
  let pp ?(boxed = false) ?(p = "") ?(pp_kv_sep = pp_c_spc) ?(pp_sep = pp_sc_spc)
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
    F.fprintf ppf (if boxed then "@[<hov 2>%s{%a}@]" else "%s{%a}") p pp_m m
end
