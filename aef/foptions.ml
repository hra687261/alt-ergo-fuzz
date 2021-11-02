
type interval = {lb: int; ub: int}

(************************************************************)
(** the upper and lower bounds for the generation options  **)
(************************************************************)

let qmd_i = {lb = 1; ub = 3}
let amd_i = {lb = 1; ub = 3}
let fmd_i = {lb = 1; ub = 3}
let nuv_i = {lb = 1; ub = 3}
let nqv_i = {lb = 1; ub = 3}
let ntd_i = {lb = 1; ub = 2}
let nst_i = {lb = 1; ub = 3}


(************************************************************)

let qmd = ref 3
let amd = ref 3
let fmd = ref 3
let nuv = ref 3
let nqv = ref 1
let ntd = ref 2
let nst = ref 3

(************************************************************)

let u_qvrs = ref true
let u_adts = ref true
let u_li = ref true
let u_ite = ref true
let u_fa = ref false
let u_btv = ref true


(* Getters *)

let get_qmd () = !qmd
let get_amd () = !amd
let get_fmd () = !fmd
let get_nuv () = !nuv
let get_nqv () = !nqv
let get_ntd () = !ntd
let get_nst () = !nst

(************************************************************)

let get_u_qvrs () = !u_qvrs
let get_u_adts () = !u_adts
let get_u_li () = !u_li
let get_u_ite () = !u_ite
let get_u_fa () = !u_fa
let get_u_btv () = !u_btv

(************************************************************)

(* Setters *)

let set_qmd = (:=) qmd
let set_amd = (:=) amd
let set_fmd = (:=) fmd
let set_nuv = (:=) nuv
let set_nqv = (:=) nqv
let set_ntd = (:=) ntd
let set_nst = (:=) nst

(************************************************************)

let set_u_qvrs = (:=) u_qvrs
let set_u_adts = (:=) u_adts
let set_u_li = (:=) u_li
let set_u_ite = (:=) u_ite
let set_u_fa = (:=) u_fa
let set_u_btv = (:=) u_btv

(************************************************************)
