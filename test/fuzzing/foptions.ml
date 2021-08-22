

let u_qvrs = ref true

let u_adts = ref true

let u_li = ref true

let u_ite = ref true

let u_fa = ref false

let u_btv = ref true


(* Getters *)

let get_u_qvrs () = !u_qvrs

let get_u_adts () = !u_adts

let get_u_li () = !u_li

let get_u_ite () = !u_ite

let get_u_fa () = !u_fa

let get_u_btv () = !u_btv

(* Setters *)

let set_u_qvrs = (:=) u_qvrs

let set_u_adts = (:=) u_adts

let set_u_li = (:=) u_li

let set_u_ite = (:=) u_ite

let set_u_fa = (:=) u_fa

let set_u_btv = (:=) u_btv
