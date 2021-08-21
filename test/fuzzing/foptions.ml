

let u_qvrs = ref false

let u_adts = ref false

let u_li = ref false

let u_ite = ref false

let u_fa = ref false

let u_btv = ref false


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
