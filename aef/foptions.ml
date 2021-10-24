
let query_max_depth = ref 3

let axiom_max_depth = ref 3

let func_max_depth = ref 3

let nb_us_vars = ref 5

let nb_q_vars = ref 1

(************************************************************)

let u_qvrs = ref true

let u_adts = ref true

let u_li = ref true

let u_ite = ref true

let u_fa = ref false

let u_btv = ref true


(* Getters *)

let get_query_max_depth () = !query_max_depth

let get_axiom_max_depth () = !axiom_max_depth

let get_func_max_depth () = !func_max_depth

let get_nb_us_vars () = !nb_us_vars

let get_nb_q_vars () = !nb_q_vars

(************************************************************)

let get_u_qvrs () = !u_qvrs

let get_u_adts () = !u_adts

let get_u_li () = !u_li

let get_u_ite () = !u_ite

let get_u_fa () = !u_fa

let get_u_btv () = !u_btv

(* Setters *)

let set_query_max_depth = (:=) query_max_depth

let set_axiom_max_depth = (:=) axiom_max_depth

let set_func_max_depth = (:=) func_max_depth

let set_nb_us_vars = (:=) nb_us_vars

let set_nb_q_vars = (:=) nb_q_vars

(************************************************************)

let set_u_qvrs = (:=) u_qvrs

let set_u_adts = (:=) u_adts

let set_u_li = (:=) u_li

let set_u_ite = (:=) u_ite

let set_u_fa = (:=) u_fa

let set_u_btv = (:=) u_btv
