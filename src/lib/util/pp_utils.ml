
type fmt = Format.formatter
let f = Format.fprintf 

let indentsize = ref 2
let set_indentsize n = indentsize := n
let get_indentsize () = !indentsize
let indent = String.make !indentsize ' '

let addpref :
  (fmt -> 'a -> unit) -> 
  (?p:string -> fmt -> 'a -> unit) =
  fun pr ?(p = "") fmt v ->
  f fmt "%s%a" p pr v

let addlb :
  (?p:string -> fmt -> 'a -> unit) ->
  (?p:string -> fmt -> 'a -> unit) =
  fun pr ?(p = "") fmt v ->
  f fmt "\n%a" (pr ~p) v

let pr_int : 
  ?p:string -> fmt -> int -> unit =
  fun ?(p = "") fmt n ->
  Format.fprintf fmt "%s%d" p n

let pr_float : 
  ?p:string -> fmt -> float -> unit =
  fun ?(p = "") fmt f -> 
  Format.fprintf fmt "%s%f" p f

let pr_bool : 
  ?p:string -> fmt -> bool -> unit =
  fun ?(p = "") fmt b ->
  Format.fprintf fmt "%s%b" p b


let print_list : 
  (?p:string -> fmt -> 'a -> unit) ->
  (fmt -> 'a list -> unit) =
  fun pr fmt l ->
  (
    f fmt "[";
    List.iter (f fmt " %a;" (pr ~p:"")) l;
    f fmt "]" 
  )

let print_list_lb :
  ?ind:bool ->
  (?p:string -> fmt -> 'a -> unit) ->
  (?p:string -> fmt -> 'a list -> unit) =
  fun ?(ind = false) pr ?(p = "") fmt l ->
  (
    let p1 = p^indent in

    if l == [] 
    then (
      if ind 
      then f fmt "%s[]" p 
      else f fmt "[]" 
    ) else (
      f fmt "\n%s[" p;
      List.iter (f fmt "\n%a;" (pr ~p:p1)) l;
      f fmt "\n%s]" p 
    )
  )


let print_array : 
  (?p:string -> fmt -> 'a -> unit) ->
  (fmt -> 'a array -> unit) =
  fun pr fmt arr -> 
  (
    f fmt "[|";
    Array.iter (f fmt " %a;" (pr ~p:"")) arr;
    f fmt "|]" 
  ) 

let print_array_lb : ?slb:bool ->
  (?p:string -> fmt -> 'a -> unit) ->
  (?p:string -> fmt -> 'a array -> unit) =
  fun ?(slb = false) pr ?(p = "") fmt arr -> 
  (
    let p1 = p^indent in
    if Array.length arr = 0
    then (
      if slb
      then f fmt "%s[||]" p1
      else f fmt "[||]"
    ) 
    else ( 
      f fmt "\n%s[|" p;
      Array.iter (f fmt "\n%a;" (pr ~p:p1)) arr;
      f fmt "\n%s|]" p 
    )
  )


let print_queue :
  (?p:string -> fmt -> 'a -> unit) -> 
  (fmt -> 'a Queue.t -> unit) =
  fun pr fmt q -> 
  (

    f fmt "{|";
    Queue.iter (f fmt " %a;" (pr ~p:"")) q;
    f fmt "|}"
  )

let print_queue_lb :
  (?p:string -> fmt -> 'a -> unit) -> 
  (?p:string -> fmt -> 'a Queue.t -> unit) =
  fun pr ?(p = "") fmt q -> 
  (
    let p1 = p^indent in
    if Queue.length q = 0
    then 
      f fmt "{||}"
    else ( 
      f fmt "\n%s{|" p;
      Queue.iter (f fmt "\n%a;" (pr ~p:p1)) q;
      f fmt "\n%s|}" p 
    )
  )


let print_stack : 
  (?p:string -> fmt -> 'a -> unit) -> 
  (fmt -> 'a Stack.t -> unit) =
  fun pr fmt s -> 
  (
    f fmt "[[";
    Stack.iter (f fmt " %a;" (pr ~p:"")) s;
    f fmt "]]" 
  )

let print_stack_lb : 
  (?p:string -> fmt -> 'a -> unit) -> 
  (?p:string -> fmt -> 'a Stack.t -> unit) =
  fun pr ?(p = "") fmt s -> 
  (
    let p1 = p^indent in

    if Stack.length s = 0
    then f fmt "[[]]"
    else ( 
      f fmt "\n%s[[" p;
      Stack.iter (f fmt "\n%a;" (pr ~p:p1)) s;
      f fmt "\n%s]]" p 
    )
  )


let print_opt : 
  (?p:string -> fmt -> 'a -> unit) -> 
  (fmt -> 'a option -> unit) =
  fun pr fmt a -> 
  (
    match a with 
    | None -> 
      f fmt "None"
    | Some x ->
      f fmt "Some";
      f fmt " %a" (pr ~p:"") x 
  )

let print_opt_lb : ?slb:bool ->
  (?p:string -> fmt -> 'a -> unit) -> 
  (?p:string -> fmt -> 'a option -> unit) =
  fun ?(slb = false) pr ?(p = "") fmt a -> 
  (
    let p1 = p^"  " in 
    match a with 
    | None -> 
      if slb 
      then f fmt "%sNone" p
      else f fmt "None"
    | Some x ->
      if slb 
      then (
        f fmt "%sSome" p;
        f fmt "\n%a" (pr ~p:p1) x
      )
      else (
        f fmt "Some";
        f fmt "\n%a" (pr ~p:p) x
      )
  )


let print_doublet : 
  (?p:string -> fmt -> 'a -> unit) * (?p:string -> fmt -> 'b -> unit) ->
  (fmt -> 'a * 'b -> unit) =
  fun (pr1, pr2) fmt (v1, v2) ->
  (
    f fmt "(";
    f fmt "%a," (pr1 ~p:"") v1;
    f fmt " %a" (pr2 ~p:"") v2;
    f fmt ")"
  )

let print_doublet_lb : 
  (?p:string -> fmt -> 'a -> unit) * (?p:string -> fmt -> 'b -> unit) ->
  (?p:string -> fmt -> 'a * 'b -> unit) =
  fun (pr1, pr2) ?(p = "") fmt (v1, v2) ->
  (
    let p1 = p^indent in

    f fmt "%s(" p;
    f fmt "\n%a," (pr1 ~p:p1) v1;
    f fmt "\n%a" (pr2 ~p:p1) v2;
    f fmt "\n%s)" p
  )


let print_triplet :  
  (?p:string -> fmt -> 'a -> unit) *
  (?p:string -> fmt -> 'b -> unit) *
  (?p:string -> fmt -> 'c -> unit) ->
  (fmt -> 'a * 'b * 'c -> unit) =
  fun (pr1, pr2, pr3) fmt (v1, v2, v3) ->
  (
    f fmt "(";
    f fmt " %a," (pr1 ~p:"") v1;
    f fmt " %a," (pr2 ~p:"") v2;
    f fmt " %a" (pr3 ~p:"") v3;
    f fmt ")"
  )

let print_triplet_lb :  
  (?p:string -> fmt -> 'a -> unit) *
  (?p:string -> fmt -> 'b -> unit) *
  (?p:string -> fmt -> 'c -> unit) ->
  (?p:string -> fmt -> 'a * 'b * 'c -> unit) =
  fun (pr1, pr2, pr3) ?(p = "") fmt (v1, v2, v3) ->
  (
    let p1 = p^indent in

    f fmt "%s(" p;
    f fmt "\n%a," (pr1 ~p:p1) v1;
    f fmt "\n%a," (pr2 ~p:p1) v2;
    f fmt "\n%a" (pr3 ~p:p1) v3;
    f fmt "\n%s)" p
  )

let print_quadruplet : 
  (?p:string -> fmt -> 'a -> unit) *
  (?p:string -> fmt -> 'b -> unit) *
  (?p:string -> fmt -> 'c -> unit) *
  (?p:string -> fmt -> 'd -> unit) ->
  (fmt -> 'a * 'b * 'c * 'd -> unit) =
  fun (pr1, pr2, pr3, pr4) fmt (v1, v2, v3, v4) ->
  (
    f fmt "(";
    f fmt " %a," (pr1 ~p:"") v1;
    f fmt " %a," (pr2 ~p:"") v2;
    f fmt " %a," (pr3 ~p:"") v3;
    f fmt " %a" (pr4 ~p:"") v4;
    f fmt ")"
  )

let print_quadruplet_lb : 
  (?p:string -> fmt -> 'a -> unit) *
  (?p:string -> fmt -> 'b -> unit) *
  (?p:string -> fmt -> 'c -> unit) *
  (?p:string -> fmt -> 'd -> unit) ->
  (?p:string -> fmt -> 'a * 'b * 'c * 'd -> unit) =
  fun (pr1, pr2, pr3, pr4) ?(p = "") fmt (v1, v2, v3, v4) ->
  (
    let p1 = p^indent in

    f fmt "%s(" p;
    f fmt "\n%a," (pr1 ~p:p1) v1;
    f fmt "\n%a," (pr2 ~p:p1) v2;
    f fmt "\n%a," (pr3 ~p:p1) v3;
    f fmt "\n%a" (pr4 ~p:p1) v4;
    f fmt "\n%s)" p
  )


let print_quintuplet : 
  (?p:string -> fmt -> 'a -> unit) *
  (?p:string -> fmt -> 'b -> unit) *
  (?p:string -> fmt -> 'c -> unit) *
  (?p:string -> fmt -> 'd -> unit) *
  (?p:string -> fmt -> 'e -> unit) ->
  (fmt -> 'a * 'b * 'c * 'd * 'e-> unit) =
  fun (pr1, pr2, pr3, pr4, pr5) fmt (v1, v2, v3, v4, v5) ->
  (
    f fmt "(";
    f fmt " %a," (pr1 ~p:"") v1;
    f fmt " %a," (pr2 ~p:"") v2;
    f fmt " %a," (pr3 ~p:"") v3;
    f fmt " %a," (pr4 ~p:"") v4;
    f fmt " %a" (pr5 ~p:"") v5;
    f fmt ")"
  )

let print_quintuplet_lb : 
  (?p:string -> fmt -> 'a -> unit) *
  (?p:string -> fmt -> 'b -> unit) *
  (?p:string -> fmt -> 'c -> unit) *
  (?p:string -> fmt -> 'd -> unit) *
  (?p:string -> fmt -> 'e -> unit) ->
  (?p:string -> fmt -> 'a * 'b * 'c * 'd * 'e -> unit) =
  fun (pr1, pr2, pr3, pr4, pr5) ?(p = "") fmt (v1, v2, v3, v4, v5) ->
  (
    let p1 = p^indent in

    f fmt "%s(" p;
    f fmt "\n%a," (pr1 ~p:p1) v1;
    f fmt "\n%a," (pr2 ~p:p1) v2;
    f fmt "\n%a," (pr3 ~p:p1) v3;
    f fmt "\n%a," (pr4 ~p:p1) v4;
    f fmt "\n%a" (pr5 ~p:p1) v5;
    f fmt "\n%s)" p
  )

let print_set :
  (module Set.S with type elt = 'a and type t = 't) ->
  (?p:string -> fmt -> 'a -> unit) -> 
  (fmt -> 't -> unit) = 
  fun (type a t) 
    (module S: Set.S with type elt = a and type t = t)
    (pr: ?p:string -> fmt -> a -> unit) fmt s -> 
    (
      let f = Format.fprintf in

      f fmt "{";
      S.iter (
        fun e ->
          f fmt " %a;" (pr ~p:"") e
      ) s;
      f fmt "}"
    )

let print_set_lb : ?slb:bool ->
  (module Set.S with type elt = 'a and type t = 't) ->
  (?p:string -> fmt -> 'a -> unit) -> 
  (?p:string -> fmt -> 't -> unit) = 
  fun (type a t) ?(slb = false)
    (module S: Set.S with type elt = a and type t = t)
    (pr: ?p:string -> fmt -> a -> unit) ?(p = "") fmt s -> 
    (
      let f = Format.fprintf in
      let p1 = p^indent in 

      if S.is_empty s 
      then (
        if slb 
        then f fmt "%s{}" p
        else f fmt "{}"
      ) else (
        f fmt "\n%s{" p;
        S.iter (
          fun e ->
            f fmt "\n%a;" (pr ~p:p1) e
        ) s;
        f fmt "\n%s}" p
      )
    )



module type MS = sig
  type key
  type +'a t
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val is_empty : 'a t -> bool
end

module MapPrinter:
  functor (M: MS) ->
  sig
    val pr:
      (?p:string -> fmt -> M.key -> unit) ->
      (?p:string -> fmt -> 'a -> unit) ->
      (fmt -> 'a M.t -> unit)
    val pr_lb:
      ?ind:bool ->
      (?p:string -> fmt -> M.key -> unit) ->
      (?p:string -> fmt -> 'a -> unit) ->
      (?p:string -> fmt -> 'a M.t -> unit)
  end =
  functor (M: MS) ->
  struct 
    let pr : 
      (?p:string -> fmt -> M.key -> unit) ->
      (?p:string -> fmt -> 'a -> unit) ->
      (fmt -> 'a M.t -> unit) =
      fun prk prv fmt m -> 
        (
          let f = Format.fprintf in
          f fmt "{";
          M.iter (
            fun k v -> 
              f fmt " %a;"
                ( print_doublet
                    (prk, prv)
                ) (k, v)
          ) m;
          f fmt "}"
        )

    let pr_lb : 
      ?ind:bool ->
      (?p:string -> fmt -> M.key -> unit) ->
      (?p:string -> fmt -> 'a -> unit) ->
      (?p:string -> fmt -> 'a M.t -> unit) =
      fun ?(ind = false) prk prv ?(p = "") fmt m -> 
      (
        let f = Format.fprintf in
        let p1 = p^indent in

        if M.is_empty m 
        then (
          if ind 
          then f fmt "%s{}" p 
          else f fmt "{}" 
        )
        else (
          f fmt "\n%s{" p;
          M.iter (
            fun k v -> 
              f fmt "\n%a;"
                ( print_doublet_lb ~p:p1
                    (prk, prv)
                ) (k, v)
          ) m;
          f fmt "\n%s}" p
        )
      )
  end


module type HS = sig
  type key
  type 'a t
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val length : 'a t -> int
end

module HTPrinter:
  functor (M: HS) ->
  sig
    val pr:
      (?p:string -> fmt -> M.key -> unit) ->
      (?p:string -> fmt -> 'a -> unit) ->
      (fmt -> 'a M.t -> unit)
    val pr_lb:
      ?ind:bool ->
      (?p:string -> fmt -> M.key -> unit) ->
      (?p:string -> fmt -> 'a -> unit) ->
      (?p:string -> fmt -> 'a M.t -> unit)
  end =
  functor (M: HS) ->
  struct 
    let pr : 
      (?p:string -> fmt -> M.key -> unit) ->
      (?p:string -> fmt -> 'a -> unit) ->
      (fmt -> 'a M.t -> unit) =
      fun prk prv fmt m -> 
        (
          let f = Format.fprintf in

          f fmt "{";
          M.iter (
            fun k v -> 
              f fmt " %a;"
                ( print_doublet
                    (prk, prv)
                ) (k, v)
          ) m;
          f fmt "}"
        )
    let pr_lb :
      ?ind:bool ->
      (?p:string -> fmt -> M.key -> unit) ->
      (?p:string -> fmt -> 'a -> unit) ->
      (?p:string -> fmt -> 'a M.t -> unit) =
      fun ?(ind = false) prk prv ?(p = "") fmt m -> 
      (
        let f = Format.fprintf in
        let p1 = p^indent in
        if M.length m = 0 
        then (
          if ind 
          then f fmt "%s{}" p 
          else f fmt "{}" 
        ) else (
          f fmt "\n%s{" p;
          M.iter (
            fun k v -> 
              f fmt "\n%a;"
                ( print_doublet_lb ~p:p1
                    (prk, prv)
                ) (k, v)
          ) m;
          f fmt "\n%s}" p
        )
      )
  end
