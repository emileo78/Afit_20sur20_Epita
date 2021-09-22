(** Basic arithmetics with built-in integers *)

open Builtin



  

(* Greater common divisor and smaller common multiple
   implemetations.
*)

(** Greater common (positive) divisor of two non-zero integers.
    @param a non-zero integers
    @param b non-zero integer
*)
let gcd a b =
  let rec gcdd  a b=match b with
    |0->a
    |zoub when 0>zoub ->gcdd b (-(modulo a b))
    |_->gcdd b (modulo a b)
  in gcdd a b
;;



(* Extended Euclidean algorithm. Computing Bezout Coefficients. *)

(** Extended euclidean division of two integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param a non-zero integer
    @param b non-zero integer.
*)
let  bezout a b =
  let rec bezout2 (r,u,v, r',u',v')=
    if r'=0 then (u,v,r)
     else    bezout2 (r',u',v', (r-((quot r r')*r')) , (u-((quot r r')*u')), (v-((quot r r')*v')))
      in  bezout2 (a, 1, 0, b, 0, 1) ;;


bezout 18 22;;
