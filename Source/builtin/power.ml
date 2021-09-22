(** Power function implementations for built-in integers *)

open Builtin
open Basic_arithmetics

(* Naive and fast exponentiation ; already implemented in-class.
 *)

(** Naive power function. Linear complexity
    @param x base
    @param n exponent
 *)
let pow x n =
  let rec pow_rec z nfois =

    if nfois=0 then 1 

    else   z*pow_rec z (nfois-1)
     in  pow_rec x n
;;


 

(** Fast integer exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
 *)
let  rec power x n = 
  if n = 0 then 1
  else if n=1 then x
  else if n mod 2 = 0 then power (x*x) (quot n 2)
  else x * power (x*x) (quot(n-1) 2);;


(* Modular expnonentiation ; modulo a given natural number smaller
   max_int we never have integer-overflows if implemented properly.
 *)

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
    @param m modular base
 *)
let mod_power x n m =
  let rec rec_mod xrec nrec =
 if n <= nrec  then xrec else
			  rec_mod (modulo (xrec*x) m) (nrec+1)
		      in rec_mod 1 0
;;

(* Making use of Fermat Little Theorem for very quick exponentation
   modulo prime number.
 *)

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base
    @param n exponent
    @param p prime modular base
 *)
let prime_mod_power x n p = if x= 0 then 0
  else mod_power x (modulo n (p-1)) p  ;;
