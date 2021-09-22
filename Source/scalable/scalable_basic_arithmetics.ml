(** Basic arithmetics for ordered euclidian ring. *)

open Scalable

(** Greater common (positive) divisor of two non-zero integers.
    @param bA non-zero bitarray.
    @param bB non-zero bitarray.
*)
let  gcd_b a b =
 if compare_b b []=0 &&compare_b a []=0 then []
else if compare_b b []=0 then [] else
  let rec gcdd a b=match b with
   |[]->a
   |[1]->a
  |_-> gcdd b (mod_b a b)
  in gcdd (abs_b a)(abs_b b);;
gcd_b (from_int (28)) (from_int 10);;
 

(** Extended euclidean division of two integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param bA non-zero bitarray.
    @param bB non-zero bitarray.
*)


(*let  bezout_b bA bB =
  let rec bezout2 (r,u,v, r',u',v')=
    if compare_b r'[]=0 then (u,v,r)
    else  bezout2 (r',u',v', (diff_b r(mult_b(quot_b r r')r'))
      , (diff_b u(mult_b(quot_b r r')u')), (diff_b v(mult_b(quot_b r r')v')))
  in  bezout2 (bA, [0;1], [], bB, [], [0;1]) ;;*)
(*let bezout_b bA bB =
       let rec bezout_rec (c,d,e,c',d',e') = match e' with
        []|[0;0]-> (c,d,e)
      | _ -> let f = quot_b e e'
             in bezout_rec(c',d',e',(diff_b  c(mult_b c' f)),
		 diff_b d(mult_b d' f) ,diff_b e (mult_b e' f))
  in bezout_rec([0;1],[],bA,[],[0;1],bB);;*)
let bezout_b bA bB=
  if mult_b bA bB = [] then
    ([0;1],[0;1],add_b bA bB) else
    let rec bezelou bC bD (u1,v1) (u2,v2)=
      let (q,r)=div_b bC bD 
      in
      if compare_b r []=0 then
	if compare_b bA bB=1 then 
	(u2,v2,bD)
	else (v2,u2,bD)else
	  
   bezelou bD r (u2,v2) (diff_b u1 (mult_b q u2), diff_b v1 (mult_b q v2))
    in if compare_b bA bB=1 then bezelou bA bB ([0;1],[0;0])([0;0],[0;1])
      else bezelou bB bA([0;1],[0;0])([0;0],[0;1]);;





	     




