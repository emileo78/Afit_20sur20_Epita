(** A naive implementation of big integers

This module aims at creating a set of big integers naively. Such data
types will be subsequently called bitarrays. A bitarray is a list of
zeros and ones ; first integer representing the sign bit. In this
context zero is reprensented by the empty list []. The list is to
be read from left to right ; this is the opposite convention to the
one you usually write binary decompositions with. After the sign bit
the first encountered bit is the coefficient in front of two to
the power zero. This convention has been chosen to ease writing
down code. A natural bitarray is understood as being a bitarray of
which you've taken out the sign bit, it is just the binary
decomposition of a non-negative integer.

 *)

(** Creates a bitarray from a built-in integer.
    @param x built-in integer.
*)
let tail l= match l with
  |[]->[]
  |e::l->l;;

let shifty_shafts l=0::l;;
let reverse list =
  let rec rev accu = function
      []   -> accu
    | e::l -> rev (e::accu) l
  in
  rev [] list ;;


let clear list= let rec clearical l=match l with
   []->[]
  |e::l->if e=0 then clearical l else e::l
		in reverse (clearical(reverse list));;

let rec shift_n l d = match d with
  |0->l
  |d->shift_n (shifty_shafts l)(d-1);;



let from_int x = if x=0 then [] else
  let rec fromage x= match x with
    |0->[]
    |x-> (x mod 2)::(fromage (x/2))
  in if x>0 then 0::(fromage x) else 1::(fromage (-x));;


from_int (0);;
(** Transforms bitarray of built-in size to built-in integer.
    UNSAFE: possible integer overflow.
    @param bA bitarray object.
*)
let  rec power x n = 
  if n = 0 then 1
  else if n=1 then x
  else if n mod 2 = 0 then power (x*x)  (n/ 2)
  else x * power (x*x) ((n-1)/ 2);;
power 5 2;;


let to_int bA = let i= match bA with
  |[]->1
  |e::l->if e =0 then 1 else -1
in let rec to_int_rec bA i2= match bA with
		  |[]-> 0
		  |e::l->(to_int_rec l (i2+1))+(power 2 i2)*e
   in let i3= i*(to_int_rec bA 0)in i3/2;;
to_int [0;0;1];;


(** Prints bitarray as binary number on standard output.
    @param bA a bitarray.
  *)
let print_b bA = let rec print bA =
		   match bA with
		     |[]->()
		     |e::l->print l;
		       print_int e;
		 in print bA;;
print_b [0;1;1;1];;



(** Toplevel directive to use print_b as bitarray printer.
    CAREFUL: print_b is then list int printer.
    UNCOMMENT FOR TOPLEVEL USE.
*)
(* #install_printer print_b *)

(** Internal comparisons on bitarrays and naturals. Naturals in this
    context are understood as bitarrays missing a bit sign and thus
    assumed to be non-negative.
*)

(** Comparing naturals. Output is 1 if first argument is bigger than
    second -1 if it is smaller and 0 in case of equality.
    @param nA A natural, a bitarray having no sign bit.
           Assumed non-negative.
    @param nB A natural.
*)
let rec length l=match l with
  []->0
  |e::l->1+length l;;
let rec compare_n nA nB=
  if length nA > length nB then 1
  else if length nB>length nA then -1 else
  let rec compote nA nB acc =match (nA,nB) with
    |([],[])-> acc
    |(e::l,[])->1
    |([],e::l)->(-1)
    |(e::l,e1::l1)-> if e1>e then compote l l1 (-1)
      else if e1<e then compote l l1 1
      else compote l l1 acc
  in compote nA nB 0;;
compare_n (from_int 49)( from_int 50);;



(** Bigger inorder comparison operator on naturals. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>!) nA nB = if compare_n nA nB =1 then true else false;;

(** Smaller inorder comparison operator on naturals. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural
    @param nB natural.
 *)
let (<<!) nA nB = if compare_n nA nB=(-1) then true else false;;

(** Bigger or equal inorder comparison operator on naturals. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>=!) nA nB = compare_n nA nB =1||compare_n nA nB=0 ;;

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<=!) nA nB = if compare_n nA nB =1||compare_n nA nB=0 then false else true;;

(** Comparing two bitarrays. Output is 1 if first argument is bigger
    than second -1 if it smaller and 0 in case of equality.
    @param bA A bitarray.
    @param bB A bitarray.
*)
let compare_b bA bB = match (bA,bB)with
    ([],[])->0
  |(e::l,[])-> if e=1 then -1 else 1
  |([],e::l)-> if e=0 then -1 else 1
  |(e::l),(e1::l1)->if e=e1 then
      match e with
      |0-> compare_n l l1
      |_->(compare_n l l1)*(-1)
    else if e<e1 then 1
    else -1;;
compare_b [1;1] [0;0;0];;

(** Bigger inorder comparison operator on bitarrays. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>) bA bB = match compare_b bA bB with
  |(1)-> true
  |_-> false;;

(** Smaller inorder comparison operator on bitarrays. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<) bA bB = match compare_b bA bB with
  |(-1)-> true 
  |_-> false;;

(** Bigger or equal inorder comparison operator on bitarrays. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>=) bA bB = match compare_b bA bB with
  |1|0-> true
  |_-> false;;

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<=) bA bB = match compare_b bA bB with
  |(1)-> false
  |_->true;;

(** Sign of a bitarray.
    @param bA Bitarray.
*)
let sign_b bA = match bA with
  |[]->1
  |e::l->if e=0 then 1 else -1;;

(** Absolute value of bitarray.
    @param bA Bitarray.
*)
let abs_b bA = match bA with
  | []->[]
  |e::l-> 0::l;;

(** Quotient of integers smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _quot_t a = if a>4 then invalid_arg "error"
  else a/2;;

(** Modulo of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _mod_t a = if a>4 then invalid_arg "error"
  else a mod 2;;
(** Division of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _div_t a = (_quot_t a,_mod_t a);;

(** Addition of two naturals.
    @param nA Natural.
    @param nB Natural.
*)
let add_n nA nB =
  let rec addimalou nA nB acc= match (nA,nB) with
    |([],[])->if acc=1 then [1] else [] 
    |([],e::l)->if e+acc>1 then 0::(addimalou [] l 1) else (acc+e)::l
    |(e::l,[])->if e+acc>1 then 0::(addimalou l [] 1) else (acc+e)::l
    |(e::l,e1::l1) when e+e1+acc<=1 ->e+e1+acc::(addimalou l l1 0) 
    |(e::l,e1::l1) when e+e1+acc>1 -> if e1+e+acc=2 then 0::(addimalou l l1 1)
      else 1::(addimalou l l1 1)
    |(_,_)->[]
  in addimalou nA nB 0;;

to_int (add_n (from_int (200))(from_int 300));;
from_int 532;;
from_int 500;;



(** Difference of two naturals.
    UNSAFE: First entry is assumed to be bigger than second.
    @param nA Natural.
    @param nB Natural.
*)
let diff_n nA nB =
	  let rec diffental nA nB c = match (nA,nB) with
	     ([],[]) -> []
	    | ([],e2::s2) -> invalid_arg "nB cannot be greater than nA"
	    | (e::s,[]) when  e-c >=0 -> (e-c)::diffental s nB  0
	    | (e::s,[])when c=1-> (2-c)::diffental s nB  1
	    | (e::s,[])-> 1::diffental s nB  0
 	    | (e::s,e2::s2) when e-e2-c=(-2)->0::diffental s s2  1
	    | (e::s,e2::s2) when e-e2-c=(-1)->1::diffental s s2  1
	    | (e::s,e2::s2) when e-e2-c=(0)->0::diffental s s2  0
	    | (e::s,e2::s2) when e-e2-c=(1)->1::diffental s s2  0
	    |(_,_)->[]
	 
	  in clear(diffental nA nB 0);;

diff_n [0;1;0;1] [0;0;1;1];;
clear [0;1]


(** Addition of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
 *)
let add_b bA bB = match (bA,bB) with
    |(bA,[])->bA
    |([],bB)->bB
    |(e::l,e1::l1) when sign_b bA=(-1) && sign_b bB =(-1) -> 1::add_n l l1
    |(e::l,e1::l1) when sign_b bA=  1  && sign_b bB =  1  -> 0::add_n l l1
    |(e::l,e1::l1) when sign_b bA=(-1) && sign_b bB=1 && compare_n l l1 = 1 ->  1::diff_n l l1
    |(e::l,e1::l1) when sign_b bA=(-1) && sign_b bB=1 && compare_n l l1 =(-1)->0::diff_n l1 l
    |(e::l,e1::l1) when compare_n l l1=1 ->0::diff_n l l1
    |(e::l,e1::l1) when compare_n l l1=(-1)->1::diff_n  l1 l
    |(e::l,e1::l1)->[];;


to_int (add_b (from_int (200)) (from_int (300)));;

(** Difference of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)
let diff_b bA bB = match (bA,bB) with
  |([],[])->[]
  |(e::l,[])->clear(e::l)
  |([],e::l)->[]
  |(e::l,e1::l1)->if e=0 && e1=0 then if l>=!l1 then clear(0::diff_n l l1)
    else clear (1::diff_n l1 l)
    else if e=0 && e1=1 then clear(0::add_n l l1)
    else if e=1 && e1=0 then clear(1::add_n l l1) 
    else if l>=!l1 then clear(1::diff_n l l1)
    else clear(0::diff_n l1 l);;
diff_b [0;1;0;1][0;0;0;1;1];;
to_int(diff_b (from_int (5))(from_int (700)));;

add_n [1;1;1;1] [1;1;1;1];;


(** Shifts bitarray to the left by a given natural number.
    @param bA Bitarray.
    @param d Non-negative integer.
*)
let rec shift bA d = match (bA,d) with
  |(l,0)->l
  |([],d)->[]
  |(e::l,d)->shift (e::0::l) (d-1);;


(** Multiplication of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)
let mult_b bA bB = let rec multiprix bA bB d1=match (bA,bB) with
  |([],[])->[]
  |(e::l,[])|([],e::l)->[]
  |(l,e1::l1) -> if e1=0 then multiprix l l1 (d1+1)
		  else  add_b (shift l d1) (multiprix l l1 (d1+1))
		   in match (bA,bB) with 
		     |(e::l,[])|([],e::l)->[]
		     |(e::l,e1::karimbenzema) when sign_b bA = sign_b bB -> multiprix (0::l) (karimbenzema) 0
		     |(e::l,e1::lizarazu) -> multiprix (1::l) (lizarazu) 0
                     |([],[])->[];;
to_int (mult_b (from_int (22)) (from_int (22)));;

(** Quotient of two bitarrays.
    @param bA Bitarray you want to divide by second argument.
    @param bB Bitarray you divide by. Non-zero!
*)
let mult_n bA bB = let rec multiprix bA bB d1=match (bA,bB) with
  |([],[])->[]
  |(e::l,[])|([],e::l)->[]
  |(l,e1::l1) -> if e1=0 then multiprix l l1 (d1+1)
    else  add_n (shift_n l d1) (multiprix l l1 (d1+1))
		   in multiprix bA bB 0;;

let list_bin_mult n d=
  let rec list l1 l2 n d= match (l1,l2) with
  |(_::l1,e2::l2) when compare_n e2 n =1 -> (tail (l1),(l2))
    |(e1::l1,l2) ->list ((shifty_shafts e1)::e1::l1) ((mult_n e1 d)::l2) n d
    |(_,_)->([],[])
  in list [[1]] [] n d;;


let quot_b bA bB= let quot_b2 bA bB=
 
  let (q2,t2)=list_bin_mult (tail bA) (tail bB) in
  let rec quotentin (l1,l2) q t= match (l1,l2) with
    |([],_)|(_,[])-> q
    |(e::l,e1::l1)->if (add_n t e1) >>! (tail bA) then quotentin (l,l1) q t
    else quotentin (l,l1)(add_n e q)(add_n e1 t)
  in match (bA,bB) with
    |(e::l,e1::l1)when e=1 -> if e=e1 then 0::(add_n(quotentin(q2,t2)[][])[1])
      else 1::(add_n (quotentin (q2,t2)[][])[1])
    |(_,[])|([],_)->[]
    |(e::l,e1::l1)when (e=e1 && (e=0 ||e=1)) -> 0::(quotentin (q2,t2)[] [])
    |(e::l,e1::l1)-> 1::(quotentin (q2,t2) [] [])
		  in clear( quot_b2 bA bB);;
    
    
      
(** Modulo of a bitarray against a positive one.
    @param bA Bitarray the modulo of which you're computing.
    @param bB Bitarray which is modular base.
 *)

let mod_b bA bB =
  if (mult_b bB (quot_b bA bB)) =bA then []  else
  (diff_b bA (mult_b (quot_b bA bB)bB));;
   
mod_b (from_int (5)) (from_int 2);;
quot_b (from_int (10)) (from_int 8);;
mult_b (from_int 2) (quot_b (from_int 4)(from_int 2));;
 mult_b (from_int 1)(from_int 8) ;;
diff_b (from_int 10) (from_int 8);;

(** Integer division of two bitarrays.
    @param bA Bitarray you want to divide.
    @param bB Bitarray you wnat to divide by.
*)
let div_b bA bB = ((quot_b bA bB),(mod_b bA bB))
