(** Generating primes *)

open Builtin
open Basic_arithmetics

(* Initializing list of integers for eratosthenes's sieve. Naive
   version.
*)

(** List composed of 2 and then odd integers starting at 3.
    @param n number of elements in the list of integers.
 *)
let init_eratosthenes n =
  let rec initrec e =
    if e = n+1 then []
    else if e=2 then 2::initrec ( e+1)
    else if modulo e 2 <> 0 then e::initrec (e+1)
    else initrec (e+1)
  in initrec 2 ;;

(* Eratosthenes sieve. *)

(** Eratosthene sieve.
    @param n limit of list of primes, starting at 2.
*)
let rec remove_nth n l1 l2 = match l1 with
	        [] -> l2
	    | e1::l when (modulo e1 n) = 0 && e1 <> n -> remove_nth n l l2
	    | e1::l -> e1::remove_nth n l l2;;
let eratosthenes n = if n < 0 then invalid_arg "bruh" else
	    let rec era n i l = 
	        if i*i > n then l
	      else era n (i+1) (remove_nth i l [])
	    in era n 3 (init_eratosthenes n);;


(* Write and read into file functions for lists. *)

(** Write a list into a file. Element seperator is newline.
    @param file path to write to.
 *)
let write_list li file =   let oc = open_out file in
  let rec aux = function
      [] -> close_out oc
    | e::s1 -> Printf.fprintf oc "%d\n" e; aux s1
  in aux li;;

  
(** Write a list of prime numbers up to limit into a txt file.
    @param n limit of prime numbers up to which to build up a list of primes.
    @param file path to write to.
*)
let write_list_primes n file = write_list (eratosthenes n) file;;

(** Read file safely ; catch End_of_file exception.
    @param in_c input channel.
 *)
let input_line_opt in_c =
  try Some (input_line in_c)
  with End_of_file -> None

(** Create a list out of reading a line per line channel.
    @param in_c input channel.
 *)
let rec create_list in_c =
  let rec list = input_line_opt in_c in
  match list with
      None -> []
    | Some list-> int_of_string(list)::create_list in_c;;

(** Load list of primes into OCaml environment.
    @param file path to load from.
 *)
let read_list_primes file = create_list (open_in file);;

(* Auxiliary functions to extract big prime numbers for testing
   purposes.
 *)

(** Get biggest prime.
    @param l list of prime numbers.
 *)
let rec last_element l = match l with
  | [] -> failwith "Builtin.generate_primes.last_element: Your list \
                    is empty. "
  | e::[] -> e
  | h::t -> last_element t

(** Get two biggest primes.
    @param l list of prime numbers.
 *)
let rec last_two l = match l with
  | [] | [_] -> failwith "Builtin.generate_primes.last_two: List has \
                          to have at least two prime numbers."
  | e::g::[] -> (e, g)
  | h::t -> last_two t
;;

(* Generating couples of prime numbers for specific or fun
   purposes.
 *)

(** Finding couples of primes where second entry is twice the first
    plus 1.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let double_primes limit isprime =
  let rec double_rec l = match l with
      [] -> []
    | e1::s1  when  isprime (e1*2+1)&& isprime e1 ->
       (e1,e1*2+1)::double_rec s1
    |e1::s1 -> double_rec s1
  in double_rec (eratosthenes limit);;
(** Finding twin primes.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let twin_primes limit isprime =
  let primes = eratosthenes limit in
  let rec test primes =
    match primes with 
      | [] -> []
      | e :: l -> if e = 2 then test l
	else if  isprime (e+2) then (e,(e+2)):: test l
     else  test l
  in test primes;;
