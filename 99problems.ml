(***********************************************************************
 *** 99 Problems in OCaml **********************************************
 *** http://ocaml.org/tutorials/99problems.html ************************
 ***********************************************************************)

(*** WORKING WITH LISTS ************************************************)

(* 1. Write a function that returns the last element of a list.
 * # val last : 'a list -> 'a option
 *)
let rec last xs = match xs with
    [] -> None
  | x :: [] -> Some x
  | x :: xs' -> last xs'
;;

last [`a; `b; `c; `d] = Some `d;;
last [] = None;;

(* 2. Find the last but one (last and penultimate) elements of a list. *)
let rec last_two xs = match xs with
    [] | [_] -> None
  | [x, y] -> Some (x, y)
  | _ :: xs' -> last_two xs'
;;

last_two [`a; `b; `c; `d] = Some (`c, `d);;
last_two [`a] = None;;

(* 3. Find the k'th element of a list *)
let rec at k xs = match (k, xs) with
    (_, []) -> None
  | (1, x::_) -> Some x
  | (n, x::xs') -> at (n-1) xs'
;;

at 3 [`a; `b; `c; `d; `e] = Some `c;;
at 3 [`a] = None;;

(* 4. Find the number of elements of a list. Bonus for a tail
 * recursive solution. *)
let rec length xs = match xs with
    [] -> 0
  | [_] -> 1
  | _ :: xs' -> 1 + length xs'
;;

length [`a; `b; `c] = 3;;
length [] = 0;;

let length xs =
  let rec tail acc = function
  | [] -> acc
  | x :: xs' -> tail (1+acc) xs'
  in tail 0 xs
;;

length [`a; `b; `c] = 3;;
length [] = 0;;

(* 5. Reverse a list. *)
let rev xs =
  let rec tail ys acc = function
      [] -> acc
    | y :: ys' -> tail (y :: acc) ys'
  in tail xs []
;;

rev [`a; `b; `c] = [`c; `b; `a];;


(*** ARITHMETIC ********************************************************)

(* 1. Determine whether a given integer number is prime. *)

(* (almost) brute force solution *)
let is_prime n =
  if n < 2 then false else
    let root = int_of_float (sqrt (float_of_int n)) in
    let rec aux k =
      if k <= root then
	if n mod k = 0 then false else aux (k+1)
      else
	true
    in aux 2
;;

not (is_prime 1);;
is_prime 7;;
not (is_prime 12);;
not (is_prime 25);;

(* sieve of eratosthenes *)
let sieve n =
  if n < 2 then raise (Invalid_argument (string_of_int n)) else
    let range =
      let rec make_list k acc =
	if k > 1 then make_list (k-1) (k :: acc) else acc in
      make_list n [] in
    let rec aux k ys =
      let excess = List.filter (fun x -> x mod k <> 0) ys in
      if k < n && (List.length ys) > 0 then
	k :: aux (List.hd excess) (List.tl excess)
      else
	[k] in
    aux 2 range 
;;

let is_prime n = n = List.hd (List.rev (sieve n));;

not (is_prime 1);;
is_prime 7;;
not (is_prime 12);;
not (is_prime 25);;
