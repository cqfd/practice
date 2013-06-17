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
  let rec tail ys acc = match ys with
      [] -> acc
    | y :: ys' -> tail ys' (y :: acc)
  in tail xs []
;;

rev [`a; `b; `c] = [`c; `b; `a];;

(* 6. Find out whether a list is a palindrome. A palindrome is its
 * own reverse *)
let rec is_palindrome xs =
  if xs = rev xs then true else false
;;

is_palindrome [`x; `a; `m; `a; `x];;
not (is_palindrome [`a; `b]);;

(* 7. Flatten a nested structure. *)

(* There is no nested list type in OCaml, so we need to define one first. A
 * node of a nested list is either an element, or a list of nodes. *)
type 'a node =
| One of 'a
| Many of 'a node list
;;

let rec flatten = function
  | [] -> []
  | One x :: xs -> x :: flatten xs
  | Many xs :: ys -> flatten xs @ flatten ys
;;

flatten [One `a; Many [One `b; Many [One `c; One `d]; One `e]];;

(* 8. Eliminate consecutive duplicates of list elements. *)
let rec compress = function
  | [] -> []
  | [x] -> [x]
  | x :: xs -> if x = List.hd xs then compress xs else x :: (compress xs)
;;

compress [`a;`a;`a;`a;`b;`c;`c;`a;`a;`d;`e;`e;`e;`e] = [`a;`b;`c;`a;`d;`e];;

(* 19. Rotate a list N places to the left. *)
let rotate xs n =
  let rec aux left right = function
    | 0 -> right @ (List.rev left)
    | m -> aux (List.hd right :: left) (List.tl right) (m-1) in
  if n < 0 then List.rev (aux [] (List.rev xs) (-n)) else aux [] xs n
;;

rotate [`a;`b;`c;`d;`e;`f;`g;`h] 3 = [`d;`e;`f;`g;`h;`a;`b;`c];;
rotate [`a;`b;`c;`d;`e;`f;`g;`h] (-2) = [`g;`h;`a;`b;`c;`d;`e;`f];;

(* 20. Remove the n'th element from a list. *)
let rec remove_at n xs = match n with
    0 -> List.tl xs
  | m -> List.hd xs :: remove_at (m-1) (List.tl xs)
;;

remove_at 1 [`a; `b; `c; `d] = [`a; `c; `d];;

(* 21. Insert an element at a given position into a list. Start counting
 * list elements with 0. *)
let rec insert_at x pos xs = match pos with
    0 -> x :: xs
  | n -> List.hd xs :: insert_at x (n-1) (List.tl xs)
;;

insert_at `alfa 1 [`a; `b; `c; `d] = [`a; `alfa; `b; `c; `d];;

(* 22. Create a list containing all integers within a given range. If the first
 * argument is smaller than the second, produce a list in decreasing order. *)
let rec range m n =
  if m = n then
    [n]
  else
    if m < n then
      m :: range (m+1) n
    else
      m :: range (m-1) n
;;

range 4 9 = [4; 5; 6; 7; 8; 9];;
range 9 4 = [9; 8; 7; 6; 5; 4];;


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

(* 2. Determine the greatest common divisor of two positive integer numbers.
 * Use Euclid's algorithm. *)

let rec gcd a b = if b = 0 then a else gcd b (a mod b);;

gcd 13 27 = 1;;
gcd 20536 7826 = 2;;

(* 3. Determine whether two positive integer numbers are coprime. Two
 * numbers are coprime if their greatest common divisor equals 1. *)
let coprime a b = gcd a b = 1;;

coprime 13 27;;
not (coprime 20536 7826);;

(* 4. Calculate Euler's totient function phi(m). Euler's so-called totient
 * function phi(m) is defined as the number of positive integers
 * r (1 <= r < m) that are coprime to m. We let phi(1) = 1.
 *
 * Find out what the value of phi(m) is if m is a prime number. Euler's
 * totient function plays an important role in one of the most widely
 * used public key crypotography methods (RSA). In this exercise you should
 * use the most primitive method to calculate this function (there are
 * smarter ways that we shall discuss later). *)
let phi m =
  List.length (List.filter (fun n -> gcd n m = 1) (range 1 m));;

phi 10 = 4;;
phi 13 = 12;;

(* 5. Determine the prime factors of a given positive integer. Construct a
 * flat list containing the prime factors in ascending order. *)
let factors n =
  let rec aux k m =
    if k = n then
      []
    else
      if m mod k = 0 then
	k :: aux k (m / k)
      else
	aux (k+1) m in
  aux 2 n
;;

factors 315 = [3; 3; 5; 7];;
