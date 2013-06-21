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

(* 9. Pack consecutive duplicates of list elements into sublists. *)
let rec pack xs =
  let rec split acc = function
    | y :: ys when y = List.hd acc -> split (y :: acc) ys
    | ys -> acc, ys in match xs with
      [] -> []
      | h :: t -> let a, b = split [h] t in
		  a :: (pack b)
;;

pack [`a; `a; `a; `a; `b; `c; `c; `a; `a; `d; `d; `e; `e; `e; `e] =
  [[`a; `a; `a; `a]; [`b]; [`c; `c]; [`a; `a]; [`d; `d]; [`e; `e; `e; `e]];;

(* 10. Run-length encoding of a list *)
let encode xs =
  let xs' = pack xs in
  List.map (fun ys -> (List.length ys, List.hd ys)) xs'
;;

encode [`a; `a; `a; `a; `b; `c; `c; `a; `a; `d; `d; `e; `e; `e; `e] =
  [4,`a; 1,`b; 2,`c; 2,`a; 2,`d; 4,`e];;

(* 11. Modified run-length encoding -- Modify the result of the previous
 * problem in such a way that if an element has no duplicates it is simply
 * copied into the result list. Only elements with duplicates are
 * transferred as (N E) lists.
 *
 * Since OCaml lists are homogeneous, one needs to define a type to hold
 * both single elements and sub-lists. *)
type 'a rle =
| One of 'a
| Many of (int * 'a)
;;

let mencode xs =
  let aux (n, e) = if n = 1 then One e else Many (n, e) in
  List.map aux (encode xs)
;;

mencode [`a;`a;`a;`a;`b;`c;`c;`a;`a;`d;`e;`e;`e;`e] =
  [Many (4,`a) ; One `b ; Many (2,`c) ; Many (2,`a) ; One `d ; Many (4,`e)];;

(* 12. Decode a run-length encoded list. Given a run-length code list
 * generated as specified in the previous problem, construct its
 * uncompressed version. *)
let decode xs =
  let rec aux = function
    | One e -> [e]
    | Many (n, e) -> e :: aux (if n = 2 then One e else Many (n-1, e)) in
  List.flatten (List.map aux xs)
;;

decode [Many (4,`a); One `b; Many (2,`c); Many (2,`a); One `d; Many (4,`e)]
  = [`a;`a;`a;`a;`b;`c;`c;`a;`a;`d;`e;`e;`e;`e];;

(* 13. Run-length encoding of a list (direct solution). *)
let rec encode xs =
  let f n e = if n = 1 then One e else Many (n,e) in
  let rec aux n e = function
    | [] -> [f n e]
    | y :: ys ->
      if y = e then
	aux (n+1) e ys
      else
	f n e :: aux 1 y ys in
  aux 1 (List.hd xs) (List.tl xs)
;;

encode [`a;`a;`a;`a;`b;`c;`c;`a;`a;`d;`e;`e;`e;`e] =
  [Many (4,`a); One `b; Many (2,`c); Many (2,`a); One `d; Many (4,`e)];;

(* 14. Duplicate the elements of a list. *)
let rec duplicate = function
  | [] -> []
  | x :: xs -> x :: x :: duplicate xs
;;

duplicate [`a;`b;`c;`c;`d] = [`a;`a;`b;`b;`c;`c;`c;`c;`d;`d];;

(* 15. Replicate the elements of a list a given number of times. *)
let replicate xs n =
  let rec repel e = function
    | 0 -> []
    | m -> e :: repel e (m-1) in
  let rec repal n = function
      [] -> []
    | h :: t -> repel h n :: repal n t in
  List.flatten (repal n xs)
;;

(* 16. Drop every n'th element from a list. *)
let drop xs n =
  let rec aux m = function
    | [] -> []
    | y :: ys -> if m = 1 then aux n ys else y :: aux (m-1) ys in
  aux n xs
;;

drop [`a;`b;`c;`d;`e;`f;`g;`h;`i;`j] 3 = [`a;`b;`d;`e;`g;`h;`j];;

(* 17. Split a list into two pars; the length of the first part is given.
 * If the length of the first part is longer than the entire list, then
 * the first part is the list and the second part is empty. *)
let split xs n =
  let rec aux acc rest m = match rest with
      _ when rest = [] || m = 0 -> (List.rev acc, rest)
    | y :: ys -> aux (y :: acc) ys (m-1) in
  aux [] xs n
;;

split [`a;`b;`c;`d;`e;`f;`g;`h;`i;`j] 3
  = ([`a;`b;`c] , [`d;`e;`f;`g;`h;`i;`j]);;

split [`a;`b;`c;`d] 5 = ([`a; `b; `c; `d], []);;

(* 18. Extract a slice from a list. Given two indices, i and k, the slice
 * is the list containing the elements between the i'th and k'th element
 * of the original list (both limits included). Start counting the elements
 * with 0 (this is the way the List module numbers elements). *)
let rec slice xs min max =
  if min > 0 then slice (List.tl xs) (min-1) (max-1)
  else if max > 0 then (List.hd xs) :: slice (List.tl xs) 0 (max-1)
  else [List.hd xs]
;;

slice [`a;`b;`c;`d;`e;`f;`g;`h;`i;`j] 2 6 = [`c;`d;`e;`f;`g];;


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

(* 22. Create a list containing all integers within a given range. If the
 * first argument is smaller than the second, produce a list in
 * decreasing order. *)
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


(*** LOGIC AND CODES****************************************************)

type bool_expr =
| Var of string
| Not of bool_expr
| And of bool_expr * bool_expr
| Or of bool_expr * bool_expr
;;

(* 1. Truth tables for logical expressions. Define a function, table2 which
 * returns the truth table of a given logical expression in two variables
 * (specified as arguments). The return value must be a list of triples
 * containing (a_val, b_val, expr_val). *)
let table2 a b expr =
  let rec aux va vb = function
    | Var x -> if x = a then va else if x = b then vb else failwith "BAM!"
    | Not x -> not (aux va vb x)
    | And (x,y) -> aux va vb x && aux va vb y
    | Or (x,y) -> aux va vb x || aux va vb y in
  [(true, true, aux true true expr);
   (true, false, aux true false expr);
   (false, true, aux false true expr);
   (false, false, aux false false expr)]
;;

table2 "a" "b" (And(Var "a", Or(Var "a", Var "b"))) =
  [(true, true, true);
   (true, false, true);
   (false, true, false);
   (false, false, false)]
;;
