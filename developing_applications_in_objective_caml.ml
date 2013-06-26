(***********************************************************************)
(***********************************************************************)
(************ Developing Applications With Objective Caml **************)
(***********************************************************************)
(***********************************************************************)

(* Example: representing trees *****************************************)

(* binary tree *)
type 'a bin_tree =
| Empty
| Node of 'a bin_tree * 'a * 'a bin_tree
;;

let rec list_of_tree = function
  | Empty -> []
  | Node (l, v, r) -> (list_of_tree l) @ (v :: (list_of_tree r))
;;

let rec insert x = function
  | Empty -> Node (Empty, x, Empty)
  | Node (l, v, r) ->
    if x < v then Node (insert x l, v, r)
    else Node (l, v, insert x r)
;;

let rec tree_of_list = function
  | [] -> Empty
  | h :: t -> insert h (tree_of_list t)
;;

let sort tr = list_of_tree (tree_of_list tr);;

sort [5; 8; 2; 7; 1; 0; 3; 6; 9; 4] = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9];;

(* general planar tree *)
type 'a tree =
| Empty
| Node of 'a * 'a tree list
;;

(* Merging two lists ***************************************************)

(* 1. Write a function merge_i which takes as input two integer lists
 * sorted in increasing order and returns a new sorted list containing the
 * elements of the first two. *)
let rec merge_i xs ys = match (xs,ys) with
  | (xs,[])       -> xs
  | ([],ys)       -> ys
  | (x::xs,y::ys) ->
    if x < y then x :: y :: (merge_i xs ys)
    else y :: x :: (merge_i xs ys)
;;

(* 2. Write a general function merge which takes as argument a comparison
 * function and two lists sorted in this order and returns the list merged
 * in the same order. The comparison function will be of type
 * 'a -> 'a -> bool *)
let rec merge cmp xs ys = match (xs,ys) with
  | (xs,[])       -> xs
  | ([],ys)       -> ys
  | (x::xs,y::ys) ->
    if cmp x y then x :: y :: (merge cmp xs ys)
    else y :: x :: (merge cmp xs ys)
;;

(* 3. Apply this function to two integer lists sorted in decreasing order,
 * then to two string lists sorted in decreasing order. *)
merge (>) [7; 4; 2; 2] [5; 4; 3; 1];;
merge (fun x y -> String.length x > String.length y)
      ["333"; "22"]
      ["4444"; "22"; "1"; ""];;

(* 4. What happens if one of the lists is not in the required
 * decreasing order? *)
(* remarks: the merge happens without any real context, it only knows
 * about the head of each list *)
merge (>) [3; 4; 2; 1] [5; 4; 3; 2];;

(* 5. Write a new list type in the form of a record containing three fields:
 * the conventional list, an order function and a boolean indicating whether
 * the list is in that order. *)
type 'a new_list = { l: 'a list; cmp: 'a -> 'a -> bool; ordered: bool };;

(* 6. Write a function insert which adds an element to a new_list. *)
let insert x xs =
  if xs.ordered then
    let rec aux = function
      | [] -> [x]
      | h :: t -> if xs.cmp x h then x :: h :: t else h :: (aux t)
    in {xs with l=(aux xs.l)}
  else
    {xs with l=x::xs.l}
;;

(* 7. Write a function sort which insertion sorts the elements of a list. *)
let rec sort xs =
  if xs.ordered then
    xs
  else match xs.l with
    [] -> {xs with l=[]; ordered=true}
  | h::t -> insert h (sort {xs with l=t})
;;

let equals x y = if x.l = y.l && x.ordered = y.ordered then true else false;;

equals (sort {l=[1; 3; 4; 2; 3]; cmp=(<); ordered=false})
       {l=[1; 2; 3; 3; 4]; cmp=(<); ordered=true}
;;
equals (sort {l=[]; cmp=(<); ordered=false})
       {l=[]; cmp=(<); ordered=true}
;;


(* 8. Write a function merge for these lists. *)
(* remarks: How do we tell if the lists are using the same cmp function? *)
let merge l1 l2 =
  if not l1.ordered || not l2.ordered then
    raise (Failure "unordered list")
  else
    let rec aux xs ys = match (xs,ys) with
	([],rest) -> rest
      | (rest,[]) -> rest
      | (h1::t1,h2::t2) ->
	if l1.cmp h1 h2 then
	  h1::h2::(aux t1 t2)
	else
	  h2::h1::(aux t1 t2) in
    let sorted = aux l1.l l2.l in
    {l=sorted; cmp=l1.cmp; ordered=true}
;;

let equals x y = if x.l = y.l && x.ordered = y.ordered then true else false;;
equals (merge {l=[1; 4; 8]; cmp=(<); ordered=true}
	      {l=[2; 3; 5]; cmp=(<); ordered=true})
       {l=[1; 2; 3; 4; 5; 8]; cmp=(<); ordered=true}
;;
