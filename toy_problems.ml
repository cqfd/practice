(* Allison Kaptur's Problems *******************************************)

(* write a function that returns the sum of a list without loops/globals. *)
let rec sum = function
  | [] -> 0
  | x::xs -> x + sum xs
;;

sum [0; 1; 2; 3; 4] = 10;;
sum [-1; 0; 1] = 0;;
sum [] = 0;;

(* write a function that returns the last index of a given input in a list. *)
let rec last_index_of x xs =
  let sx = List.rev xs in
  let rec aux = function
    | [] -> -1
    | y::ys -> if y = x then 1 else 1 + aux ys in
  List.length xs - aux sx
;;

last_index_of 5 [1; 2; 4; 6; 5; 2; 7] = 4;;
last_index_of 5 [1; 2; 4; 6; 2; 7] = 1;;
last_index_of 5 [1; 2; 5; 4; 6; 5; 2; 7] = 5;;

(* implement reduce *)
let rec reduce f a = function
  | [] -> a
  | x::xs -> f x (reduce f a xs)
;;

reduce (+) 0 [0; 1; 2; 3; 4] = 10;;
reduce ( * ) 1 [1; 2; 3; 4] = 24;;

(* implement map in terms of reduce *)
let map f xs = reduce (fun x y -> f x :: y) [] xs;;

map (fun x -> x * x) [1; 2; 3; 4] = [1; 4; 9; 16];;
map (fun x -> x / 2) [1; 2; 3; 4] = [0; 1; 1; 2];;

(* implement filter in terms of reduce *)
let filter f xs = reduce (fun x y -> if f x then x::y else y) [] xs;;

filter (fun x -> x mod 2 = 0) [1; 2; 3; 4] = [2; 4];;
