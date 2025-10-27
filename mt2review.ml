(* ------------backtracking----------- *)

(* rewrite/translate the change making problem from lecture 9 in two ways
    1. using CPS 
    2. using options *)
exception Change
let rec change coins amount = match coins with
  | _ when amount = 0 -> []
  | [] -> raise Change
  | c :: cs ->
      if c > amount
      then change cs amount
      else
        try c :: change (c :: cs) (amount - c) with
        | Change -> change cs amount


(* CPS from class *)
let rec change_cps coins amount (return : int list -> 'r) (fail : unit -> 'r) : 'r =
    failwith "TODO"
           
(* follow up: how do we call this?  *)

(* translated to options *)
let rec change_opt coins amount: 'r =
    failwith "TODO"











(* ------------partial evaluation - simulating macros with HOFs ----------- *)
(* the following are partial eval examples from class*)

(* this is a standard power function *)
let rec pow k n =  
    if k = 0 then 1
    else pow (k-1) n * n

let rec pow (k : int) : int -> int =
    failwith "Implement a partial evaulation version of pow"

(* Exercise from class:
 Let's implement`map_gen l` s.t. 
 it outputs a non-recursive function `f` where
 `f g` applies `g` to each element of `l` obtaining a new list with the results *)

 let rec map_gen (l : 'a list) : ('a -> 'b) -> 'b list =
    failwith "TODO"

    
(* Follow-up exercises from class:
   implement map for trees
   and filter for lists *)
    
type 'a tree = Empty | Node of 'a tree * 'a * 'a tree

let rec tree_map_gen (t : 'a tree) : ('a -> 'b) -> 'b tree = 
  failwith "TODO"


let rec list_filter_gen (l : 'a list) : ('a -> bool) -> 'a list = 
  failwith "TODO"



(* Implement a filter generator for trees!  
   will generalize our output to a list because 
   we dont have a good way of deleting nodes from a tree :) *)

let rec tree_filter_gen (t: 'a tree) : ('a -> bool) -> 'a list =
    failwith "TODO, if there is time" 
        

(* recall: 
let rec filter l f : 'a list = 
    match l with 
    | [] -> []
    | x::xs -> if f x then x :: (filter xs f) else (filter xs f)
*)
;;










(* ------------lazy programming: stateful generators----------- *)
exception StopIteration
type 'a gen = { next : unit -> 'a } 

let filter  ( p : 'a -> bool) (g: 'a gen) : 'a gen =
  { next = failwith "TODO" }



(* recall: 
let rec filter l f : 'a list = 
  match l with 
  | [] -> []
  | x::xs -> if f x then x :: (filter xs f) else (filter xs f)
;;
*)


(* a generator to run it on, from notes *)
(*
let range (n : int) : int gen =
  let c = ref 0 in
  { next = fun () ->
    let i = !c in
    if i = n
    then raise StopIteration
    else (c := i+1; i) } 
*)
(* and a predicate to select the evens
fun x -> x mod 2 = 0
 *)
       



(* Conceptual follow-up:
  Q: Suppose we call `let g' = filter p g` such that `g` is an infinite
  sequence and `p` is false for ALL items of `g`. When happens when we
  call `g'.next ()` ?

  
  
  Q: What if g was finite?
  

*)




(* ------------proofs by induction----------- *)
(*
Proof by induction

type 'a tree = Empty | Node of 'a tree * int * 'a tree

let rec sum t = match t with
    | Empty -> 0
    | Node (l, x, r) -> sum l + x + sum r

let rec sum' t acc = match t with
    | Empty -> acc
    | Node (l, x, r) -> sum' l (sum' r (acc + x))
```


THEOREM 1. (sum and sum' are equivalent.)
for all t:int list,  sum t = sum' t 0

we need to generalize this. 

THEOREM 2. (generalization)
For all acc:int, t: int tree, acc + sum t = sum’ t acc

PROOF
CASE. ?
    
  
    

THEOREM1. (sum and sum' are equivalent.)
for all t:int list,  sum t = sum' t 0

PROOF.
    0 + sum t = sum' t 0                by immediately by THEOREM2 with acc = 0
    sum t = sum' t 0                    by + identity (for all n: Nat, 0 + n = n ) 


*)