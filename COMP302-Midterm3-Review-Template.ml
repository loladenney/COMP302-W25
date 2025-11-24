open List
exception TODO

(* identifier for variables*)
type ident = string

(* this is a subset of MiniCaml. Lets extend it with Delay *)


(* Parsing *)

(*
Let's try to parse these examples: 
1. fun (x : Susp Int) -> let delay y = x in y

2. fun (x : int => int) -> fun (y: int) -> 5 

3. let delay x = delay (fun (y:int) -> y) in x
*)


type tp = 
| Int
| Arrow of tp * tp
| Susp of tp

type exp =
| ConstI of int
| Var of ident                      (* x *)
| Fn of ident * tp * exp            (* fn x : t -> e  *)


(*Type Inference*)
exception TypeError of string
type ctx = (ident * tp) list

let rec infer (c : ctx) (e: exp) : tp = 
  match e with
  | ConstI i -> Int
  | Var x -> (match List.assoc_opt x c with
    | None -> raise (TypeError "unbound variable")
    | Some t -> t)
  | Fn (x, t, e) -> Arrow( t, infer ((x, t):: c) e)





(*
(* test cases *)
let testinfer1 = infer [] (Fn ( "x", Susp Int , LetDelay ("y", Var ("x"), Var ("y"))))
(*  want: susp int => int *)

let testinfer2 = infer [] (Fn ("x" , Arrow (Int, Int), Fn ("y", Int, ConstI 5)))
(*  want: int => int => int => int *)

let testinfer3 = infer [] (LetDelay ("x", Delay (Fn ("y", Int, Var "y")), Var "x"))
(*  want: int => int *)
*)



(* Evaluation *)

(*
   e1 || delay e1'   [e1'/x]e2 || v
   --------------------------------     ---------------------
   let delay x = e1 in e2 || v           delay e || delay e

*)

(* outputs the result of [d/z]e *)
let rec subst ((d, z) : exp * ident) (e : exp) : exp = failwith "imagine this is implemented"

exception EvaluationStuck

let rec eval (e : exp): exp =
  match e with 
  | Var _ -> raise EvaluationStuck
  | ConstI i -> e
  | Fn (_,_,_) -> e         (* functions are values *)





