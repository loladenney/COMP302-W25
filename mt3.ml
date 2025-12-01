open List
exception TODO

(* identifier for variables*)
type ident = string

(* this is a subset of MiniCaml. Lets extend it with Delay *)
type tp = 
| Int
| Arrow of tp * tp
| Susp of tp

type exp =
| ConstI of int
| Var of ident                      (* x *)
| Fn of ident * tp * exp            (* fn x : t -> e  *)
| Delay of exp                      (* delay e *)
| LetDelay of ident * exp * exp     (* let delay x = e1 in e2 *)




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
  | Delay e -> Susp (infer c e)
  | LetDelay (x, e1,e2) -> 
      begin match infer c e1 with
      | Susp a -> infer ((x,a)::c) e2
      | _ -> raise (TypeError "LetDelay requires e1 be of type Susp a")
      end

(*       Gamma |- e :  A 
    --------------------------delay
    Gamma |- delay e : Susp A

    Gamma |- e1 : Susp A    Gamma,x: A |- e2 : B
    -------------------------------------------- letdelay
        Gamma |- let delay x = e1 in e2 : B
       
*)



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


(* outputs the result of [d/z]e *)
let rec subst ((d, z) : exp * ident) (e : exp) : exp = failwith "imagine this is implemented"
exception EvaluationStuck
let rec eval (e : exp): exp =
  match e with 
  | Var _ -> raise EvaluationStuck
  | ConstI i -> e
  | Fn (_,_,_) -> e         (* functions are values *)
  | Delay e' -> Delay e'
  | LetDelay (x, e1, e2) -> 
    (match  eval e1 with
    | Delay e1' -> eval (subst (e1',x) e2)
    | _ -> raise EvaluationStuck
    )

(*
   e1 || delay e1'   [e1'/x]e2 || v
   -------------------------------- letdelay     --------------------- delay
   let delay x = e1 in e2 || v                    delay e || delay e

*)



