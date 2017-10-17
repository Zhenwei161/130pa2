(* CSE 130: Programming Assignment 2
 * misc.ml
 *)

(* ***** DOCUMENT ALL FUNCTIONS YOU WRITE OR COMPLETE ***** *)

let assoc (d,k,l) = 
  let rec helper assoc_so_far remaining_elmts =
    match remaining_elmts with
    | [] -> d
    | (a,v)::t -> if a = k then v else helper ((a,v)::assoc_so_far) t
    in helper [] l;;  

(* fill in the code wherever it says : failwith "to be written" *)
let removeDuplicates l = 
  let rec helper (seen,rest) = 
      match rest with 
        [] -> seen
      | h::t -> 
        let seen' = if (List.mem h seen) then seen else h::seen in
        let rest' = t in
	  helper (seen',rest') 
  in
      List.rev (helper ([],l));;


(* Small hint: see how ffor is implemented below *)
let rec wwhile (f,b) = 
  let (b',c') = f b in
    if c' = false then b' else wwhile (f,b');;  

(* fill in the code wherever it says : failwith "to be written" *)
let rec fixpoint (f,b) = let f' x = (f x, not ((f x) = x)) 
  in wwhile (f',b);;
(*  let x' = f b in
    if x' = b then b else fixpoint (f,x');*)


(* ffor: int * int * (int -> unit) -> unit
   Applies the function f to all the integers between low and high
   inclusive; the results get thrown away.
 *)

let rec ffor (low,high,f) = 
  if low>high 
  then () 
  else let _ = f low in ffor (low+1,high,f)
      
(************** Add Testing Code Here ***************)
