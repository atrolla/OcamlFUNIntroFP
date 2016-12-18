(*
FINDING THE MINIMUM  (20/20 points)
Consider a non empty array of integers a.

Write a function min : int array -> int that returns the minimal element of a.
Write a function min_index : int array -> int that returns the index of the minimal element of a.
Do you think these functions work well on large arrays ?

Define a variable it_scales and set it to "yes" or "no".
*)
let rec min_indexp index curr a =
  if curr+1 = Array.length a
  then index
  else if a.(curr+1) < a.(index)
  then min_indexp (curr+1) (curr+1) a
  else min_indexp index (curr+1) a;;

let min a = a.(min_indexp 0 0 a);;

let min_index a = min_indexp 0 0 a;;

let it_scales = "no" ;;
