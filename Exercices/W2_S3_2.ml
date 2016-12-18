(*
SEARCHING FOR STRINGS IN ARRAYS  (30/30 points)
Write a function is_sorted : string array -> bool which checks if the values of the input array are sorted in strictly increasing order, implying that its elements are unique (use String.compare).
Using the binary search algorithm, an element can be found very quickly in a sorted array.
Write a function find : string array -> string -> int such that find arr word is the index of the word in the sorted array arr if it occurs in arr or -1 if word does not occur in arr.
The number or array accesses will be counted, to check that you obtain the expected algorithmic complexity. Beware that you really perform the minimal number of accesses. For instance, if your function has to test the contents of a cell twice, be sure to put the result of the access in a variable, and then perform the tests on that variable.
*)

let rec is_sortedp i a =
  if i+1 = Array.length a
  then true
  else if String.compare a.(i) a.(i+1) < 0
  then is_sortedp (i+1) a
  else false ;;

let is_sorted a =
  if a = [||] then true
  else is_sortedp 0 a;;

let rec findp l r dict word =
  if l > r
  then -1
  else let m = ((l+r)/2) in
    let value = dict.(m) in
    if String.compare value word < 0 then findp (m+1) r dict word
    else if String.compare value word > 0 then findp l (m-1) dict word
    else m;;

let find dict word = findp 0 ((Array.length dict)-1) dict word;;
