(*
STRING IDENTIFIERS  (2/2 points)
Suppose that a variable word exists and is a string.

Define a variable sentence that uses 5 string concatenations to create a string containing 9 times word, separated by commas (',').

This time, experiment with defining local let ... ins to store the partial results.
*)

let sentence =
  let c1 = word ^ ","
  in let c2 = c1 ^ c1
  in let c3 = c2 ^ c2
  in let c4 = c3 ^c3
  in c4 ^ word;;
