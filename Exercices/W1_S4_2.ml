(*
SIMPLE FUNCTIONS OVER STRINGS  (12/12 points)
Let's define two functions working with strings:

last_character that returns the last character of a string, assuming that the string argument is not empty;
string_of_bool that converts a boolean value to its string representation.
*)

let last_character str =
  let lastCharIndex = (String.length str) - 1
  in String.get str lastCharIndex;;

let string_of_bool truth =
  if true = truth then "true" else "false";;
