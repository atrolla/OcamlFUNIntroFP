(*
PRINTING LISTS  (200/200 points)
Write a function print_int_list : int list -> unit that takes a list of integers as input, and prints all the elements of the list, each on its own line.
Write a function print_every_other : int -> int list -> unit that takes a value k and a list of integers as input, and prints the elements of the list that are in positions multiple of k, each on its own line. Note: the first element of a list is at the position 0, not 1.
Write a function print_list : ('a -> unit) -> 'a list -> unit that takes a printer of values of some type 'a and a list of such values as input, and prints all the elements of the list, each on its own line.

*)

let rec print_int_list = function
  | [] -> ()
  | h::t -> print_int h ; print_newline () ; print_int_list t;;

let print_every_other k l= let rec func x= function
    | [] -> ()
    | h :: t -> if (x=k) then (print_int h ; print_newline () ; func 1 t)
        else func (x+1) t
  in func k l;;

let rec print_list print = function
  | [] -> ()
  | h::t -> print h ; print_newline () ; print_list print t;;
