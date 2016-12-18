(*
IMPLEMENTING MUTABLE LISTS  (80 points possibles)
Using mutable record fields, we can define the type of a list data structure with explicit pointers, as defined by the type 'a xlist given in the prelude.

The empty list is written:

{ pointer = Nil }

The singleton list containing only "one" is written:
{ pointer = List (1, { pointer = Nil }) }

The list containing the elements 1, then 2 then 3 is written:
{ pointer =
    List (1, { pointer =
                 List (2, { pointer =
                              List (3, { pointer =
                                           Nil }) }) }) }

Define head : 'a xlist -> 'a that returns the first element of the list if it exists, or fails with Empty_xlist.
This function does not modify the list.
Define tail : 'a xlist -> 'a xlist that returns the list without its first element if it exists, or fails with Empty_xlist.
This function does not modify the list.
Define add : 'a -> 'a xlist -> unit that modifies the list in place to add an element at the front.
Define chop : 'a xlist -> unit that modifies the list to remove its front element, or fails with Empty_xlist.
Define append : 'a xlist -> 'a xlist -> unit, a destructive concatenation operation that modifies the last pointer of the first list to point to the beginning of the second list.
Define filter : ('a -> bool) -> 'a xlist -> unit, a destructive filter operation on lists that removes from the list all elements that do not satisfy the boolean predicate passed as parameter.
THE GIVEN PRELUDE

type 'a xlist =
  { mutable pointer : 'a cell }
and 'a cell =
  | Nil
  | List of 'a * 'a xlist ;;

let nil () =
  { pointer = Nil } ;;

let cons elt rest =
  { pointer = List (elt, rest) } ;;

exception Empty_xlist ;;
*)

let head l =
  "Replace this string with your implementation." ;;

let tail l =
  "Replace this string with your implementation." ;;

let add a l =
  "Replace this string with your implementation." ;;

let chop l =
  "Replace this string with your implementation." ;;

let rec append l l' =
  "Replace this string with your implementation." ;;

let rec filter p l =
  "Replace this string with your implementation." ;;
