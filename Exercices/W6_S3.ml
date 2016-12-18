(*
CHAR INDEXED HASHTABLES  (40/40 points)
Have a look at the documentation of module Hashtbl.

Implement a module CharHashedType, compatible with the HashedType signature, where type t = char.
Use the module defined in the previous question to instantiate the Hashtbl.Make functor as a module CharHashtbl.
Reimplement the data structure of trie from a previous exercise, so that a hash table is used to represent the association between characters and children. To do so, complete the definition of module Trie, so that it is compatible with the given signature GenericTrie, whose 'a table type is instanciated to char indexed hash tables.
Be careful, a hash table is not a purely functional data structure. Therefore, it must be copied when necessary!
Note: you must neither change the signature nor the types of module Trie or the tests will fail.
THE GIVEN PRELUDE

module type GenericTrie = sig
  type 'a char_table
  type 'a trie = Trie of 'a option * 'a trie char_table
  val empty : unit -> 'a trie
  val insert : 'a trie -> string -> 'a -> 'a trie
  val lookup : 'a trie -> string -> 'a option
end
*)

module CharHashedType =
struct
  type t = char

  let equal c = fun x -> x = c

  let hash = Char.code
end

module CharHashtbl = Hashtbl.Make(CharHashedType)

module Trie : GenericTrie
  with type 'a char_table = 'a CharHashtbl.t =

struct
  type 'a char_table = 'a CharHashtbl.t
  type 'a trie = Trie of 'a option * 'a trie char_table

  let empty () = Trie (None, CharHashtbl.create 0);;

  let insert trie w v =
    let length = (String.length w) -1
    in let rec add tbl i c v =
         if (CharHashtbl.mem tbl c)
         then
           match (CharHashtbl.find tbl c) with
           | Trie (_, t ) -> if (i = length)
               then CharHashtbl.replace tbl c (Trie (Some v, t))
               else add t (i+1) w.[i+1] v
         else
         if (i = length)
         then CharHashtbl.replace tbl c (Trie (Some v, CharHashtbl.create 0))
         else (let t = CharHashtbl.create 0
               in CharHashtbl.replace tbl c (Trie (None, t));
               add t (i+1) w.[i+1] v)
    in match trie with
    | Trie (_,tr) -> add tr 0 w.[0] v;
        trie;;

  let lookup trie w =
    let length = (String.length w ) -1
    in let rec find tbl i c =
         if (CharHashtbl.mem tbl c)
         then match (CharHashtbl.find tbl c) with
           | Trie (r, t ) -> if (i = length)
               then r
               else find t (i+1) w.[i+1]
         else None
    in match trie with
    | Trie (_,tr) -> find tr 0 w.[0];
  ;;

end
