(*
grade_only [ 7;8;9;10;11 ]

let rec loop p f x = let px = p x in
  if (px = true) then x else loop p f (f x);;

let rec exists p l = match l with
  | [] -> false
  | h::t -> let ph = p h in
      if(ph = true) then true else exists p t;;

let rec find p l = match l with
  | [] -> raise NotFound
  | h::t -> let ph = p h in
      if(ph = true) then h else find p t ;;

(* --- Part A: A Generic Problem Solver --- *)

let near x = let n = 2 in
  let from = x-n in
  let rec machin y acc = if(y < from)
    then acc
    else machin (y-1) (y::acc) in
  machin (x+n) [];;

let flat_map f l = List.concat (List.map f l);;

let iter_rel rel n t =
  let rec truc x acc = if (x<1) then acc else
      let nacc = flat_map rel acc in
      truc (x-1) nacc
  in truc n [t];;

let solve r p x = let res = loop (exists p) (flat_map r) [x]
  in find p res;;

let solve_path r p x = let r' i= List.map (fun j -> i@[j]) (flat_map r i) in
  let p' = exists p in
  solve r' p' x;;

let archive_map opset r (s, l) =
  "Replace this string with your implementation." ;;

let solve' opset r p x =
  "Replace this string with your implementation." ;;

let solve_path' opset r p x =
  "Replace this string with your implementation." ;;

let solve_puzzle p opset c =
  "Replace this string with your implementation." ;;

(* --- Part B: A Solver for Klotski --- *)

let final board =
  "Replace this string with your implementation." ;;

let move_piece board piece { drow; dcol } =
  "Replace this string with your implementation." ;;

let possible_moves board =
  "Replace this string with your implementation." ;;

module BoardSet = Set.Make (struct
    type t = board
    let compare b1 b2 =
      failwith "Replace this with your implementation." ;;
  end)

let solve_klotski initial_board =
  "Replace this string with your implementation." ;;

*)

(* -- Part A -------------------------------------------------------------- *)

let words str = let nonesc = "azertyuiopqsdfghjklmwxcvbnAZERTYUIOPQSDFGHJKLMWXCVBN0123456789" in
  let b = ref (Buffer.create 16) in
  let acc = ref [] in
  for i=0 to (String.length str)-1 do
    let c = str.[i] in
    if (String.contains nonesc c)
    then
      Buffer.add_char !b c
    else
      (let contents = Buffer.contents !b in
       acc := (contents::!acc);
       b := Buffer.create 16)
  done;
  let contents = Buffer.contents !b in
  if (contents = "") then () else acc := (contents::!acc);
  List.rev !acc
;;

let build_ltable words =
  let rec assoc l acc = match l with
    | [] -> acc
    | h::[] -> ((h,"STOP")::acc)
    | h::i::t -> assoc (i::t) ((h,i)::acc)
  in
  let rec append key va acc = function
    | [] -> (key,va::[])::acc
    | (a,b)::t -> if (key = a)
        then (a, (va::b))::acc @ t
        else append key va ((a,b)::acc) t
  in let rec cltable l acc = match l with
      | [] -> acc
      | (a,b)::t -> cltable t (append a b [] acc)
  in let list = assoc words [("START",List.hd words)] in
  cltable list []
;;

let next_in_ltable table word =
  let succ = List.assoc word table
  in let i = Random.int (List.length succ) in
  List.nth succ i;;

let walk_ltable table =
  let rec walk table word acc =
    let next = next_in_ltable table word in
    if (next <> "STOP")
    then walk table next (next::acc)
    else List.rev acc
  in  walk table "START" []
;;

(* -- Part B -------------------------------------------------------------- *)

let compute_distribution l = let uniq = List.sort_uniq String.compare l in
  let assoc = List.map (fun x -> let occ = List.length (List.filter (fun i -> i = x) l)
                         in (x,occ)
                       ) uniq in
  { total = List.length l ; amounts = assoc };;

let build_htable words = let htable = Hashtbl.create 3 in
  let k = ref "" in
  Hashtbl.replace htable "START" [List.hd words];
  Hashtbl.replace htable (List.hd (List.rev words))  ["STOP"];
  List.iter (fun w -> if (!k = "")
              then
                k := w
              else
                (if (Hashtbl.mem htable !k)
                 then
                   Hashtbl.replace htable !k (w::Hashtbl.find htable !k)
                 else
                   Hashtbl.replace htable !k [w];
                 k := w)
            )
    words ;

  let ftable = Hashtbl.create 3 in
  Hashtbl.iter (fun k v -> Hashtbl.replace ftable k (compute_distribution v)) htable;
  ftable
;;

let next_in_htable table word = let dist = Hashtbl.find table word
  in let rand = Random.int dist.total
  in let rec next l i = match l with
      | [] -> raise (Failure "never append")
      | (a,b)::t -> if (i >= b) then next t (i-b) else a
  in next dist.amounts rand;;

(*let walk table =
   "Replace this string with your implementation." ;;*)

let walk_htable table =
  let rec walk table word acc =
    let next = next_in_htable table word in
    if (next <> "STOP")
    then walk table next (next::acc)
    else List.rev acc
  in  walk table "START" [];;

(* -- Part C -------------------------------------------------------------- *)

let sentences str =
  let cut h =
    let is_char = fun x -> let i = Char.code x in ((i>=128 && i <=255) || (i>=97 && i <=122) || (i>=65 && i <=90) || (i>=48 && i <=57))
    in let is_word = fun x -> (x = ';' ||x = ','|| x = ':'||x =  '-'|| x = '"'|| x = '\''|| x = '?'|| x = '!' || x = '.')
    in let to_string = fun x -> let c = Buffer.create 1 in
         Buffer.add_char c x;
         Buffer.contents c
    in let b = Buffer.create 16
    in let acc2 = ref []
    in (for i=0 to (String.length h)-1 do
          let c = h.[i] in
          if (is_char c)
          then
            Buffer.add_char b c
          else
            (let contents = Buffer.contents b in
             if (0 < String.length contents)
             then acc2 := (contents::!acc2)
             else ();
             if (is_word c)
             then acc2 := (to_string c)::!acc2
             else ();
             Buffer.reset b)
        done;
        let contents = Buffer.contents b in
        if (contents = "") then () else acc2 := (contents::!acc2);
        List.rev !acc2) in
  let sent =
    let w = Buffer.create 16 in
    let acc = ref [] in
    (for i=0 to (String.length str)-1 do
       let c = str.[i] in
       Buffer.add_char w c;
       if (c = '.' || c = '?' || c = '!')
       then
         let contents = cut (Buffer.contents w) in
         (acc := (contents::!acc);
          Buffer.reset w)
       else
         ()
     done ;
     let contents = cut (Buffer.contents w) in
     if (contents = []) then () else acc := (contents::!acc);
     List.rev !acc)
  in sent;;

let start pl =
  let rec machin x acc =
    if (x>0)
    then machin (x-1) ("START"::acc)
    else acc
  in machin pl [];;

let shift l x = match l with
  | [] -> [x]
  | h::t -> t @ [x];;

let build_ptable words pl =
  let start = start pl
  in let create_dist n = { total = 1 ;
                           amounts = [(n,1)] }
  in let modify_dist dist n =
       let replace_or_add l n =
         if (List.mem_assoc n l)
         then List.map (fun (x,i) -> if (x = n) then (x,i+1) else (x,i)) l
         else l @ [(n,1)]
       in
       { total = dist.total +1 ;
         amounts = (replace_or_add dist.amounts n) }
  in let modify_htable h p n =
       if (Hashtbl.mem h p)
       then
         let current_dist = Hashtbl.find h p
         in Hashtbl.replace h p (modify_dist current_dist n)
       else
         Hashtbl.replace h p (create_dist n)
  in let htable = Hashtbl.create 10
  in let rec build_table p w = match w with
      | [] -> modify_htable htable p "STOP"
      | h::t ->
          (modify_htable htable p h;
           let np = shift p h in
           build_table np t)
  in
  build_table start words;
  { prefix_length = pl ;
    table = htable};;

let walk_ptable { table ; prefix_length = pl } =
  let rec walk table word acc =
    let next = next_in_htable table word in
    if (next <> "STOP")
    then
      walk table (shift word next) (next::acc)
    else (
      let i = Random.int 10 in
      if (i <8)
      then
        List.rev acc
      else
        walk table (start pl) acc)
  in  walk table (start pl) [];;

let merge_ptables tl = match tl with
  | [] -> raise (Failure "no ptables")
  | h::t -> let rec merge tables {table ; prefix_length = pl} = match tables with
      | [] -> {table = table ; prefix_length = pl}
      | { table = table2 ; prefix_length = pl2 }::tail -> if (pl <> pl2)
          then raise (Failure "incompatible prefix_length")
          else
            let merge_dist dist dist2 =
              let replace_or_add l (word,occ) =
                if (List.mem_assoc word l)
                then List.map (fun (x,i) -> if (x = word) then (x,i+occ) else (x,i)) l
                else l @ [(word,occ)]
              in let rec merge_amounts a1 a2 = match a2 with
                  | [] -> a1
                  | h::t -> merge_amounts (replace_or_add a1 h) t
              in
              { total = dist.total + dist2.total ;
                amounts = (merge_amounts dist.amounts dist2.amounts) }
            in let merge_table t1 t2 =
                 let modify_htable str_list dist =
                   if (Hashtbl.mem t1 str_list)
                   then
                     let current_dist = Hashtbl.find t1 str_list
                     in Hashtbl.replace t1 str_list (merge_dist current_dist dist)
                   else
                     Hashtbl.replace t1 str_list dist
                 in (Hashtbl.iter (modify_htable) t2;
                     t1)
            in merge tail {table = (merge_table table table2) ; prefix_length = pl}
      in merge t h;;
