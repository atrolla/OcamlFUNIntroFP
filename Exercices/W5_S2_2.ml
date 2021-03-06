(*
DISPLAYING A FILESYSTEM HIERARCHY  (320/320 points)
In this exercise, we will pretty-print directory structures.

The prelude gives the types that we will use to represent directory structures. A node in the filesystem is either a simple File, a Directory that contains a nested filesystem, or a Symlink.
The latter, as on Unix systems, is a fake file that redirects to another file. For this, it provides the relative path to this target file. The path is the list of directory to traverse to get to the target file, followed by the later. If one has to go a directory up, we use the common ".." directory name that represents the parent directory.
A filesystem is a list of named nodes. An example filesystem is given below, in the format that you will have to produce. Don't worry, we'll break this piece by piece.

  /photos
  | /march
  | | photo_1.bmp
  | | photo_2.bmp
  | | photo_3.bmp
  | | index.html
  | /april
  | | photo_1.bmp
  | | photo_2.bmp
  | | index.html
  /videos
  | video1.avi
  | video2.avi
  | video3.avi
  | video4.avi
  | best.avi -> video4.avi
  | index.html
  /indexes
  | videos.html -> ../videos/index.html
  | photos_march.html -> ../photos/march/index.html
  | photos_april.html -> ../photos/april/index.html
  | photos_may.html -> INVALID
This output was generated from the following OCaml structure.

[ "photos", Dir
    [ "march", Dir
        [ "photo_1.bmp", File ;
          "photo_2.bmp", File ;
          "photo_3.bmp", File ;
          "index.html", File ] ;
      "april", Dir
        [ "photo_1.bmp", File ;
          "photo_2.bmp", File ;
          "index.html", File ] ] ;
  "videos", Dir
    [ "video1.avi", File ;
      "video2.avi", File ;
      "video3.avi", File ;
      "video4.avi", File ;
      "best.avi", Symlink [ "video4.avi" ] ;
      "index.html", File ] ;
  "indexes", Dir
    [ "videos.html",
      Symlink [ ".." ; "videos" ; "index.html" ] ;
      "photos_march.html",
      Symlink [ ".." ; "photos" ; "march" ; "index.html" ] ;
      "photos_april.html",
      Symlink [ ".." ; "photos" ; "april" ; "index.html" ] ;
      "photos_may.html",
      Symlink [ ".." ; "photos" ; "may" ; "index.html" ] ] ]
Write a function print_path: string list -> unit that prints a relative path (the argument of a Symlink) and pretty prints it as shown in the example display, using slashes ('/') as separator.
As you can see in the example, the depth of a file in the filesystem (the number of nested folders that are its ancestors) is represented by a sequence of vertical lines. Write a function print_file: int -> string -> unit that prints a file name, with the given number of "| " in front of it.
Write a similar function print_symlink: int -> string -> string list -> unit that prints the link name, with the given number of "| " in front of it, and the relative path (preceded by an arrow " -> ").
Write a similar function print_dir: int -> string -> unit that prints the dir name, with the given number of "| " in front of it, and the prepended '/'.
Write a function print_filesystem: filesystem -> unit that traverses the filesystem, producing the same display as in the example. You will probably need an auxiliary, recursive function, and you will have to use the previous answers.
Write a function resolve: string list -> string list -> string list. It takes as parameters:
The full path from the root to a symlink, including its name. In the given example, that could be for instance [ "indexes" ; "photos_april.html" ].
The relative path for this symlink. Here, that would be [ ".." ; "photos" ; "april" ; "index.html" ].
The function returns the full path from the root to the target of the symlink. Here, we should get [ "photos" ; "april" ; "index.html" ]. Note that it may not be as easy as it seems, so you may think about it before plunging into the code.
Write a function file_exists : filesystem -> string list -> bool that tells if a file exists in the filesystem. The path is the full absolute path to the file, and the target must be a File, not a Dir or a Symlink.
Update your function print_filesystem: filesystem -> unit so that it replaces the printed relative path by "INVALID" when the symlink cannot be resolved to an existing file.
THE GIVEN PRELUDE

type filesystem =
  (string * node) list
and node =
  | File
  | Dir of filesystem
  | Symlink of string list

*)

let rec print_path = function
  | [] -> ()
  | h::t -> let path = List.fold_left (fun x y -> x ^ "/" ^ y) h t in
      print_string path ;;

let rec print_file lvl name = match lvl with
  | 0 -> print_string name
  | x -> (print_string "| " ; print_file (x-1) name);;

let rec print_symlink lvl name path = match lvl with
  | 0 -> (print_string name;print_string " -> ";print_path path)
  | x -> (print_string "| " ; print_symlink (x-1) name path);;

let rec print_dir lvl name =match lvl with
  | 0 -> (print_char '/';print_string name)
  | x -> (print_string "| " ; print_dir (x-1) name);;

(* let print_filesystem root =
  let rec print_filesystem lvl items = match items with
    | [] -> ()
    | h::t -> match h with
      | (name,Dir f) -> (print_dir lvl name;print_newline();print_filesystem (lvl+1) f;print_newline();print_filesystem lvl t)
      | (name,Symlink s) -> (print_symlink lvl name s;print_newline(); print_filesystem lvl t)
      | (name, _) -> (print_file lvl name;print_newline();print_filesystem lvl t)
  in
  print_filesystem 0 root ;;
*)
let rec resolve sym path =
  (* This pre-completed structure is only here to help you.
     If it confuses you, don't hesitate to change it. *)
  let rec resolve acc path = match path with
    | [] -> List.rev acc
    | h::t -> if (h="..")
        then try resolve (List.tl acc) t with
            _ -> resolve acc t
        else resolve (h::acc) t  in
  resolve (List.tl (List.rev sym)) path ;;

let rec file_exists root path = match path with
  | [] -> true
  | h::[] -> let rec search_in_dir file items = match items with
      | [] -> false
      | hi::ti -> (match hi with
          |  (name,Dir f) -> search_in_dir file ti
          | (name,Symlink s) -> search_in_dir file ti
          | (name, _) -> if (name=file) then true else search_in_dir file ti)
      in search_in_dir h root
  | h::t -> (let rec search_in_dir dir items = match items with
      | [] -> false
      | hi::ti -> match hi with
        |  (name,Dir f) -> if(name=dir) then file_exists f t else search_in_dir dir ti
        | (name,Symlink s) -> search_in_dir dir ti
        | (name, f) -> search_in_dir dir ti
     in search_in_dir h root);;

(* move print_filesystem here for exercise 8 *)
let print_filesystem root =
  let check_path revroot s =
    let abspath = resolve (List.rev revroot) s in
    if(file_exists root abspath = true) then s else ["INVALID"] in
  let rec print_filesystem lvl items acc = match items with
    | [] -> ()
    | h::t -> match h with
      | (name,Dir f) -> (print_dir lvl name;print_newline();print_filesystem (lvl+1) f (name::acc);print_newline();print_filesystem lvl t acc)
      | (name,Symlink s) -> (print_symlink lvl name (check_path (name::acc) s);print_newline(); print_filesystem lvl t acc)
      | (name, _) -> (print_file lvl name;print_newline();print_filesystem lvl t acc)
  in
  print_filesystem 0 root [];;
