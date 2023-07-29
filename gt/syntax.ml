open Parse
open Util

type link =
  | FreeLink of string  (** free link *)
  | LocalLink of int  (** local link *)

let string_of_link = function
  | FreeLink link -> link
  | LocalLink i -> "_L" ^ string_of_int i

type atom_name =
  | Constr of string  (** Constructor Name. *)
  | Int of int  (** integer value. *)
  | Lam of Parse.ctx * exp * theta  (** Lambda Abstraction. *)
  | RecLam of Parse.ctx * Parse.ctx * exp * theta
      (** Lambda Abstraction with a name for a recursive definition. *)

and atom = atom_name * link list

and ctx = string * link list
(** Graph context. *)

(* and graph = atom list * free_links *)
and graph = atom list
(** graph as data. *)

and theta = (ctx * graph) list
(** Graph substitution, i.e., environment. *)

type graph_template = atom list * ctx list
(** graph on the left/right-hand side of rules *)

let string_of_atom_name = function
  | Constr name -> name
  | Int i -> string_of_int i
  | Lam _ | RecLam _ -> "<fun>"

let string_of_atom = function
  | Constr "><", [ x; y ] -> string_of_link x ^ " >< " ^ string_of_link y
  | atom_name, [] -> string_of_atom_name atom_name
  | atom_name, args ->
      string_of_atom_name atom_name
      ^ " ("
      ^ String.concat ", " (List.map string_of_link args)
      ^ ")"

(** [fusion_of x y] creates a fusion atom ['><'(x, y)] from the link names [x]
    and [y]. *)
let fusion_of x y = (Constr "><", [ x; y ])

(** [is_free_link x] tests whether the [x] is a free link or not. *)
let is_free_link = function LocalLink _ -> false | FreeLink _ -> true

(** [local_links_of_atoms atoms] gathers all the local links in [atoms]. *)
let local_links_of_atoms atoms =
  List.concat_map (List.filter (not <. is_free_link)) @@ List.map snd atoms

(** [free_links_of_atoms atoms] gathers all the free links in [atoms]. *)
let free_links_of_atoms atoms =
  (List.concat_map @@ List.filter is_free_link <. List.map snd) atoms

(** [dump_atoms atoms] converts [atoms] to a string without \nu. *)
let dump_atoms atoms =
  "{" ^ String.concat ", " (List.map string_of_atom atoms) ^ "}"

(** [string_of_graph_with_nu atoms] pretty prints [atoms]. *)
let string_of_graph (atoms as graph) =
  let graph_str = String.concat ", " @@ List.map string_of_atom atoms in
  let local_links = List.sort_uniq compare @@ local_links_of_atoms graph in
  if local_links = [] then "{" ^ graph_str ^ "}"
  else
    let local_links_str =
      "nu " ^ String.concat " " (List.map string_of_link local_links) ^ ". "
    in
    if List.length atoms > 1 then "{" ^ local_links_str ^ "(" ^ graph_str ^ ")}"
    else "{" ^ local_links_str ^ graph_str ^ "}"
