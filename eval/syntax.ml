open Parse
open Util

type link =
  | FreeLink of string  (** free link *)
  | LocalLink of int  (** local link *)

let string_of_link = function
  | FreeLink link -> link
  | LocalLink i -> "_L" ^ string_of_int i

(** Free fusions. *)
module FFs = QuoSet.Make (struct
  type t = link

  let compare = compare
end)

type free_links = FFs.t
(** Free links are represented with a quotient set of link names. *)

type atom_name =
  | Constr of string  (** Constructor Name. *)
  | Lam of Parse.ctx * exp * theta  (** Lambda Abstraction. *)
  | RecLam of Parse.ctx * Parse.ctx * exp * theta
      (** Lambda Abstraction with a name for a recursive definition. *)

and atom = atom_name * link list

and ctx = string * link list
(** Graph context. *)

and graph = atom list
(** Graphs as data. Notice that the fusions are incorporated as Constructor
    atoms in this representation. *)

and theta = (ctx * graph) list
(** Graph substitution, i.e., environment. Fusions are incorporated as
    Constructor atoms here. We convert them to a [QuoSet] after evaluating
    (substituting). *)

type graph_template = atom list * ctx list
(** A template of a graph: graph with wild-cards for pattern matchings. *)

let string_of_atom_name = function
  | Constr name -> name
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
let fusion_of (x, y) : atom = (Constr "><", [ x; y ])

(** [is_free_link x] tests whether the [x] is a free link or not. *)
let is_free_link = function LocalLink _ -> false | FreeLink _ -> true

(** [local_links_of_atoms atoms] gathers all the local links in [atoms]. *)
let local_links_of_atoms atoms =
  List.concat_map (List.filter (not <. is_free_link)) @@ List.map snd atoms

(** [free_links_of_atoms atoms] gathers all the free links in [atoms]. *)
let free_links_of_atoms atoms =
  (List.concat_map @@ List.filter is_free_link <. List.map snd) atoms

(** [dump_atoms atoms] converts [atoms] to a string without \nu. *)
let dump_atoms atoms = "{" ^ ListExtra.string_of_seq string_of_atom atoms ^ "}"

(** [string_of_graph atoms] pretty prints [atoms]. *)
let string_of_graph ((atoms : atom list), fusions) =
  let fusions = List.map fusion_of @@ FFs.graph fusions in
  let graph = atoms @ fusions in
  let graph_str = ListExtra.string_of_seq string_of_atom graph in
  let local_links = List.sort_uniq compare @@ local_links_of_atoms atoms in
  if local_links = [] then "{" ^ graph_str ^ "}"
  else
    let local_links_str =
      "nu " ^ ListExtra.string_of_seq string_of_link local_links ^ ". "
    in
    if List.length atoms > 1 then "{" ^ local_links_str ^ "(" ^ graph_str ^ ")}"
    else "{" ^ local_links_str ^ graph_str ^ "}"
