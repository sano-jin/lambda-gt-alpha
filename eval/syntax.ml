open Parse
open Util

type link =
  | FreeLink of string  (** free link *)
  | LocalLink of int  (** local link *)

let string_of_link = function
  | FreeLink link -> link
  | LocalLink i -> "_L" ^ string_of_int i

type atom_name =
  | Constr of string  (** constructor name *)
  | Num of int  (** integer *)
  | Lam of Parse.ctx * exp * theta  (** lambda abstraction *)
  | RecLam of Parse.ctx * Parse.ctx * exp * theta  (** lambda abstraction *)

and atom = atom_name * link list
and ctx = string * link list

and graph = atom list
(** graph as data*)

and theta = (ctx * graph) list

type e_graph = atom list * ctx list
(** graph on the left/right-hand side of rules *)

let string_of_atom_name = function
  | Constr name -> name
  | Num i -> string_of_int i
  | Lam _ | RecLam _ -> "<fun>"

let string_of_atom = function
  | Constr "><", [ x; y ] -> string_of_link x ^ " >< " ^ string_of_link y
  | atom_name, args ->
      string_of_atom_name atom_name
      ^ " ("
      ^ String.concat ", " (List.map string_of_link args)
      ^ ")"

let fusion_of x y = (Constr "><", [ x; y ])
let is_free_link = function LocalLink _ -> false | FreeLink _ -> true

let local_links_of_graph =
  List.concat_map (List.filter (not <. is_free_link)) <. List.map snd

let string_of_graph atoms =
  "{" ^ String.concat ", " (List.map string_of_atom atoms) ^ "}"

let string_of_graph_with_nu atoms =
  let graph_str = String.concat ", " @@ List.map string_of_atom atoms in
  let local_links = List.sort_uniq compare @@ local_links_of_graph atoms in
  let local_links_str =
    let helper local_link = "nu " ^ string_of_link local_link ^ ". " in
    String.concat "" @@ List.map helper local_links
  in
  if local_links <> [] && List.length atoms > 1 then
    "{" ^ local_links_str ^ "(" ^ graph_str ^ ")}"
  else "{" ^ local_links_str ^ graph_str ^ "}"
