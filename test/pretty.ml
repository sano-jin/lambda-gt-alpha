(** Pretty print expressions. *)

open Eval
open Parse
open Util

let string_of_ctx (x, args) =
  if args = [] then x else x ^ "[" ^ String.concat ", " args ^ "]"

let rec string_of_exp = function
  | Graph graph -> "{" ^ string_of_p_graph graph ^ "}"
  | Case (e1, template, e2, e3) ->
      "(case " ^ string_of_exp e1 ^ " of {" ^ string_of_p_graph template
      ^ "} -> " ^ string_of_exp e2 ^ " | otherwise -> " ^ string_of_exp e3 ^ ")"
  | App (e1, e2) -> "(" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
  | LetRec (ctx1, ctx2, e1, e2) ->
      "(let rec " ^ string_of_ctx ctx1 ^ " " ^ string_of_ctx ctx2 ^ " = "
      ^ string_of_exp e1 ^ " in " ^ string_of_exp e2 ^ ")"
  | Let (ctx, e1, e2) ->
      "(let " ^ string_of_ctx ctx ^ " = " ^ string_of_exp e1 ^ " in "
      ^ string_of_exp e2 ^ ")"

and string_of_atom_name = function
  | PConstr name -> name
  | PLam (ctx, e) -> "<\\" ^ string_of_ctx ctx ^ ". " ^ string_of_exp e ^ ">"

and string_of_p_graph = function
  | Zero -> "0"
  | Atom (v, []) -> string_of_atom_name v
  | Atom (v, args) ->
      string_of_atom_name v ^ " (" ^ String.concat ", " args ^ ")"
  | Ctx (x, args) -> string_of_ctx (x, args)
  | Mol (g1, g2) ->
      "(" ^ string_of_p_graph g1 ^ ", " ^ string_of_p_graph g2 ^ ")"
  | Nu (x, g) -> "nu " ^ x ^ string_of_nus g

and string_of_nus = function
  | Nu (x, g) -> " " ^ x ^ string_of_nus g
  | g -> ". " ^ string_of_p_graph g

let string_of_rule (lhs, rhs) =
  string_of_graph lhs ^ " ---> " ^ string_of_graph rhs

let string_of_ctx (name, args) =
  name ^ " [" ^ String.concat ", " (List.map string_of_link args) ^ "]"

let string_of_e_graph (atoms, gctxs) =
  "{"
  ^ String.concat ", "
      (List.map string_of_atom atoms @ List.map string_of_ctx gctxs)
  ^ "}"

let string_of_ctxs ctxs =
  "{" ^ String.concat ", " (List.map string_of_ctx ctxs) ^ "}"

let string_of_theta (theta : theta) =
  let helper (ctx, graph) =
    string_of_ctx ctx ^ " -> " ^ Eval.string_of_graph graph
  in
  ListExtra.string_of_list helper theta
