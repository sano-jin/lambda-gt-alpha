open Parse
open Syntax

(** Get the link name of [x] according to the given link environment [link_env]. *)
let get_link link_env x =
  match List.assoc_opt x link_env with None -> FreeLink x | Some y -> y

(** Alpha convert local link names to numbers and flattern graph to a list of
    atoms.

    @param i the seed for the indentifier of local links. *)

let rec alpha i link_env = function
  | Zero -> (i, ([], []))
  | Atom (v, args) ->
      let v =
        match v with
        | PConstr constr -> Constr constr
        | PInt i -> Int i
        | PLam (ctx, e) -> Lam (ctx, e, [])
      in
      let links = List.map (get_link link_env) args in
      (i, ([ (v, links) ], []))
  | Ctx (x, args) ->
      let links = List.map (get_link link_env) args in
      (i, ([], [ (x, links) ]))
  | Mol (g1, g2) ->
      let i, (atoms1, gctxs1) = alpha i link_env g1 in
      let i, (atoms2, gctxs2) = alpha i link_env g2 in
      (i, (atoms1 @ atoms2, gctxs1 @ gctxs2))
  | Nu (x, g) -> alpha (succ i) ((x, LocalLink i) :: link_env) g

let alpha100 = alpha 100 []

let alpha_link ((i, link_env) as env) x =
  match List.assoc_opt x link_env with
  | None -> ((succ i, (x, LocalLink i) :: link_env), LocalLink i)
  | Some x -> (env, x)

let alpha_atom link_env (v, args) =
  let env, args = List.fold_left_map alpha_link link_env args in
  (env, (v, args))

(** Alpha-convert local link names in a flatten graph (a list of atoms). *)
let alpha_atoms (i, link_env) atoms =
  let (i, _), atoms = List.fold_left_map alpha_atom (i, link_env) atoms in
  (i, atoms)

(** アトムを id でソートして，id を振り直す

    使われていない． *)
let reid atoms =
  let atoms = List.sort (fun ((i, _), _) ((j, _), _) -> compare i j) atoms in
  let rec helper ids = function
    | [] -> []
    | ((i, v), args) :: t ->
        let i = if List.mem i ids then Util.unique () else i in
        ((i, v), args) :: helper (i :: ids) t
  in
  helper [] atoms
