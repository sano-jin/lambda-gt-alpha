open Parse
open Syntax

let get_link link_env x =
  match List.assoc_opt x link_env with None -> FreeLink x | Some y -> y

let rec alpha i link_env = function
  | Zero -> (i, ([], []))
  | Atom (v, args) ->
      let v =
        match v with
        | PConstr constr -> Constr constr
        | PNum i -> Num i
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

let alpha_atom env (v, args) =
  let env, args = List.fold_left_map alpha_link env args in
  (env, (v, args))

let alpha_atoms (i, link_env) atoms =
  let (i, _), atoms = List.fold_left_map alpha_atom (i, link_env) atoms in
  (i, atoms)
