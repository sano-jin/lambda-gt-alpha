open Util
open Util.OptionExtra
open Syntax
open Preprocess

(** lambda abstraction atom を評価した時に，クロージャにする *)
let make_closure theta = function
  | Lam (ctx, e, _), links -> (Lam (ctx, e, theta), links)
  | atom -> atom

(** Check that the given two atoms or graph contexts have the same functors. *)
let check_functor (v1, args1) (v2, args2) =
  (v1, List.length args1) = (v2, List.length args2)

(** [subst theta template_graph] substitutes graph contexts in [template_graph]
    with [theta]. *)
let subst theta template_graph =
  let i, (atoms, ctxs) = alpha 0 [] template_graph in
  let atoms = List.map (make_closure theta) atoms in
  let subst_graph i ctx =
    match List.find_opt (check_functor ctx <. fst) theta with
    | None -> failwith @@ "unbound graph context " ^ fst ctx
    | Some (ctx2, graph) ->
        let link_theta = List.combine (snd ctx2) (snd ctx) in
        alpha_atoms (i, link_theta) graph
  in
  let _, graphs = List.fold_left_map subst_graph i ctxs in
  atoms @ List.concat graphs

(** Get fusing links from an atom. At least one of the links should be a local
    link. *)
let get_local_fusion_opt = function
  | Constr "><", ([ (LocalLink _ as x); y ] | [ y; (LocalLink _ as x) ]) ->
      Some (x, y)
  | _ -> None

(** [fuse_fusions graph] absorbs all the fusions in the [graph] except for that
    connects free links. *)
let fuse_fusions graph =
  let fuse_fusion graph =
    let+ fusion, (g1, g2) =
      ListExtra.rev_break_opt get_local_fusion_opt graph
    in
    Postprocess.subst_link_of_atoms [ fusion ] (List.rev_append g1 g2)
  in
  whileM fuse_fusion graph
