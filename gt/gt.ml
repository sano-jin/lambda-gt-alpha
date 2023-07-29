open Util
include Syntax
include Preprocess
include Match_atoms
include Match_ctxs
include Pushout
include Postprocess

(** グラフのマッチングを行い．graph substitions を返す *)
let match_ (atoms_lhs, ctxs_lhs) target_graph =
  match
    Match_atoms.match_atoms
      (Match_ctxs.match_ctxs ctxs_lhs <. Postprocess.rest_graph_of)
      target_graph atoms_lhs
  with
  | Some (theta, []) -> Some theta
  | _ -> None
