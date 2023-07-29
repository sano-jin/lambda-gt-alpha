open Util
open Util.OptionExtra
open Syntax

(** graph context のマッチングを行う．*)

let has_link_of_atom (_, args) x = List.mem x args
let has_links_of_atom xs atom = List.exists (has_link_of_atom atom) xs
let has_link_of_atoms x = List.exists @@ List.mem x <. snd

(** リンクを辿って，連結グラフを取得する *)
let rec traverse_links traversed_graph rest_graph traversing_links =
  let traversable_graph (* graph context の持つ自由リンクを持つアトムの集合 *), rest_graph =
    List.partition (has_links_of_atom traversing_links) rest_graph
  in
  if traversable_graph = [] then (traversed_graph, rest_graph)
  else
    let new_links = List.concat_map snd traversable_graph in
    let new_links = ListExtra.set_minus new_links traversing_links in
    traverse_links (traversable_graph @ traversed_graph) rest_graph new_links

(** graph context をマッチングさせる *)
let match_ctxs ctxs_lhs target_graph =
  let rec match_ctxs theta target_graph = function
    | [] -> Some (theta, target_graph)
    | ctx :: rest_lhs_ctxs ->
        (* ターゲットのグラフのマッチングを試していないアトムのリストを引数にとる *)
        let free_links = snd ctx in
        (let matched_graph, rest_target_graph =
           traverse_links [] target_graph free_links
         in
         if
           (* target graph の自由自由リンクは必ず template
              の自由リンクでマッチする必要があるので含まれているかどうか確認する． *)
           ListExtra.set_minus (free_links_of_atoms matched_graph) free_links
           = []
         then
           match_ctxs
             ((ctx, matched_graph) :: theta)
             rest_target_graph rest_lhs_ctxs
         else None)
        <|> fun _ -> match_ctxs theta target_graph rest_lhs_ctxs
  in
  match_ctxs [] target_graph ctxs_lhs
