open Util
open Util.OptionExtra
open Syntax

(** [check_link link_env (host_link, template_link)] checks that whether the
    link in the host graph, [host_link], can match the link in the template
    graph, [template_link].

    @param link_env
      A mapping from the local links in the graph template to the links in the
      host graph. *)
let check_link link_env = function
  | FreeLink x, FreeLink y -> if x = y then Some link_env else None
  | LocalLink _, FreeLink _ ->
      None (* cannot match a local link with a free link *)
  | x, LocalLink y -> (
      match List.assoc_opt y link_env with
      | None -> Some ((y, x) :: link_env) (* bind if unbounded *)
      | Some z ->
          if x = z then Some link_env (* if matched with the environment *)
          else None)

(** [match_atom link_env (v1, args1) (v2, args2)] matches an atom [(v1, args1)]
    in the host graph and an atom [(v2, args2)] in the graph tempate with the
    [link_env]. Returns the new [link_env] if it succeeds to match. *)
let match_atom link_env (v1, args1) (v2, args2) =
  if v1 <> v2 then None
  else
    ListExtra.combine_opt args1 args2 >>= OptionExtra.foldM check_link link_env

(** [match_atoms f host_graph template_atoms] matches all the atoms in
    [template_atoms] to the [host_graph] and apply the obtained [link_env] and
    the rest host graph to [f].

    @param f
      A procedure done after the matching of the atoms (i.e., graph context
      matchings). *)
let match_atoms f host_graph template_atoms =
  let rec find_atoms link_env host_graph = function
    | [] -> f (link_env, host_graph)
    | template_atom :: rest_template_atoms ->
        (* ターゲットのグラフのマッチングを試していないアトムのリストを引数にとる *)
        let rec find_atom tested_host_atoms = function
          | [] -> None (* 全て失敗 *)
          | host_atom :: rest_host_atoms ->
              (let* link_env = match_atom link_env host_atom template_atom in
               let rest_host_graph =
                 List.rev_append tested_host_atoms rest_host_atoms
               in
               find_atoms link_env rest_host_graph rest_template_atoms)
              <|> fun _ ->
              find_atom (host_atom :: tested_host_atoms) rest_host_atoms
        in
        find_atom [] host_graph
  in
  find_atoms [] host_graph template_atoms
