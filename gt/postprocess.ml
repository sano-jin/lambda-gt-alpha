open Util
open Syntax

(** atom のマッチング終了後，graph context のマッチングの前に，[link_env] を用いて，残りの target graph
    のリンク名を変換し，fusion を補う．*)

(** Link name substitution. *)
let subst_link_of_link link_env x =
  Option.value (List.assoc_opt x link_env) ~default:x

(** Link name substitutions in a list of atoms (a graph). *)
let subst_link_of_atoms = List.map <. second <. List.map <. subst_link_of_link

(** target graph において同じリンク名となるリンクに対応している，テンプレートのリンクをまとめる， *)
let gather_links =
  (List.map @@ List.map @@ fun x -> LocalLink x)
  <. List.map snd <. ListExtra.gather <. List.map swap

(** link_env から補った，局所リンク同士を引数にもつ fusion を作る *)
let local2local_fusions_of_link_env =
  let helper = function [] -> [] | x :: xs -> List.map (fusion_of x) xs in
  List.concat_map helper <. gather_links

(** マッチング対象のグラフでは自由リンクだが，テンプレートのリンクでは局所リンクであった場合に，自由リンクと局所リンクの fusion
    を作ってグラフに補う． *)
let local2free_fusions_of_link_env =
  List.map (fun (x, y) -> fusion_of (LocalLink x) y)
  <. List.filter (is_free_link <. snd)

(** マッチング対象のグラフのリンク名をテンプレートのリンク名に変換する． [link_env] と [rest_graph] を引数にとる． *)
let subst_links_of_rest_graph =
  let helper = function
    | x, (LocalLink _ as y) -> Some (y, LocalLink x)
    | _ -> None
  in
  subst_link_of_atoms <. List.filter_map helper

(** find_atoms をした後の link_env の後処理をする． 自由リンクと局所リンクを引数にもつ fusion を作る．
    マッチング対象のグラフの局所リンク名をルール左辺のものに合わせる． *)
let rest_graph_of (link_env, rest_graph) =
  subst_links_of_rest_graph link_env rest_graph
  @ local2local_fusions_of_link_env link_env
  @ local2free_fusions_of_link_env link_env
