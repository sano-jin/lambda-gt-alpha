open Parse
open Util

(** Get the link name of [x] according to the given link environment [link_env].
    自由リンクであった場合は末尾に追加していく． *)
let get_link (i, link_env) x =
  match List.assoc_opt x link_env with
  | None -> ((succ i, link_env @ [ (x, i) ]), i)
  | Some y -> ((i, link_env), y)

(** Alpha convert local link names to numbers and flattern the graph (in a term)
    to a pair of a list of atoms and a list of variables.

    @param i the seed for the indentifier of local links (link ids).
    @param link_env the mapping from bounded link names to link ids .
    @param fusion fusion of link ids. *)
let rec alpha ((((i, link_env) as i_link_env), fusion) as env) = function
  | Zero -> (env, ([], []))
  | Atom (v, args) ->
      let i_link_env, links = List.fold_left_map get_link i_link_env args in
      ((i_link_env, fusion), ([ (v, links) ], []))
  | Var (x, args) ->
      let i_link_env, links = List.fold_left_map get_link i_link_env args in
      ((i_link_env, fusion), ([], [ (x, links) ]))
  | Mol (g1, g2) ->
      let env, (atoms1, gvars1) = alpha env g1 in
      let env, (atoms2, gvars2) = alpha env g2 in
      (env, (atoms1 @ atoms2, gvars1 @ gvars2))
  | Nu (x, g) ->
      (first <. first <. second) List.tl
      @@ alpha ((succ i, (x, i) :: link_env), fusion) g
  | Fuse (x, y) ->
      let i_link_env, x = get_link i_link_env x in
      let i_link_env, y = get_link i_link_env y in
      ((i_link_env, (x, y) :: fusion), ([], []))

(** Substitute a link [x] with a link [y] in [link_env] and a graph ([atoms] and
    [vars]). *)
let subst_links (link_env, (atoms, vars)) (x, y) =
  let subst x y z = if z = x then y else z in
  let subst_links = List.map <. second <. List.map <.. subst in
  ( List.map (second @@ subst x y) link_env,
    (subst_links x y atoms, subst_links x y vars) )

(** Fusion を行う．*)
let fuse_links fusion link_env graph =
  List.fold_left subst_links (link_env, graph) fusion

(** Preprocess graph. *)
let preprocess graph =
  let ((i, link_env), fusion), graph = alpha ((0, []), []) graph in
  let link_env, graph = fuse_links fusion link_env graph in
  ((i, link_env), graph)

(** Number of local links. *)
let num_local_links (link_env, (atoms, vars)) =
  List.length @@ List.sort_uniq compare @@ List.map snd link_env
  @ List.concat_map snd atoms @ List.concat_map snd vars

(** Apply a production rule. *)
let app_prod graph (_var, _rhs) = graph
