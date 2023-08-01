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

let subst (x, y) z = if z = x then y else z

(** Substitute a link [x] with a link [y] in [link_env] and a graph ([atoms] and
    [vars]). *)
let subst_links (link_env, (atoms, vars)) xy =
  let subst_links = List.map <. second <. List.map <. subst in
  ( List.map (second @@ subst xy) link_env,
    (subst_links xy atoms, subst_links xy vars) )

(** Fusion を行う．残りの fusions に対しても substitution を行いながら [link_env] と [graph] のリンクを
    substitute していく．*)
let rec fuse_links link_env_graph = function
  | [] -> link_env_graph
  | xy :: fusions ->
      fuse_links (subst_links link_env_graph xy)
      @@ List.map (fun (x, y) -> (subst xy x, subst xy y)) fusions

(** Preprocess graph. *)
let preprocess graph i =
  let ((i, link_env), fusion), graph = alpha ((i, []), []) graph in
  let link_env, graph = fuse_links (link_env, graph) fusion in
  ((i, link_env), graph)

(** Number of local links. *)
let num_local_links (link_env, (atoms, vars)) =
  List.length @@ List.sort_uniq compare @@ List.map snd link_env
  @ List.concat_map snd atoms @ List.concat_map snd vars

let concat_graphs (atoms, vars) (atoms', vars') = (atoms @ atoms', vars @ vars')

(** Apply a production rule. [app_prod target var' prod local_link_i] applies
    the production rule [prod] substituting the variable [var'] that existed in
    the target graph [target].

    - The variable [var'] must have the same functor (the same and the same
      arity) that the variable on the left-hand side of the production rule
      [prod] has.
    - The variable [var'] in the target graph [target] must be removed before
      using this function.

    @param local_link_i Fresh な local link の id を seed として与える． *)
let app_prod (link_env, graph) var' (var, rhs) local_link_i =
  let (local_link_i, rhs_link_env), rhs_graph = preprocess rhs local_link_i in
  let link_env_graph = (link_env, concat_graphs rhs_graph graph) in
  let x2y = List.combine (snd var') (snd var) in
  let rhs_link_env' = List.map (first @@ flip List.assoc x2y) rhs_link_env in
  (local_link_i, fuse_links link_env_graph rhs_link_env')
