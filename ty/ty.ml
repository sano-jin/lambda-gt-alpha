open Parse
open Util
open OptionExtra

(** [get_link (i, link_env) x ] gets the link name of [x] according to the given
    link environment [link_env]. 自由リンクであった場合は末尾に追加していく． *)
let get_link (i, link_env) x =
  match List.assoc_opt x link_env with
  | None -> ((succ i, link_env @ [ (x, i) ]), i)
  | Some y -> ((i, link_env), y)

(** [transform env graph] alpha converts local link names to numbers and
    flattern the graph (in a term) to a pair of a list of atoms and a list of
    variables.

    @param i the seed for the indentifier of local links (link ids).
    @param link_env the mapping from bounded link names to link ids .
    @param fusion fusion of link ids. *)
let rec transform ((((i, link_env) as i_link_env), fusion) as env) = function
  | Zero -> (env, ([], []))
  | Atom (v, args) ->
      let i_link_env, links = List.fold_left_map get_link i_link_env args in
      ((i_link_env, fusion), ([ (v, links) ], []))
  | Var (x, args) ->
      let i_link_env, links = List.fold_left_map get_link i_link_env args in
      ((i_link_env, fusion), ([], [ (x, links) ]))
  | Mol (g1, g2) ->
      let env, (atoms1, gvars1) = transform env g1 in
      let env, (atoms2, gvars2) = transform env g2 in
      (env, (atoms1 @ atoms2, gvars1 @ gvars2))
  | Nu (x, g) ->
      (first <. first <. second) List.tl
      @@ transform ((succ i, (x, i) :: link_env), fusion) g
  | Fuse (x, y) ->
      let i_link_env, x = get_link i_link_env x in
      let i_link_env, y = get_link i_link_env y in
      ((i_link_env, (x, y) :: fusion), ([], []))

(** [assign_ids atom_i graph] assign unique ids starting from [atom_i] to atoms
    and variables in the graph [graph]. *)
let assign_ids atom_i (atoms, vars) =
  let num_atoms = List.length atoms in
  let num_vars = List.length vars in
  let atoms = List.mapi (fun i atom -> (i + atom_i, atom)) atoms in
  let vars = List.mapi (fun i var -> (i + atom_i + num_atoms, var)) vars in
  (atom_i + num_atoms + num_vars, (atoms, vars))

let subst (x, y) z = if z = x then y else z

(** [subst_links graph (x, y)] substitutes a link [x] with a link [y] in the
    graph [graph] ([link_env], atoms [atoms] and variables [vars]). *)
let subst_links (link_env, (atoms, vars)) xy =
  let subst_links = List.map <. second <. second <. List.map <. subst in
  ( List.map (second @@ subst xy) link_env,
    (subst_links xy atoms, subst_links xy vars) )

(** [fuse_links link_env_graph fusions] fuses local links in the graph
    [link_env_graph] using the fusions [fusions]. 残りの fusions に対しても substitution
    を行いながら [link_env] と [graph] のリンクを substitute していく．*)
let rec fuse_links link_env_graph = function
  | [] -> link_env_graph
  | xy :: fusions ->
      fuse_links (subst_links link_env_graph xy)
      @@ List.map (fun (x, y) -> (subst xy x, subst xy y)) fusions

(** [preprocess (atom_i, link_i) graph] preprocesses graph assigning the ids as
    the local links starting from the seed [i]. *)
let preprocess (atom_i, link_i) graph =
  let ((link_i, link_env), fusion), graph =
    transform ((link_i, []), []) graph
  in
  let atom_i, graph = assign_ids atom_i graph in
  let link_env, graph = fuse_links (link_env, graph) fusion in
  ((atom_i, link_i), (link_env, graph))

(** [local_links_of graph] obtains local links of a preprocessed graph [graph]. *)
let local_links_of (link_env, (atoms, vars)) =
  List.sort_uniq compare @@ List.map snd link_env
  @ List.concat_map (snd <. snd) atoms
  @ List.concat_map (snd <. snd) vars

(** [alpha_links graph local_links_i] alpha converts the local links (ids) in
    the graph [graph] into the local links (ids) starting from the integer seed
    [local_link_i] *)
let alpha_links graph local_link_i =
  let local_links = local_links_of graph in
  let mappings = List.mapi (fun i x -> (x, i + local_link_i)) local_links in
  ( List.length local_links + local_link_i,
    List.fold_left subst_links graph mappings )

(** [reassign_ids graph atom_i] assign unique ids starting from [atom_i] to
    atoms and variables in the graph [graph]. *)
let reassign_ids atom_i (atoms, vars) =
  let graph = (List.map snd atoms, List.map snd vars) in
  assign_ids atom_i graph

let concat_graphs (atoms, vars) (atoms', vars') = (atoms @ atoms', vars @ vars')

(** [app_prod target var' prod local_link_i] applies the production rule [prod]
    substituting the variable [var'] that existed in the target graph [target].

    - The variable [var'] must have the same functor (the same and the same
      arity) that the variable on the left-hand side of the production rule
      [prod] has.
    - The variable [var'] in the target graph [target] must be removed before
      using this function.
    - やっぱり [var'] の removal もここでやることにした．

    @param local_link_i Fresh な local link の id を seed として与える． *)
let app_prod (link_env, graph) ivar' (var, rhs) (atom_i, local_link_i) =
  let local_link_i, (rhs_link_env, rhs_graph) = alpha_links rhs local_link_i in
  let atom_i, rhs_graph = reassign_ids atom_i rhs_graph in
  let graph = (second @@ List.filter @@ ( <> ) ivar') graph in
  let link_env_graph = (link_env, concat_graphs rhs_graph graph) in
  let x2y = List.combine (snd @@ snd ivar') (snd var) in
  let rhs_link_env' = List.map (first @@ flip List.assoc x2y) rhs_link_env in
  ((atom_i, local_link_i), fuse_links link_env_graph rhs_link_env')

(** [size_of_graph graph] returns the size of the graph [graph]. *)
let size_of_graph ((link_env, (atoms, vars)) as g) =
  let num_links = List.length (local_links_of g) - List.length link_env in
  let num_atoms_vars = List.length atoms + List.length vars in
  (num_atoms_vars, num_links)

let functor_of (p, xs) = (p, List.length xs)
let eq_funct pxs qys = functor_of pxs = functor_of qys

(** 適用可能なルール． *)
let applicable_rules var prods = List.filter (eq_funct var <. fst) prods

(* let apply_prod2 ((_, var) as ivar) target prods var_i local_link_i = let+
   prod = List.find_opt (eq_funct var <. fst) prods in let var_i, prod = let
   lhs_var, (rhs_link_env, (rhs_atoms, rhs_vars)) = prod in let rhs_vars =
   List.mapi (fun i var -> (i + var_i, var)) rhs_vars in ( List.length rhs_vars
   + var_i, (lhs_var, (rhs_link_env, (rhs_atoms, rhs_vars))) ) in

   let target = (second @@ second @@ List.filter @@ ( <> ) ivar) target in
   (var_i, target, prod) *)
