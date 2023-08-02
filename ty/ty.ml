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
  num_atoms_vars + num_links

let functor_of (p, xs) = (p, List.length xs)
let eq_funct pxs qys = functor_of pxs = functor_of qys

type state_id = int * int
(** [state_id] is a pair of the id of a rule and the id of a non-terminal
    symbol. *)

module SIDs = Set.Make (struct
  type t = state_id

  let compare = compare
end)

module SIDss = Set.Make (SIDs)

let string_of_state (sid, ((atom_i, link_i), (link_env, (atoms, vars)))) =
  let string_of_locallink x = "L" ^ string_of_int x in
  let string_of_atom (atom_i, (p, xs)) =
    string_of_int atom_i ^ " : " ^ p ^ "("
    ^ String.concat ", " (List.map string_of_locallink xs)
    ^ ")"
  in
  let atoms_vars =
    String.concat ", "
    @@ List.map string_of_atom atoms
    @ List.map string_of_atom vars
  in
  let link_env =
    String.concat ", "
    @@ (List.map (fun (x, l) -> x ^ " -> " ^ string_of_locallink l)) link_env
  in
  let sid =
    "{" ^ String.concat ", "
    @@ List.map (fun (rule_id, var_id) ->
           "(" ^ string_of_int rule_id ^ ", " ^ string_of_int var_id ^ ")")
    @@ SIDs.elements sid
  in
  "{sid = " ^ sid ^ ", (atom_i, link_i) = (" ^ string_of_int atom_i ^ ", "
  ^ string_of_int link_i ^ "), " ^ "link_env = " ^ link_env ^ ", atoms_vars = "
  ^ atoms_vars ^ "}"

(** 非終端記号にルールの適用を試みる．

    [app_prod_opt max_size var sids state prod] applies the production rule
    [prod] to the variable [var] in the state [state] using the state ids [sids]
    if it does not exceed the size [max_size]. *)
let app_var_prod_opt max_size var sids ((sid, (atom_local_i, graph)) as state)
    (prod_i, prod) =
  let current_size = size_of_graph graph + (size_of_graph @@ snd prod) - 1 in
  let sid = SIDs.add (fst var, prod_i) sid in
  if
    eq_funct (snd var) (fst prod)
    && max_size >= current_size
    && not (SIDss.mem sid sids)
  then (
    print_endline @@ string_of_state state;
    Some (sid, app_prod graph var prod atom_local_i))
  else None

(** [app_var_prods next_states max_size sids state var prods] *)
let rec app_var_prods next_states max_size sids state var = function
  | [] -> (sids, next_states)
  | prod :: prods ->
      let sids, next_states =
        match app_var_prod_opt max_size var sids state prod with
        | None -> (sids, next_states)
        | Some ((sid, _) as next_state) ->
            (SIDss.add sid sids, next_state :: next_states)
      in
      app_var_prods next_states max_size sids state var prods

let app_var_prods max_size state prods sids var =
  app_var_prods [] max_size sids state var prods

let app_vars_prods max_size sids state prods vars =
  List.fold_left_map (app_var_prods max_size state prods) sids vars

(** [state = (sid, (atom_local_i, (link_env, (atoms, vars))))] *)
let vars_of_state (_, (_, (_, (_, vars)))) = vars

let gen_1_step max_size prods sids state =
  let vars = vars_of_state state in
  let sids, next_stateses = app_vars_prods max_size sids state prods vars in
  let next_states = List.concat next_stateses in
  (sids, next_states)

let rec gen max_size prods (sids, states) state =
  let sids, next_states = gen_1_step max_size prods sids state in
  List.fold_left (gen max_size prods) (sids, next_states @ states) next_states
