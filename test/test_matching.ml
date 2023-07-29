open Util

let alpha_min = snd <. Gt.alpha_atoms (0, [])

let string_of_link_env =
  let helper (x, y) =
    Gt.string_of_link (LocalLink x) ^ "->" ^ Gt.string_of_link y
  in
  ListExtra.string_of_list helper

let test_find_atoms host_graph template_graph =
  prerr_endline @@ "testing whether a host graph '" ^ host_graph
  ^ "' can be matched with a graph template " ^ template_graph ^ ".";
  let graph = Parse.parse_graph host_graph in
  let lhs = Parse.parse_graph template_graph in
  let _, (graph, _) = Gt.alpha100 graph in
  let graph = alpha_min graph in
  let _, (lhs, _) = Gt.alpha100 lhs in
  prerr_endline @@ Gt.string_of_graph graph;
  prerr_endline @@ Gt.string_of_graph lhs;
  prerr_endline
    (match Gt.match_atoms Option.some lhs graph with
    | None -> "match failed"
    | Some (link_env, graph) ->
        "match succeded with link_env = "
        ^ string_of_link_env link_env
        ^ " where graph = " ^ Gt.string_of_graph graph ^ " left");
  prerr_newline ()

let test () =
  prerr_endline "Hello world!";

  test_find_atoms "Cons" "Cons";
  test_find_atoms "Cons, Cons" "Cons";
  test_find_atoms "Cons" "Cons, Cons";
  test_find_atoms "nu _X. (A(_X), B(_X))" "nu _X. (B(_X), A(_X))";
  test_find_atoms "nu _X. (A(_X), B(_X)), C" "nu _X. (B(_X), A(_X))";
  test_find_atoms "Cons (_Z, _X)" "Cons (_Z, _X)";
  test_find_atoms "nu _Z. (Cons (_Z, _X), Cons(_Y, _Z))" "Cons (_Z, _X)";
  test_find_atoms "nu _Z. nu _W. (Cons (_W, _X), Cons(_Y, _W))"
    "nu _Z. (Cons (_Z, _X), Cons(_Y, _Z))";
  test_find_atoms "Cons (_Z, _X)" "nu _Z. Cons (_Z, _X)";
  test_find_atoms "nu _W. Cons (_W, _X)" "nu _Z. Cons (_Z, _X)";
  test_find_atoms "nu _W. Cons (_W, _X)" "Cons (_Z, _X)";
  test_find_atoms "nu _Z. nu _X. Cons (_Z, _X)" "nu _W. Cons (_W, _W)";
  test_find_atoms "nu _W. (Cons (_W, _W), A (_W))"
    "nu _Z. nu _X. (Cons (_Z, _X), x [_Z, _X])"
