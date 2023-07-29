open Util
open OptionExtra

let alpha_min = snd <. Gt.alpha_atoms (0, [])

let match_and_synthesis graph1 lhs graph2 =
  let+ theta = Gt.match_ lhs graph1 in
  Gt.subst theta graph2

let test_synthesis graph1 lhs graph2 =
  prerr_endline @@ "testing whether '" ^ graph1 ^ "' can be matched with ("
  ^ lhs ^ ":- ...)" ^ " and substituted to " ^ graph2;
  let graph1 = Parse.parse_graph graph1 in
  let graph2 = Parse.parse_graph graph2 in
  let lhs = Parse.parse_graph lhs in
  (* let _, (graph1, _) = alpha100 graph1 in let _, (graph2, _) = alpha100
     graph2 in let graph1 = alpha_min graph1 in *)
  let _, (graph1, _) = Gt.alpha100 graph1 in
  let _, (lhs_atoms, lhs_ctxs) = Gt.alpha100 lhs in
  let graph1 = alpha_min graph1 in
  prerr_endline @@ Gt.string_of_graph graph1;
  prerr_endline @@ Pretty.string_of_e_graph (lhs_atoms, lhs_ctxs);
  prerr_endline @@ Pretty.string_of_p_graph graph2;
  prerr_endline
    (match match_and_synthesis graph1 (lhs_atoms, lhs_ctxs) graph2 with
    | None -> "match failed"
    | Some graph ->
        "match succeded and reduced to graph = " ^ Gt.string_of_graph graph);
  prerr_newline ()

let test () =
  test_synthesis "Cons" "Cons" "Nil";

  test_synthesis "nu _X. (A(_X), B(_X))" "nu _X. (B(_X), A(_X))"
    "nu _X. (B(_X), A(_X))";

  test_synthesis "nu _W. (Cons (_W, _W), A (_W))"
    "nu _Z. nu _X. (Cons (_Z, _X), x [_Z, _X])" "x [_W1, _W2]";

  test_synthesis
    "nu _Z1. nu _Z2. (Cons (_Z1, _Z2, _X), Zero (_Z1), Nil\n   (_Z2))"
    "nu _W1. nu _W2. (Cons (_W1, _W2, _X), h [_W1], t [_W2])"
    "nu _W1. nu _W2. (Hoge (_W1, _W2, _X1), h [_W1], t [_W2])";

  test_synthesis "nu _Z1. nu _Z2. (Cons (_Z1, _Y, _X), Zero (_Z1))"
    "nu _W1. nu _W2. (Cons (_W1, _W2, _X), h [_W1], t [_Y, _W2])"
    "nu _W1. nu _W2. (Append (_W1, _W2, _X'), h [_W1], t [_Y', _W2])";

  test_synthesis "nu _Z1. nu _Z2. (Cons (_Z1, _Y, _X), Zero (_Z1))"
    "nu _W1. nu _W2. (Cons (_W1, _W2, _X), h [_W1], t [_Y, _W2])" "t [_Y, _X]"
