open Pretty
open Util

let alpha_min = snd <. Gt.alpha_atoms (0, [])

let test_find_atoms graph lhs =
  prerr_endline @@ "testing whether '" ^ graph ^ "' can be matched with (" ^ lhs
  ^ ":- ...)";
  let graph = Parse.parse_graph graph in
  let lhs = Parse.parse_graph lhs in
  let _, (graph, _) = Gt.alpha100 graph in
  let graph = alpha_min graph in
  let _, (lhs_atoms, lhs_ctxs) = Gt.alpha100 lhs in
  prerr_endline @@ Gt.string_of_graph graph;
  prerr_endline @@ Gt.string_of_graph lhs_atoms ^ ", " ^ string_of_ctxs lhs_ctxs;
  prerr_endline
    (match Gt.match_ (lhs_atoms, lhs_ctxs) graph with
    | None -> "match failed"
    | Some theta -> "match succeded with theta = " ^ string_of_theta theta);
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

  test_find_atoms "nu _W. Cons (_W, _W)"
    "nu _Z. nu _X. (Cons (_Z, _X), x [_Z, _X])";

  test_find_atoms "nu _W. (Cons (_W, _W), A (_W))"
    "nu _Z. nu _X. (Cons (_Z, _X), x [_Z, _X])";

  test_find_atoms "nu _Z1. nu _Z2. (Cons (_Z1, _Z2, _X), Zero (_Z1), Nil (_Z2))"
    "nu _W1. nu _W2. (Cons (_W1, _W2, _X), h [_W1], t [_W2])";

  test_find_atoms "nu _Z1. nu _Z2. (Cons (_Z1, _Y, _X), Zero (_Z1))"
    "nu _W1. nu _W2. (Cons (_W1, _W2, _X), h [_W1], t [_Y, _W2])";

  test_find_atoms "nu _Z1. nu _Z2. (Cons (_Z1, _Y, _X), Zero (_Z1))"
    "nu _W1. nu _W2. (nodes [_W2, _X], Cons (_W1, _Y, _W2), h [_W1])"
