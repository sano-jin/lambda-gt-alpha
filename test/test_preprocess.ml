let test str =
  prerr_endline @@ "testing ... " ^ str;
  let graph = Parse.parse_graph str in
  prerr_endline @@ "    " ^ Pretty.string_of_p_graph graph;
  let _, e_graph = Gt.alpha100 graph in
  prerr_endline @@ "    " ^ Pretty.string_of_e_graph e_graph

let test () =
  test "Cons";
  test "Cons (_Z, _X)";
  test "x [_Z, _X]";
  test "nu _Z. (Cons (_Z, _X), Cons(_Y, _Z))";
  test "nu _Z. (x [_Z, _X], y [_Y, _Z])";
  test "nu _Z1. nu _Z2. (x [_Z1, _X], y [_Z2, _Z1], Cons(_Y, _Z2))";
  test "nu _Z. nu _X. A (_X)"
