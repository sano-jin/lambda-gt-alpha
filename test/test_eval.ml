let test_eval exp =
  print_endline @@ "testing '" ^ exp ^ "'";
  let exp = Parse.parse_exp exp in
  let graph = Eval.eval exp in
  print_endline @@ "reduced to graph = " ^ Gt.string_of_graph graph;
  prerr_newline ()

let test () =
  test_eval "{nu _Z. (A (_Z, _X), B (_Y, _Z))}";

  test_eval "{<\\x.{x}>}";

  test_eval @@ "{<\\y[_Y, _X]. {nu _Z. (x[_Z, _X], y[_Y, _Z])}>}";

  test_eval
  @@ "{<\\x[_Y, _X]. {<\\y[_Y, _X]. {nu _Z. (x[_Z, _X], y[_Y, _Z])}>}>}";

  test_eval
  @@ "{<\\x[_Y, _X]. {<\\y[_Y, _X]. {nu _Z. (x[_Z, _X], y[_Y, _Z])}>}>}"
  ^ " {nu _X1. (Cons (_X1, _Y, _X), Zero1 (_X1))}"
  ^ " {nu _X1. (Cons (_X1, _Y, _X), Zero2 (_X1))} ";

  (* test_eval "{f} {nu _Z. (x [_Z, _X], y [_Y, _Z])}"; *)
  (* マッチングがうまくいかない例 *)
  test_eval
    "case {nu _Z. (A (_Z, _X), B (_Y, _Z))} of {nu _Z. (x [_Z, _X], y [_Y, \
     _Z])} -> {x [_U1, _U2]} | otherwise -> {B ()}";

  test_eval
    "case {nu _Z. (A (_Z, _X), B (_Y, _Z))} of {nu _Z. (A (_Z, _X), x [_Y, \
     _Z])} -> {x [_U1, _U2]} | otherwise -> {B ()}";

  test_eval @@ "case {nu _Z1. nu _Z2. (Cons (_Z1, _Y, _X), Zero (_Z1))} of "
  ^ "{nu _W1. nu _W2. (nodes [_W2, _X], Cons (_W1, _Y, _W2), h [_W1])} -> { \
     nodes [_Y, _X] } | otherwise -> { Error }";

  test_eval @@ "case {nu _Z1. nu _Z2. (Cons (_Z1, _Y, _X), Zero (_Z1))} of "
  ^ "{nu _W1. nu _W2. (nodes [_W2, _X], Cons (_W1, _Y, _W2), h [_W1])} -> { nu \
     _U1. nu _U2. (Cons (_U1, _U2, _X), Zero2 (_U1), nodes [_Y, _U2]) } | \
     otherwise -> { Error }";

  test_eval @@ "let x[_X] = {Val (_X)} in {x[_Y]}";

  test_eval
  @@ "let x[_Y, _X] = {nu _Z. (Cons (_Z, _Y, _X), Val (_Z))} in {x[_Y2, _X2]}";

  test_eval
  @@ "let rec f[_X] x[_Y, _X] = {nu _Z. (Cons (_Z, _Y, _X), Val1 (_Z))} "
  ^ "in {f[_X]} {Val2}"
