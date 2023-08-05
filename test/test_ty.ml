include Util

let test str =
  prerr_endline @@ "Case " ^ str;
  prerr_endline @@ "  parsing ... ";
  prerr_endline @@ "  running ... ";
  print_endline @@ "  >" ^ String.concat "\n  >"
  @@ List.map Ty.string_of_state
  @@ snd @@ Ty.gen_parse str;
  prerr_endline @@ "  end running ... "

let test () =
  test "Cons : x[] { x[] -> { Cons } }";

  test "Cons(_X) : x[_X] { x[_X] -> { Cons(_X) } }";

  test
    "nu _L. (Cons(_L, _X), Nil(_L)) : x[_X] { x[_X] -> { nu _L. (Cons(_L, _X), \
     x[_L]) } }";

  test
    "nu _L. (Cons(_L, _X), Cons(_Y, _L)) : dlist[_Y, _X] { dlist[_U, _V] -> { \
     nu _L. (Cons(_L, _V), dlist[_U, _L]) }, dlist[_U, _V] -> { _V >< _U }}";

  test
    "nu _L _L1 _L2. (Cons(_L1, _L, _X), int[_L1], Cons(_L2, _Y, _L), int[_L2]) \
     : dlist[_Y, _X] { dlist[_U, _V] -> { nu _L _L1. (Cons(_L1, _L, _V), \
     int[_L1], dlist[_U, _L]) }, dlist[_U, _V] -> { _V >< _U }}"

(* test "1 (_X)";

   test "Cons (_Z, _X)";

   test "x [_Z, _X]";

   test "nu _Z. (Cons (_Z, _X), Cons(_Y, _Z))";

   test "nu _Z. (x [_Z, _X], y [_Y, _Z])";

   test "nu _Z1. nu _Z2. (x [_Z1, _X], y [_Z2, _Z1], Cons(_Y, _Z2))";

   test "nu _Z1 _Z2. (x [_Z1, _X], y [_Z2, _Z1], Cons(_Y, _Z2))";

   test "nu _L0 _L1 _L2 _L3 _L4. (M (_L), Node (_L0, _L1, _X), Leaf (_L2, _L,
   _L3, \ _L0), Zero (_L2), Leaf (_L4, _L3, _R, _L1), Zero (_L4))"

   test_exp "{nu _Z. (x [_Z, _X], y [_Y, _Z])}";

   test_exp "{f} {nu _Z. (x [_Z, _X], y [_Y, _Z])}";

   test_exp "case {nu _Z. (x [_Z, _X], y [_Y, _Z])} of {nu _Z. (x [_Z, _X], y
   [_Y, \ _Z])} -> {A ()} | otherwise -> {B ()}";

   test_exp "{(\\x.{x})}";

   test_exp "{1}";

   test_exp "{1(_X)}";

   test_exp "{1(_X)} + {2(_X)}";

   test_exp @@ "{(\\y[_Y, _X]. {nu _Z. (x[_Z, _X], y[_Y, _Z])})}";

   test_exp @@ "{(\\x[_Y, _X]. {(\\y[_Y, _X]. {nu _Z. (x[_Z, _X], y[_Y,
   _Z])})})}";

   test_exp @@ "{(\\x[_Y, _X]. {(\\y[_Y, _X]. {nu _Z. (x[_Z, _X], y[_Y,
   _Z])})})}" ^ " {Cons (_X1, _Y, _X), Zero (_X1)}" ^ " {Cons (_X1, _Y, _X),
   Zero (_X1)} ";

   test_exp @@ "let x[_X] = {Val (_X)} in {x[_Y]}";

   test_exp @@ "let x[_Y, _X] = {nu _Z. (Cons (_Z, _Y, _X), Val (_Z))} in
   {x[_Y2, _X2]}";

   test_exp @@ "let rec f[_X] x[_Y, _X] = {nu _Z. (Cons (_Z, _Y, _X), Val1
   (_Z))} in \ {f[_X]} {Val2}" *)
