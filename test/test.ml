(* test util *)

let config =
  [
    ("parse", false);
    ("preprocess", false);
    ("matching", false);
    ("match_atoms", false);
    ("synthesis", false);
    ("eval", true);
  ]

let run_test name test =
  match List.assoc_opt name config with
  | None -> failwith @@ "Cannot find '" ^ name ^ "' in the configuration"
  | Some true ->
      prerr_endline @@ String.make 80 '-';
      prerr_endline @@ "testing '" ^ name ^ "'.";
      test ();
      prerr_newline ()
  | Some false -> ()

let () =
  run_test "parse" Test_parse.test;
  run_test "preprocess" Test_preprocess.test;
  run_test "matching" Test_matching.test;
  run_test "match_atoms" Test_match_atoms.test;
  run_test "synthesis" Test_synthesis.test;
  run_test "eval" Test_eval.test
