open Util

let exec str =
  Yojson.Basic.pretty_to_string @@ Ty.json_of_repair @@ Ty.gen_parse str

let () =
  print_endline @@ exec @@ read_file Sys.argv.(1)
