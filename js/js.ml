open Util
open Js_of_ocaml

type ('a, 'b) k = K of ((unit -> ('a, 'b) k) option * 'a * 'b)

let rec k_of_cont = function
  | Either.Right v ->
      K
        ( None,
          Js.string @@ Vis.pretty_graph v,
          Js.string @@ Gt.string_of_graph v )
  | Either.Left (cont, v) ->
      K
        ( Some (fun () -> k_of_cont @@ Vis.app_cont cont v),
          Js.string @@ Vis.pretty_graph v,
          Js.string @@ Gt.string_of_graph v )

let extract_k = function K k -> k
let eval_grad exp = k_of_cont @@ Vis.eval [] exp (Vis.Cont Either.right)

let () =
  print_endline "LambdaGT v3";

  Js.export "LambdaGT"
    (object%js
       method extractk = extract_k
       method parse = Parse.parse_exp <. Js.to_string
       method rungrad = eval_grad <. Parse.parse_exp <. Js.to_string
    end)
