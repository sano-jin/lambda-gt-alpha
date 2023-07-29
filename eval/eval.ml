open Parse
open Gt

let ctx_of (x, args) = (x, List.map (fun x -> FreeLink x) args)

let rec eval theta = function
  | Graph graph -> fuse_fusions @@ subst theta graph
  | RelOp (f, op, e1, e2) -> (
      match (eval theta e1, eval theta e2) with
      | [ (Int i1, xs1) ], [ (Int i2, _) ] ->
          [ (Constr (if f i1 i2 then "True" else "False"), xs1) ]
      | v1, v2 ->
          failwith @@ "integers are expected for " ^ op ^ " but were "
          ^ string_of_graph v1 ^ " and " ^ string_of_graph v2)
  | BinOp (f, op, e1, e2) -> (
      let v1 = eval theta e1 in
      let v2 = eval theta e2 in
      match (v1, v2) with
      | [ (Int i1, xs1) ], [ (Int i2, _) ] -> [ (Int (f i1 i2), xs1) ]
      | _ ->
          failwith @@ "integers are expected for " ^ op ^ " but were "
          ^ string_of_graph v1 ^ " and " ^ string_of_graph v2)
  | App (e1, e2) -> (
      let v1 = eval theta e1 in
      let v2 = eval theta e2 in
      match v1 with
      | [ (Constr "Log", _) ] ->
          print_endline @@ "> " ^ string_of_graph v2;
          v2
      | [ (Lam (ctx, e, theta), _) ] ->
          let ctx = ctx_of ctx in
          let theta = (ctx, v2) :: theta in
          eval theta e
      | [ ((RecLam (ctx1, ctx2, e, theta), _) as rec_lam) ] ->
          let ctx1 = ctx_of ctx1 in
          let ctx2 = ctx_of ctx2 in
          let theta = (ctx2, v2) :: (ctx1, [ rec_lam ]) :: theta in
          eval theta e
      | _ -> failwith @@ "function was expected but were " ^ string_of_graph v1)
  | Case (e1, template, e2, e3) -> (
      let v1 = eval theta e1 in
      let _, template = alpha100 template in
      match match_ template v1 with
      | None -> eval theta e3
      | Some theta2 ->
          let theta = theta2 @ theta in
          eval theta e2)
  | Let (ctx, e1, e2) ->
      let v1 = eval theta e1 in
      let ctx = ctx_of ctx in
      let theta = (ctx, v1) :: theta in
      eval theta e2
  | LetRec (ctx1, ctx2, e1, e2) ->
      let rec_lam = RecLam (ctx1, ctx2, e1, theta) in
      let ctx = ctx_of ctx1 in
      let theta = (ctx, [ (rec_lam, snd ctx) ]) :: theta in
      eval theta e2

let eval = eval []
