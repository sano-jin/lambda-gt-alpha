open Util
open Parse
open Gt

let links_of_atoms atoms = List.concat_map snd @@ atoms

type cont = Cont of (Gt.graph -> (cont * Gt.graph, Gt.graph) Either.t)

let app_cont = function Cont cont -> cont

let rec eval theta exp cont =
  let k cont = Cont cont in
  match exp with
  | RelOp (f, op, e1, e2) -> (
      eval theta e1 @@ k
      @@ fun v1 ->
      eval theta e2 @@ k
      @@ fun v2 ->
      match (v1, v2) with
      | [ (Int i1, xs1) ], [ (Int i2, _) ] ->
          app_cont cont [ (Constr (if f i1 i2 then "True" else "False"), xs1) ]
      | v1, v2 ->
          failwith @@ "integers are expected for " ^ op ^ " but were "
          ^ string_of_graph v1 ^ " and " ^ string_of_graph v2)
  | BinOp (f, op, e1, e2) -> (
      eval theta e1 @@ k
      @@ fun v1 ->
      eval theta e2 @@ k
      @@ fun v2 ->
      match (v1, v2) with
      | [ (Int i1, xs1) ], [ (Int i2, _) ] ->
          app_cont cont [ (Int (f i1 i2), xs1) ]
      | _ ->
          failwith @@ "integers are expected for " ^ op ^ " but were "
          ^ string_of_graph v1 ^ " and " ^ string_of_graph v2)
  | Graph graph -> app_cont cont @@ fuse_fusions @@ subst theta graph
  | App (e1, e2) -> (
      eval theta e1 @@ k
      @@ fun v1 ->
      eval theta e2 @@ k
      @@ fun v2 ->
      match v1 with
      | [ (Lam (ctx, e, theta), _) ] ->
          let ctx = Gt.ctx_of ctx in
          let theta = (ctx, v2) :: theta in
          eval theta e cont
      | [ ((RecLam (ctx1, ctx2, e, theta), _) as rec_lam) ] ->
          let ctx1 = Gt.ctx_of ctx1 in
          let ctx2 = Gt.ctx_of ctx2 in
          let theta = (ctx1, [ rec_lam ]) :: (ctx2, v2) :: theta in
          eval theta e cont
      | [ (Constr "Log", _) ] -> Either.Left (cont, v2)
      | _ -> failwith @@ "function expected but got " ^ string_of_graph v1)
  | Case (e1, template, e2, e3) -> (
      eval theta e1 @@ k
      @@ fun v1 ->
      let _, template = alpha100 template in
      match match_ template v1 with
      | None -> eval theta e3 cont
      | Some theta2 ->
          let theta = theta2 @ theta in
          eval theta e2 cont)
  | Let (ctx, e1, e2) ->
      eval theta e1 @@ k
      @@ fun v1 ->
      let ctx = Gt.ctx_of ctx in
      let theta = (ctx, v1) :: theta in
      eval theta e2 cont
  | LetRec (ctx1, ctx2, e1, e2) ->
      let rec_lam = RecLam (ctx1, ctx2, e1, theta) in
      let ctx = Gt.ctx_of ctx1 in
      let theta = (ctx, [ (rec_lam, snd ctx) ]) :: theta in
      eval theta e2 cont

let exec code =
  let exp = Parse.parse_exp code in
  let rec helper = function
    | Either.Right v -> v
    | Either.Left (cont, v) ->
        print_endline @@ string_of_graph v;
        helper @@ app_cont cont v
  in
  helper @@ eval [] exp (Cont Either.right)

let vis () =
  let graph = exec @@ read_file Sys.argv.(1) in
  print_endline @@ "// " ^ Gt.string_of_graph graph;
  print_newline ()

let pretty_graph = Json.pretty_graph
