(** Parse *)

include Syntax

(** @return AST of expression *)
let make_parser parser_ str =
  let linebuf = Lexing.from_string str in
  try parser_ Lexer.token linebuf with
  | Parser.Error ->
      Printf.eprintf "At offset %d: syntax error.\n%!"
        (Lexing.lexeme_start linebuf);
      exit 1
  | Lexer.Error msg ->
      Printf.eprintf "%s%!" msg;
      exit 1

(** @return AST of type graph *)
let parse_ty = make_parser Parser.ty_eof

(** @return AST of type graph *)
let parse_ty_graph = make_parser Parser.ty_graph_eof

(** @return AST of graph *)
let parse_graph = make_parser Parser.graph_eof

(** @return AST of expression *)
let parse_exp = make_parser Parser.exp_eof
