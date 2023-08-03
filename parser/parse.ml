(** Parse *)

open Util
include Syntax

(** @return AST of type graph *)
let parse_ty = Parser.ty_eof Lexer.token <. Lexing.from_string

(** @return AST of type graph *)
let parse_ty_graph = Parser.ty_graph_eof Lexer.token <. Lexing.from_string

(** @return AST of graph *)
let parse_graph = Parser.graph_eof Lexer.token <. Lexing.from_string

(** @return AST of expression *)
let parse_exp str =
  let linebuf = Lexing.from_string str in
  try Parser.exp_eof Lexer.token linebuf
  with Parser.Error ->
    failwith
    @@ Printf.sprintf "At offset %d: syntax error.\n%!"
         (Lexing.lexeme_start linebuf)
