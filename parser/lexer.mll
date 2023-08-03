(** Lexer *)

{
  open Parser
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let alpha = lower | upper 
let alnum = digit | alpha | '\'' | '_'
	       
			      
rule token = parse
  (** Operators *)
  | '.'			{ DOT }
  | ','			{ COMMA }
  | "nu"		{ NU }
  | "><"		{ NECKTIE }
  | "case"      { CASE }
  | "of"        { OF }
  | "->"        { ARROW }
  | "\\"        { LAMBDA }
  | "otherwise" { OTHERWISE }
  | "|"         { VBAR }
  | "let"       { LET }
  | "rec"       { REC }
  | "in"        { IN }
  | "="         { EQ }
  | "+"         { PLUS }
  | "-"         { MINUS }
  | "*"         { TIMES }
  | ":"         { COLON }

  (** Parentheses *)
  | '('			{ LPAREN }
  | ')'			{ RPAREN }
  | '['			{ LBRACKET }
  | ']'			{ RBRACKET }
  | '{'			{ LCBRACKET }
  | '}'			{ RCBRACKET }
  | '<'			{ LT }
  (*
  | '>'			{ GT }
   *)
			
  (** integer literal *)
  | ['1'-'9'] digit* | '0'
    { INT (int_of_string @@ Lexing.lexeme lexbuf) }

  (** constructor name *)
  | upper alnum*
    { CONSTR (Lexing.lexeme lexbuf) }

  (** variable name *)
  | lower alnum*
    { VAR (Lexing.lexeme lexbuf) }

  (** link name *)
  | '_' upper alnum*
    { LINK (Lexing.lexeme lexbuf) }


  (** end of file *)
  | eof       { EOF }

  (** spaces *)
  | space+    { token lexbuf }

  (** comments *)
  | '%' [^ '\n']*  { token lexbuf }

  | _
    {
      let message = Printf.sprintf
        "unknown token '%s' near characters %d-%d"
        (Lexing.lexeme lexbuf)
        (Lexing.lexeme_start lexbuf)
        (Lexing.lexeme_end lexbuf)
      in
      failwith message
    }

