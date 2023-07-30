(**  Parser *)

%{
  open Syntax
%}

(** tokens with values *)
(** Symbol atom name *)
%token <string> VAR    (** x, y, abc, ... *)
%token <string> CONSTR (** Cons, Node, ... *)

(** link name *)
%token <string> LINK   (** _X, _Y, _ABC, ...  *)

(** integer literal *)
%token <int> INT   (** 1, 2, 3, ...  *)

(** operators *)
%token DOT            (**  '.' *)
%token COMMA          (**  ',' *)
%token NU             (**  "nu" *)
%token NECKTIE        (**  "><" *)
%token CASE           (**  "case" *)
%token OF             (**  "of" *)
%token ARROW          (**  "->" *)
%token LAMBDA         (**  "\\" *)
%token OTHERWISE      (**  "otherwise" *)
%token VBAR           (**  "|" *)
%token LET            (**  "let" *)
%token REC            (**  "rec" *)
%token IN             (**  "in" *)
%token EQ             (**  "=" *)
%token PLUS           (**  "+" *)
%token MINUS          (**  "-" *)
%token TIMES          (**  "*" *)

(** Parentheses *)
%token LPAREN         (**  '(' *)
%token RPAREN         (**  ')' *)
%token LBRACKET       (**  '[' *)
%token RBRACKET       (**  ']' *)
%token LCBRACKET      (**  '{' *)
%token RCBRACKET      (**  '}' *)
%token LT             (**  '<' *)
(*
%token GT             (**  '>' *)
*)

(** End of file *)
%token EOF

(** Operator associativity *)
%nonassoc  DOT
%left      COMMA
%left      EQ
%left      LT
%left      PLUS MINUS
%left      TIMES
%nonassoc  LET IN CASE ARROW
%nonassoc  LPAREN LCBRACKET


%start ty_graph_eof
%type <string graph> ty_graph_eof


%start graph_eof
%type <atom_name graph> graph_eof

%start exp_eof
%type <exp> exp_eof

%%


(** arguments of an atom separated by comma without parentheses *)
let args_inner := ~ = separated_list(COMMA, LINK); <>



(** Type Graph *)

ty_atom_name:
  | CONSTR { $1 }

(**  proccesses separeted by comma *)
let ty_graph := graph(ty_atom_name)

(** the whole program *)
ty_graph_eof: ty_graph EOF { $1 }



(** Graph Template *)

(** Syntax for an atom *)
atom_name:
  | CONSTR { PConstr ($1) }
  | LPAREN LAMBDA var DOT exp RPAREN { PLam ($3, $5) }
  | INT { PInt ($1) }
  | MINUS INT { PInt (- $2) }


atom(atom_name):
  | atom_name				            { Atom ($1, []) }	(** e.g. C *)
  | atom_name LPAREN args_inner RPAREN	{ Atom ($1, $3) }	(** e.g. C (_X1, ..., _Xm) *)


var:
  | VAR { ($1, []) }	(** e.g. a *)
  | VAR LBRACKET args_inner RBRACKET { ($1, $3) }	(** e.g. x [_X1, ..., _Xm] *)



(**  proccesses separeted by comma *)
graph(atom_name):
  | atom(atom_name) { $1 }

  | var { let (v, args) = ($1) in Var (v, args) }	(** e.g. x[_X1, ..., _Xm] *)

  | graph(atom_name) COMMA graph(atom_name) { Mol ($1, $3) }

  | NU LINK+ DOT graph(atom_name)
    { List.fold_right (fun x graph -> Nu (x, graph)) $2 $4 }

  | LPAREN graph(atom_name) RPAREN { $2 }

  | LINK NECKTIE LINK                   { Fuse ($1, $3) }


(** the whole program *)
graph_eof: graph(atom_name) EOF { $1 }



(** Expression *)

exp_single:
  | LCBRACKET graph(atom_name) RCBRACKET { Graph ($2) }

  | CASE exp OF LCBRACKET graph(atom_name) RCBRACKET ARROW exp VBAR OTHERWISE ARROW exp
      { Case ($2, $5, $8, $12) }

  | LET REC var var var* EQ exp IN exp
      { LetRec ($3, $4, List.fold_right (make_lambda $3) $5 $7, $9) }

  | LET var var* EQ exp IN exp
      { Let ($2, List.fold_right (make_lambda $2) $3 $5, $7) }
 
  | LPAREN exp RPAREN { $2 }


exp:
  | exp exp_single { App ($1, $2) }
  | exp_single     { $1 }
  | exp PLUS exp   { BinOp (( + ), "+", $1, $3) }
  | exp MINUS exp  { BinOp (( - ), "-", $1, $3) }
  | exp TIMES exp  { BinOp (( * ), "*", $1, $3) }
  | exp LT exp     { RelOp (( < ), "<", $1, $3) }
  | exp EQ exp     { RelOp (( = ), "=", $1, $3) }


(** the whole program *)
exp_eof: exp EOF { $1 }
