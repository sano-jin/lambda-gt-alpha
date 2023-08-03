(** Syntax *)

type var = string * string list

(** graph template *)
type 'atom_name graph =
  | Zero
  | Atom of 'atom_name * string list  (** atom. e.g. [C(_X, _Y, _Z)] *)
  | Var of var  (** graph context. e.g. [x\[_X, _Y\]] *)
  | Mol of 'atom_name graph * 'atom_name graph  (** molecule *)
  | Nu of string * 'atom_name graph  (** hyperlink creation *)
  | Fuse of string * string  (** Fusion. e.g. [_X >< _Y] *)

(** Syntax of the type system. *)

type ty_graph = string graph
(** type graph (RHS of production rules). *)

type ty_rule = var * ty_graph
type ty = ty_graph * var * ty_rule list

(** Syntax of the language. *)

type atom_name =
  | PConstr of string  (** constructor name *)
  | PInt of int  (** integer literal *)
  | PLam of var * exp  (** lambda abstraction *)

(** expression *)
and exp =
  | BinOp of (int -> int -> int) * string * exp * exp  (** Binary operator *)
  | RelOp of (int -> int -> bool) * string * exp * exp  (** Binary operator *)
  | Graph of atom_name graph  (** Graph *)
  | Case of exp * atom_name graph * exp * exp  (** Case expression *)
  | App of exp * exp  (** Apply *)
  | LetRec of var * var * exp * exp  (** let rec f x = e1 in e2 *)
  | Let of var * exp * exp  (** let x = e1 in e2 *)

let make_lambda (_, xs) var exp = Graph (Atom (PLam (var, exp), xs))
