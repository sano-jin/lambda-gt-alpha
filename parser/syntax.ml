(** Syntax *)

type var = string * string list
(** Type variable. *)

(** Graph template *)
type 'atom_name graph =
  | Zero  (** Null graph. *)
  | Atom of 'atom_name * string list  (** Atom. E.g., [C(_X, _Y, _Z)]. *)
  | Var of var  (** Graph context. E.g., [x\[_X, _Y\]]. *)
  | Mol of 'atom_name graph * 'atom_name graph  (** Molecule. *)
  | Nu of string * 'atom_name graph
      (** Hyperlink name hiding. E.g., [nu _X. G]. *)
  | Fuse of string * string  (** Fusion. E.g., [_X >< _Y]. *)

(** Syntax of the type system. *)

type ty_graph = string graph
(** Type graph (RHS of production rules). The names of atoms in a type graph is
    restricted to be strings for this time. We are going to add arrow
    (functions) later. *)

type ty_rule = var * ty_graph
(** Production rule. E.g., [x\[...\] -> G]. *)

type ty = ty_graph * var * ty_rule list
(** A typing relation: expression, type, and production rules. Only type
    variable is allowed as an expression for this time and only graph is allowed
    as an type annotation for this time. *)

(** Syntax of the language. *)

(** The name of atoms in the Lambda GT language. *)
type atom_name =
  | PConstr of string  (** constructor name *)
  | PInt of int  (** integer literal *)
  | PLam of var * exp  (** lambda abstraction *)

(** The expression of the Lambda GT language. *)
and exp =
  | BinOp of (int -> int -> int) * string * exp * exp  (** Binary operator *)
  | RelOp of (int -> int -> bool) * string * exp * exp  (** Binary operator *)
  | Graph of atom_name graph  (** Graph *)
  | Case of exp * atom_name graph * exp * exp  (** Case expression *)
  | App of exp * exp  (** Apply *)
  | LetRec of var * var * exp * exp  (** let rec f x = e1 in e2 *)
  | Let of var * exp * exp  (** let x = e1 in e2 *)

(** The expression of the Lambda GT language. *)
let make_lambda (_, xs) var exp = Graph (Atom (PLam (var, exp), xs))
