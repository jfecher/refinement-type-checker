type expr = IntLit of int
          | BoolLit of bool
          | Identifier of string
          | Lambda of string list * expr
          | FnCall of expr * expr list
          | Let of string * expr * expr
          | If of expr * expr * expr
          | Assert of expr * expr
