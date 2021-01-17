structure SAST = struct

  datatype term
    = Var of string
    | App of term * term
    | Case of term * term
    | Wildcard
    | Or of term * term
    | Let of term * term * term
    | Def of term * term
    | None


  fun tos (Var x) = x
    | tos (App (t1, t2)) = "(" ^ tos t1 ^ " " ^ tos t2 ^ ")"
    | tos (Case (t1, t2)) =  "(" ^ tos t1 ^ "->" ^ tos t2 ^ ")"
    | tos (Or (t1, t2)) =  "(" ^ tos t1 ^ "|" ^ tos t2 ^ ")"
    | tos (Wildcard) = "_"
    | tos (Let (t1, t2, rest)) = "(" ^ tos t1 ^ ":" ^ tos t2 ^ "\n" ^ tos rest ^ ")"
    | tos (Def (t1, t2)) = "(" ^ tos t1 ^ ":" ^ tos t2 ^ ")"
    | tos None = "()"
end