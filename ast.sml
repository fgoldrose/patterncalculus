structure AST = struct


  datatype direction = Left | Right | Field of string

  datatype term
    = Free of string
    | Bound of int * (direction list)
    | App of term * term
    | Case of term * term
    | Wildcard
    | Or of term * term
    | None
    | Record of (string * term) list

  (*fun eq (Record t1, Record t2) = 
    | eq (t1, t2) = (t1 = t2)*)

  fun pathTos [] = ""
    | pathTos (Left :: p) = "L" ^ pathTos p 
    | pathTos (Right :: p) = "R" ^ pathTos p
    | pathTos (Field s :: p) = s ^ pathTos p

  and tos (Free x) = x
    | tos (Bound (i, p)) = Int.toString i ^ "." ^ pathTos p
    | tos (App (t1, t2)) = "(" ^ tos t1 ^ " " ^ tos t2 ^ ")"
    | tos (Case (t1, t2)) =  "(" ^ tos t1 ^ "->" ^ tos t2 ^ ")"
    | tos Wildcard = "_"
    | tos (Or (t1, t2)) =  "(" ^ tos t1 ^ "|" ^ tos t2 ^ ")"
    | tos None = "NONE"
    | tos (Record t) = "{" ^ concat (map (fn (s, x) => s ^ "= " ^ tos x ^ ",") t) (*(Map.listItemsi t))*) ^ "}"
                               
end