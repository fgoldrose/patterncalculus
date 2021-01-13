structure AST = struct


  datatype direction = Left | Right

  datatype term
    = Free of string
    | Bound of int * (direction list)
    | App of term * term
    | Case of term * term
    | Wildcard
    | Or of term * term
    | None

  fun pathTos [] = ""
    | pathTos (Left :: p) = "L" ^ pathTos p 
    | pathTos (Right :: p) = "R" ^ pathTos p

  and tos (Free x) = x
    | tos (Bound (i, p)) = Int.toString i ^ "." ^ pathTos p
    | tos (App (t1, t2)) = "(" ^ tos t1 ^ " " ^ tos t2 ^ ")"
    | tos (Case (t1, t2)) =  "(" ^ tos t1 ^ "->" ^ tos t2 ^ ")"
    | tos Wildcard = "_"
    | tos (Or (t1, t2)) =  "(" ^ tos t1 ^ "|" ^ tos t2 ^ ")"
    | tos None = "NONE"
    
                  
                  
end