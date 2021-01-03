structure AST = struct

  datatype term
    = Var of string
    | App of term * term
    | Case of term * term
    

  fun tos (Var x) = x
    | tos (App (t1, t2)) = "(" ^ tos t1 ^ " " ^ tos t2 ^ ")"
    
    | tos (Case (t1, t2)) =  tos t1 ^ "->" ^ tos t2
                  
                  
end