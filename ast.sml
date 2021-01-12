structure AST = struct


  datatype direction = Left | Right

  datatype term
    = Free of string
    | Bound of int * (direction list)
    | App of term * term
    | Case of term * term
    | Wildcard

  (*fun eq (tx, ty) =
    (case (tx, ty) of
        (Var x, Var y) => x = y
      | (Case (tx1, tx2), Case (ty1, ty2)) => 
          eq(tx1, ty1) andalso eq(tx2, ty2)
      | (App (tx1, tx2), App (ty1, ty2)) => 
          eq(tx1, ty1) andalso eq(tx2, ty2)
      | (Closure (tx1, tx2, mx), Closure (ty1, ty2, my)) => 
          eq(tx1, ty1) andalso eq(tx2, ty2) andalso 
            (Map.alli (fn (k, v) => (case Map.find (mx, k) of
                            NONE => false
                          | SOME t => eq(t, v)
                          )
                        ) my)
      | _ => false
    )*)

  fun pathTos [] = ""
    | pathTos (Left :: p) = "L" ^ pathTos p 
    | pathTos (Right :: p) = "R" ^ pathTos p

  and tos (Free x) = x
    | tos (Bound (i, p)) = Int.toString i ^ "." ^ pathTos p
    | tos (App (t1, t2)) = "(" ^ tos t1 ^ " " ^ tos t2 ^ ")"
    | tos (Case (t1, t2)) =  "(" ^ tos t1 ^ "->" ^ tos t2 ^ ")"
    | tos Wildcard = "_"
    
                  
                  
end