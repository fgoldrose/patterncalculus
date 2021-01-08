structure AST = struct


  datatype term
    = Var of string
    | App of term * term
    | Case of term * term
    | Closure of term * term * (term Map.map)


  fun eq (tx, ty) =
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
    )

  fun mapTos m = concat (map (fn (k, v) => "(" ^ k ^ ", " ^ tos v ^ ")") (Map.listItemsi m))

  and tos (Var x) = x
    | tos (App (t1, t2)) = "(" ^ tos t1 ^ " " ^ tos t2 ^ ")"
    | tos (Case (t1, t2)) =  "(" ^ tos t1 ^ "->" ^ tos t2 ^ ")"
    | tos (Closure (t1, t2, m)) =  "(" ^ tos t1 ^ "->" ^ tos t2 ^ ")" ^ "[" ^ mapTos m ^ "]"
    
                  
                  
end