structure Eval : sig

  val eval : AST.term -> AST.term

end = struct

  structure Map = RedBlackMapFn (struct
    type ord_key = string
    val compare = String.compare
  end)


  fun err info = raise Fail ("runtime error: " ^ info)
  
  fun subst m t =
        (case t of
          AST.Var s => 
            (case Map.find (m, s) of 
                NONE => AST.Var s
              | SOME (v, path) => v 
            )
          | AST.App (t1, t2) => AST.App (subst m t1, subst m t2)
          | AST.Case (t1, t2) => AST.Case (subst m t1, subst m t2) 
                    (* doesnt avoid variable clash! *)

        )
  fun match x y path =
        (case (x, y) of
          (AST.App(x1, x2), AST.App (y1, y2)) =>

            (case (match x1 y1 " L") of
              SOME map1 =>
                (case (match x2 y2 " R") of
                  SOME map2 =>
                    if Map.existsi (fn (i,n) => Map.inDomain(map1, i)) map2
                    then NONE
                    else SOME (Map.unionWith (fn (v1, v2) => v1) (map1, map2))
                    
                  | _ => NONE
                )
              | _ => NONE
            )
          | (v, AST.Var sy) =>
            if v = (AST.Var sy) then
              SOME Map.empty
            else
              (print(sy^ (AST.tos v)) ;
                            SOME (Map.singleton (sy, (v, path))))
          | _ => NONE
        )


  fun step t =
    (case t of
      AST.App (t1, t2) =>
        (case (step t1) of
            SOME t1' => SOME (AST.App(t1', t2))
          | NONE =>
           (case (step t2) of
              SOME t2' => SOME (AST.App(t1, t2'))
            | NONE =>
              (case t1 of
                AST.Case (left, right) => 
                (case match t2 left "" of
                    NONE => SOME (AST.Case (AST.Var " x", AST.Var " x"))
                  | SOME map => SOME (subst map right))
                | _ => NONE)
            )
        )
      | AST.Case (t1, t2) =>
        (case (step t1) of
            SOME t1' => SOME (AST.Case (t1', t2))
          | NONE => NONE
        ) 
      | _ => NONE
    )

  fun eval t =
    let
      fun lp t =
        (case step t of
           SOME t' => lp t'
         | NONE => t)
    in
      lp t
    end
     
      
end
