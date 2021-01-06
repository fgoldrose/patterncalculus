structure Eval : sig

  val eval : AST.term -> AST.term

end = struct

  fun err info = raise Fail ("runtime error: " ^ info)

  fun union x = Map.unionWith (fn (v1, v2) => v2) x

  fun match x y bindings =
        ((*print("match: " ^ AST.tos x ^ " ; " ^AST.tos y ^ "\n");*)
          case (x, y) of
          (AST.App(x1, x2), AST.App (y1, y2)) =>
            (case (match x1 y1 bindings) of
              SOME map1 =>
                (case (match x2 y2 bindings) of
                  SOME map2 =>
                    if Map.existsi (fn (i,n) => Map.inDomain(map1, i)) map2
                    then NONE
                    else SOME (union (map1, map2))
                    
                  | _ => NONE
                )
              | _ => NONE
            )
          | (v, AST.Var sy) =>
            (case Map.find (bindings, sy) of
                          NONE =>  (SOME (Map.singleton (sy, v)))
                          | SOME b => if v = b then 
                                    SOME Map.empty
                                    else NONE)
          | (AST.Var sx, v) =>
            (case Map.find (bindings, sx) of
                NONE => NONE
                |SOME b => match b v bindings)
          | _ => NONE
        )

  and doEval t bindings =
    ((*print("eval: " ^ AST.tos t ^ "\n");
      print(("bindings: " ^ concat (Map.listKeys bindings) )^ "\n");*)
      case t of 
      AST.App (t1, t2) =>
        (case doEval t1 bindings of
          AST.Case(left, right) => 
            (case match (doEval t2 bindings) left bindings of
                NONE => AST.Case (AST.Var " x", AST.Var " x")
              | SOME b => doEval right (union (b, bindings))
            )
          | t1' => AST.App(t1', doEval t2 bindings)
        )
      | AST.Case (t1, t2) => AST.Case(doEval t1 bindings, t2)
      | AST.Var x =>
        case Map.find (bindings, x) of
            NONE => AST.Var x
          | SOME v => v
    )

  fun eval t =
     doEval t Map.empty
     
      
end
