structure Eval : sig

  val eval : AST.term -> AST.term

end = struct

  fun err info = raise Fail ("runtime error: " ^ info)

  fun union (x, y) = Map.unionWith (fn (v1, v2) => v2) (x, y)

  fun match (l, a, env): (AST.term Map.map) option =
    (case (l, a) of 
        (AST.Var x, t) => (case Map.find(env, x) of
                                NONE => SOME (Map.singleton (x, t))
                              | SOME t' => if t = t' 
                                          then SOME Map.empty
                                          else NONE
                          )
        | (AST.App(l1, l2), AST.App(a1, a2)) =>
            (case (match (l1, a1, env)) of
                NONE => NONE
              | SOME map1 => 
                (case (match (l2, a2, env)) of
                  NONE => NONE
                | SOME map2 => 
                    if Map.existsi (fn (i, _) => Map.inDomain(map1, i)) map2
                    then NONE
                    else SOME (union (map1, map2))
                )
              )
        | _ => NONE
      )

  fun ev (t, env) =
    (case t of
        AST.Var x =>
          (case Map.find (env, x) of
              NONE => SOME ((AST.Var x), Map.empty)
            | SOME v => SOME (v, Map.empty)
          )
      
      | AST.Case (t1, t2) => 
          (case ev(t1, env) of
            SOME (t1', _) =>
              if Map.isEmpty env 
              then SOME (AST.Case(t1', t2), Map.empty)
              else (case ev(t2, env) of
                        SOME (t2', _) => 
                            SOME (AST.Case(t1', t2'), Map.empty)
                      | NONE => NONE
                    )
          | NONE => NONE)

      | AST.App(t1, t2) =>
          (case ev(t2, env) of
            NONE => NONE
          | SOME (t2', _) => 
            (case ev(t1, env) of
                SOME (AST.Case(left, right), env') =>
                      (case match(left, t2', env') of
                          SOME m => ev(right, union (env', m))
                        | NONE => SOME(AST.Case(AST.Var "x", AST.Var "x"), Map.empty)
                      )
              | SOME (t1', env') => SOME (AST.App(t1', t2'), Map.empty)
              | NONE => NONE
            )
          )
    )

  (*fun match x y bindings =
        (
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
        )*)

  

  (*and doEval t bindings =
    (
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
    )*)

  fun eval t =
     (case ev (t, Map.empty) of
           NONE => t
           | SOME (t', _) => t')
     
      
end
