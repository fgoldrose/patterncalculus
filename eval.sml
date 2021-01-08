structure Eval : sig

  val eval : AST.term -> AST.term
  val ev : AST.term * AST.term Map.map -> AST.term
  val match: (AST.term * AST.term * (AST.term Map.map)) -> (AST.term Map.map) option

end = struct

  fun err info = raise Fail ("runtime error: " ^ info)

  fun union (x, y) = Map.unionWith (fn (v1, v2) => v2) (x, y)


  fun match (l:AST.term, a:AST.term, env): (AST.term Map.map) option =
    (case (l, a) of 
        (AST.Var x, t) => (case Map.find(env, x) of
                                NONE => SOME (Map.singleton (x, t))
                              | SOME t' => if AST.eq(t,t') 
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
              NONE => (AST.Var x)
            | SOME v => v
          )

      | AST.Case (t1, t2) => AST.Closure(ev(t1, env), t2, env)

      | AST.Closure(t1, t2, m) => AST.Case(t1, ev(t2, m))

      | AST.App(t1, t2) =>
          let
            val t1' = ev(t1, env)
            val t2' = ev(t2, env)
          in
          case t1' of
            AST.Closure(left, right, env') =>
                      (case match(left, t2', env') of
                          SOME m => ev(right, union (env', m))
                        | NONE => AST.Case(AST.Var "x", AST.Var "x")
                      )
            | _ => AST.App(t1', t2')
          end
             
    )

  fun eval t =
     ev(ev (t, Map.empty), Map.empty)
      
end
