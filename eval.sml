structure Eval : sig

  val eval : AST.term -> AST.term
  val ev : AST.term * AST.term list -> AST.term
  val match: (AST.term * AST.term) -> bool
end = struct

  fun err info = raise Fail ("runtime error: " ^ info)

  fun union (x, y) = Map.unionWith (fn (v1, v2) => v2) (x, y)


  fun match (l, a) =
    (case (l, a) of
          (AST.Wildcard, _) => true
        | (AST.Free x, AST.Free y) => x = y
        | (AST.App(l1, l2), AST.App(a1, a2)) => match(l1, a1) andalso match(l2, a2)
        | _ => false
      )


  fun followpath p t =
    case p of
        [] => t
      | AST.Left :: p' => (case t of
                          AST.App(l, _) => followpath p' l
                          | _ => err "path does not exist in term")
      | AST.Right :: p' => (case t of
                          AST.App(_, r) => followpath p' r
                          | _ => err "path does not exist in term")

  fun getval env i p = 
    case env of
      [] => err "bound variable exists without a valid binding"
      | t :: xs => if i = 0 then followpath p t else getval xs (i-1) p


  fun ev (t, env) =
    (case t of        
        AST.Free x => AST.Free x
      | AST.Bound(i, p) => getval env i p
      | AST.Wildcard => AST.Wildcard
      | AST.Case (t1, t2) => AST.Case(ev (t1, env), t2)
      | AST.App(t1, t2) =>
          let
            val t1' = ev(t1,env)

            val t2' = ev(t2, env)
          in
          (case t1' of
            AST.Case(left, right) => 
                          if match(left, t2')
                          then ev(right, t2' :: env)
                          else AST.Case(AST.Wildcard, AST.Bound(0, []))
          
            | _ => AST.App(t1', t2')
          )
          end

             
    )

  fun eval t =
     ev (t, [])
      
end
