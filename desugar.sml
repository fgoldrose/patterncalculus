structure Desugar : sig
    
  val desugar : SAST.term -> AST.term

  val ds : string -> AST.term

end = struct

  structure S = SAST
  structure A = AST

    fun disjointunion(x, y) = 
      if Map.existsi (fn (i, _) => Map.inDomain(x, i)) y
      then raise Fail "variable reuse in pattern is not allowed (e.g. (x x)->x)"
      else Map.unionWith (fn (v1, v2) => v1) (x, y)


    

    fun incrlevel t = 
      (case t of
          A.Bound(l, p) => A.Bound(l+1, p)
        | A.Or (t1, t2) => A.Or (incrlevel t1, incrlevel t2)
        | _ => t)

    (*bind the term and return the new substitutions map as well.
      Called on the left side of case.*)
    fun getsubs t path subs =
      (case t of
        S.Var x => (case Map.find(subs, x) of
                        SOME v => (v, Map.empty)
                        | NONE => (A.Wildcard, Map.singleton(x, A.Bound (0, path)))
                      )   
      | S.App (t1, t2) =>
        let
          val (v1, s1) = getsubs t1 (path @ [A.Left]) subs
          val (v2, s2) = getsubs t2 (path @ [A.Right]) subs
        in
          (case t1 of
            S.Case _ => (A.App(v1, v2), Map.empty)
            | _ => (A.App(v1, v2), disjointunion(s1, s2))
            )
        end
      | S.Case (t1, t2) =>
        let
          val incrsubs = Map.map incrlevel subs
          val (leftbound, newsubs) = getsubs t1 [] subs
          val (rightbound,_ ) = getsubs t2 [](disjointunion (incrsubs, newsubs))
          val (rightbound, rsubs) = getsubs t2 (path @ [A.Right]) (disjointunion (incrsubs, newsubs))
          val (_, lsubs) = getsubs t1 (path @ [A.Left]) subs
 
          
        in
          (A.Case(leftbound, rightbound), disjointunion(lsubs, rsubs))
        end
      | S.Or (t1, t2) =>
        let
          val (b1, s1) = getsubs t1 path subs
          val (b2, s2) = getsubs t2 path subs
          val ormap = Map.intersectWith (fn (v1, v2) => A.Or(v1, v2)) (s1, s2)
        in
          (A.Or(b1, b2), ormap)
        end
      | S.Def (t1, t2) =>
       let
          val (v1, s1) = getsubs t1 path subs
          val (v2, s2) = getsubs t2 path subs
        in
          (v2, disjointunion(s1, s2))
        end
      | _ => (bindvars t subs, Map.empty)

      )


    and bindvars t subs =
      (case t of
          S.Var x => (case Map.find(subs, x) of
                        SOME v => v
                        | NONE => A.Free x
                      )
        | S.Wildcard => A.Wildcard
        | S.Or (t1, t2) => A.Or(bindvars t1 subs, bindvars t2 subs)

        | S.App (t1, t2) => A.App(bindvars t1 subs, bindvars t2 subs)
        | S.Case (t1, t2) => 
          let
            val incrsubs = Map.map incrlevel subs
            val (leftbound, newsubs) = getsubs t1 [] subs
            val rightbound = bindvars t2 (disjointunion (incrsubs, newsubs))
          in
          A.Case(leftbound, rightbound)
          end
        | S.Let (t1, t2, t3) => bindvars (S.App(S.Case(t1, t3),t2)) subs
        | S.Def (t1, t2) => bindvars t2 subs
      )

    fun desugar t =
      bindvars t Map.empty

    fun ds s = desugar (Parse.parse (Scan.scan s));


end



