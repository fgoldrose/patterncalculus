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
      | S.Record (r) =>
        let
          val boundr = map (fn (s, x) => 
                              (case getsubs x (path @ [A.Field s]) subs of
                                (b, _) => (s, b))) r
        in
          (A.Record boundr, Map.empty)
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
        | S.None => A.None
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
        | S.Record ls => 
          A.Record(Map.listItemsi 
                  (foldl 
                    (fn ((s, x), m) => 
                      if Map.inDomain (m,s) then raise Fail "record may not have two identical keys"
                      else Map.insert (m, s, bindvars x subs))
                     Map.empty ls))
        | S.Access (t, f) => (case bindvars t subs of
            A.Bound (i,p) => A.Bound(i, p @ [AST.Field f])
            | t' => A.App (A.Case(A.Wildcard, A.Bound (0, [AST.Field f])), t')
          ))

      
  fun contains outer inner  = 
      (if outer = inner then true
      else (case outer of
           S.Let(t1, t2, t3) => contains t3 inner
          | S.App(t1,t2) => contains t1 inner orelse contains t2 inner
          | S.Case(t1,t2) => contains t1 inner orelse contains t2 inner
          | S.Or(t1,t2) => contains t1 inner orelse contains t2 inner
          | _ => false)
        )

    fun subst t v b =
      (if t = v then b
      else (case t of
          S.Let(t1, t2, t3) =>  S.Let(subst t1 v b, subst t2 v b, subst t3 v b)
          | S.App(t1,t2) => S.App(subst t1 v b, subst t2 v b)
          | S.Case(t1,t2) => S.Case (subst t1 v b, subst t2 v b)
          | S.Or(t1,t2) => S.Or (subst t1 v b, subst t2 v b)
          | _ => t)
        )


      (*find recursive let and defs and desugar them*)
    fun desugarrec t =
      let
        fun recursive f = S.App(S.Case(S.Var " r", S.App(S.Var " r", S.Var " r")), f)
      in
        
        (case t of
          S.Let (S.Var x, S.Var y, t3) => S.Let(S.Var x, S.Var y, desugarrec t3)
          | S.Let (S.Var x, t2, t3) =>
            if contains t2 (S.Var x) then
              (S.Let(S.Var x, 
                    recursive(S.Case(S.Var x, subst t2 (S.Var x) (recursive (S.Var x)))), 
                    desugarrec t3))
            else S.Let (S.Var x, t2, desugarrec t3)
          | S.Let(t1, t2, t3) => S.Let(t1, t2, desugarrec t3)
          | S.Def(S.Var x, t2) => 
              if contains t2 (S.Var x) 
              then S.Def(S.Var x, recursive(S.Case(S.Var x, subst t2 (S.Var x) (recursive (S.Var x)))))
              else S.Def(S.Var x, t2)
          | S.App(t1,t2) => S.App (desugarrec t1, desugarrec t2)
          | S.Case(t1,t2) => S.Case (desugarrec t1, desugarrec t2)
          | S.Or(t1,t2) => S.Or (desugarrec t1, desugarrec t2)
          | _ => t)
      end
      

    fun desugar t =
      bindvars (desugarrec t) Map.empty

    fun ds s = desugar (Parse.parse (Scan.scan s));


end



