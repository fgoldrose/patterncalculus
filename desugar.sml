structure Desugar : sig
    
  val desugar : SAST.term -> AST.term

  val ds : string -> AST.term

end = struct

  structure S = SAST
  structure A = AST

    fun disjointunion(x, y) = 
      if Map.existsi (fn (i, _) => Map.inDomain(x, i)) y
      then Map.empty
      else Map.unionWith (fn (v1, v2) => v2) (x, y)


    fun getsubs t path =
      (case t of
        A.Free x => Map.singleton(x, (path,0))
      | A.App (t1, t2) =>  
          disjointunion 
          (getsubs t1 (path @ [A.Left]), getsubs t2 (path @ [A.Right]))
      | _ => Map.empty

      )

    fun bindvars t subs wild =
      (case t of
          S.Var x => (case Map.find(subs, x) of
                        SOME (p, l) => A.Bound(l, p)
                        | NONE => if wild then A.Wildcard else A.Free x
                      )

        | S.App (t1, t2) => A.App(bindvars t1 subs wild, bindvars t2 subs wild)
        | S.Case (t1, t2) => 
          A.Case(bindvars t1 subs true, 
            bindvars t2 (disjointunion(
                          Map.map (fn (p, l) => (p, l+ 1)) subs,
                          getsubs (bindvars t1 subs false) [])) wild)
          
      )

    fun desugar t =
      bindvars t Map.empty false

    fun ds s = desugar (Parse.parse (Scan.scan s));


end



