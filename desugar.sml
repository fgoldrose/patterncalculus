structure Desugar : sig
    
  val desugar : SAST.term -> AST.term

  

end = struct

  structure S = SAST
  structure A = AST

    fun desugar t =
      (case t of
          S.Var x => A.Var x
        | S.App (t1, t2) => A.App(desugar t1, desugar t2)
        | S.Case (t1, t2) => A.Case(desugar t1, desugar t2)
        | S.Or (t1, t2) => 
        let
          val t1' = desugar t1
          val t2' = desugar t2
        in
          A.Case(A.Var " x", A.App(A.App (A.Case (A.Var "y", A.Case(t1', t1')), t2'), A.Var " x"))
        end
      )

end



