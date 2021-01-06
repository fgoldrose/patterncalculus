structure Tests = struct

  (* type Tests.run() in the SML repl *)

  fun ce (a, b, m) =
    Check.expect(AST.tos, Interpret.interpret a, b, m)
  fun run () =
    let
      val _ = TextIO.print ">>> running tests..."
      val test0 = ce ("((x->x) y)", AST.Var "y", "test0")
      val test1 = ce ("((x-> (((b x)-> b) (c y))) y)", AST.Var "c", "test1")
      val test2 = ce("(((a b)->a) (y z))", AST.Var "y", "test2")
      val test3 = ce("(((((a b)->a) (y z)) -> a) q)", AST.Var "q", "test3")
    in
      TextIO.print "<<< tests done.\n"
    end

end
