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
      val test3 = ce("((y -> (((a b)->a) (y x))) z)", AST.Var "z", "test3")
      val test4 = ce("((y->(x->y)) a)", AST.Case(AST.Var "x", AST.Var "a"), "test4")
      val test5 = ce("((first-> (first (a b))) ((x y)->x))", AST.Var "a", "test5")
      val test6 = ce("((a->((a y)->y)) (n->(f->(x->(f (n (f x)))))))", 
                Interpret.interpret "(((n->(f->(x->(f (n (f x)))))) y) -> y)", "test6")      
      val test7 = ce("((x -> ((((a b) -> (y -> a)) x) ((a->a) x))) (q p))", AST.Var "q", "test7")
      val test8 = ce("((x -> ((((a b) -> (y -> a)) x) ((a->a) x))) q)", AST.Var "q", "test8")
      val test9 = ce("((x -> (((p -> (y -> s)) x) (r x))) d)", AST.Var "s", "test9")
      val test10 = ce("((x -> (((a b) -> yes) x)) (q p))", AST.Var "yes", "test10")
      val _ = ce("(x->x y->y) z", AST.Case(AST.App(AST.Var "z", AST.Var "y"), AST.Var "y"), "test11")
    in
      TextIO.print "<<< tests done.\n"
    end

end