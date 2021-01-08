structure Tests = struct

  (* type Tests.run() in the SML repl *)

fun ce (x, y, m) =
    let 
      val v = Interpret.interpret x
      fun message a b = m ^ " FAILED: got " ^ (AST.tos a) ^ " but expected " ^ (AST.tos b) ^ "\n"
    in
    if AST.eq(v,y) then (print(m ^ " succeeded\n")) else print(message v y)
    end

  fun run () =
    let
      

    in
      print ">>> running tests...\n";
      ce ("((x->x) y)", AST.Var "y", "test0");
      ce ("((x-> (((b x)-> b) (c y))) y)", AST.Var "c", "test1");
      ce("(((a b)->a) (y z))", AST.Var "y", "test2");
      ce("((y -> (((a b)->a) (y x))) z)", AST.Var "z", "test3");
      ce("((y->(x->y)) a)", AST.Case(AST.Var "x", AST.Var "a"), "test4");
      ce("((first-> (first (a b))) ((x y)->x))", AST.Var "a", "test5");
      ce("((a->((a y)->y)) (n->(f->(x->(f (n (f x)))))))", 
                Interpret.interpret "(((n->(f->(x->(f (n (f x)))))) y) -> y)", "test6");    
      ce("((x -> ((((a b) -> (y -> a)) x) ((a->a) x))) (q p))", AST.Var "q", "test7");
      ce("((x -> ((((a b) -> (y -> a)) x) ((a->a) x))) q)", AST.Var "q", "test8");
      ce("((x -> (((p -> (y -> s)) x) (r x))) d)", AST.Var "s", "test9");
      ce("((x -> (((a b) -> yes) x)) (q p))", AST.Var "yes", "test10");
      ce("(x->x y->y) z", AST.Case(AST.App(AST.Var "z", AST.Var "y"), AST.Var "y"), "test11");
      ce("((p z)-> ((a->(a->a) z) p)) (p p)", AST.Var "p", "test12");
      print "<<< tests done.\n"
    end

end



