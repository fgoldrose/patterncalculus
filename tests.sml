structure Tests = struct

  (* type Tests.run() in the SML repl *)

fun ce (x, y, m) =
    let 
      val v = Interpret.interpret x
      fun message a b = m ^ " FAILED: got " ^ (AST.tos a) ^ " but expected " ^ (AST.tos b) ^ "\n"
    in
    if v = y then (print(m ^ " succeeded\n")) else print(message v y)
    end

  fun run () =
    let
      

    in
      print ">>> running tests...\n";
      ce ("((x->x) y)", AST.Free "y", "test0");
      ce ("((x-> (((b x)-> b) (c y))) y)", AST.Free "c", "test1");
      ce("(((a b)->a) (y z))", AST.Free "y", "test2");
      ce("((y -> (((a b)->a) (y x))) z)", AST.Free "z", "test3");
      ce("((y->(x->y)) a)", AST.Case(AST.Wildcard, AST.Free "a"), "test4");
      ce("((first-> (first (a b))) ((x y)->x))", AST.Free "a", "test5");
      ce("((a->((a y)->y)) (n->(f->(x->(f (n (f x)))))))", 
                Interpret.interpret "(((n->(f->(x->(f (n (f x)))))) y) -> y)", "test6");    
      ce("((x -> ((((a b) -> (y -> a)) x) ((a->a) x))) (q p))", AST.Free "q", "test7");
      ce("((x -> ((((a b) -> (y -> a)) x) ((a->a) x))) q)", AST.Free "q", "test8");
      ce("((x -> (((p -> (y -> s)) x) (r x))) d)", AST.Free "s", "test9");
      ce("((x -> (((a b) -> yes) x)) (q p))", AST.Free "yes", "test10");
      ce("(x->x y->y) z", AST.Case(AST.App(AST.Free "z", AST.Wildcard), AST.Bound(0, [AST.Right])), "test11");
      ce("((p z)-> ((a->(a->a) z) p)) (p p)", AST.Free "p", "test12");
      print "<<< tests done.\n"
    end

end



