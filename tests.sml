structure Tests = struct

  (* type Tests.run() in the SML repl *)

fun ce (x, y, m) =
    let 
      val v = Interpret.interpret x
      fun message a b = m ^ " FAILED: got " ^ (AST.tos a) ^ " but expected " ^ (AST.tos b) ^ "\n"
    in
    if v = y then (print(m ^ " succeeded\n")) else print(message v y)
    end

  fun or_tests() = (
    print ">>> testing or...\n";
    ce("(((a x)->x)|_->y) z", AST.Free "y", "ortest1");
    ce("((x->x)|_->y) z", AST.Free "z", "ortest2");
    ce("(((p x)|x)->x) z", AST.Free "z", "ortest4");
    ce("(((p x)|x)->x) (q z)", AST.Free "z", "ortest4");
    ce("((((p x)|x) r)->(x r)) (q z)", AST.App(AST.Free "q", AST.Free "z"), "ortest5");
    ce("((((p x)|x) r)->(x r)) (a q z)", AST.App(AST.Free "q", AST.Free "z"), "ortest6");
    ce("(r->(((a b)->b) r)) (q|(q z))", AST.Free("z"), "ortest7");
    ce("(r->((b->b) r)) (q|(q z))", AST.Free("q"), "ortest8");
    print "\n")

  fun def_tests() = (
    
    )

  fun run () =(
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
    ce("((x -> (((p -> (y -> s)) x) (r x))) d)", AST.Free "s", "test9");
    ce("((x -> (((a b) -> yes) x)) (q p))", AST.Free "yes", "test10");
    ce("(x->x y->y) z", AST.Case(AST.App(AST.Free "z", AST.Wildcard), AST.Bound(1, [AST.Right])), "test11");
    ce("((p z)-> ((a->(a->a) z) p)) (p p)", AST.Free "p", "test12");
    ce("((x->x) x -> y) z", AST.Free("y"), "test14");
    ce("(x->((x -> y) z)) i", AST.None, "test15");
    ce("(x->y a->(x y)) z", AST.Case(AST.App(AST.Wildcard, AST.Wildcard), AST.App(AST.Free "z", AST.Bound(1, [AST.Left]))), "test16");
    or_tests();
    print "<<< tests done.\n")
    

end



