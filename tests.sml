structure Tests = struct

  (* type Tests.run() in the SML repl *)

    fun expect (v, y, m) =
        let 
          fun message a b = m ^ " FAILED: got " ^ (AST.tos a) ^ " but expected " ^ (AST.tos b) ^ "\n"
        in
        if v = y then (print(m ^ " succeeded\n")) else print(message v y)
        end

    fun ce (x, y, m) =
        expect(Interpret.interpret x, y, m)


  
  fun or_tests() = (
    print ">>> testing or...\n";
    ce("(((a x)->x)|_->y) z", AST.Free "y", "ortest1");
    ce("((x->x)|_->y) z", AST.Free "z", "ortest2");
    ce("(((p x)|x)->x) z", AST.Free "z", "ortest3");
    ce("(((p x)|x)->x) (q z)", AST.Free "z", "ortest4");
    ce("((((p x)|x) r)->(x r)) (q z)", AST.App(AST.Free "q", AST.Free "z"), "ortest5");
    ce("((((p x)|x) r)->(x r)) (a q z)", AST.App(AST.Free "q", AST.Free "z"), "ortest6");
    ce("(r->(((a b)->b) r)) (q|(q z))", AST.Free("z"), "ortest7");
    ce("(r->((b->b) r)) (q|(q z))", AST.Free("q"), "ortest8");
    ce("(((p x)|x)->x) (z|(a b))", AST.Free("z"), "ortest9");
    ce("x:x y:y ((z:(x|y)->z) x)", AST.Free("x"), "ortest10");
    ce("x:x y:y ((z:(x|y)->z) y)", AST.Free("y"), "ortest11");
    ce("x:x y:y ((z:(x|y)->z) a)", AST.None, "ortest12");
    ce("x:X y:Y (((z:(x|y)->z) | _->b) a)", AST.Free "b", "ortest13");
    print "\n")

  fun def_tests() = (

    )

  (*Not sure what these should actually return but im leaving them to just
  see how the program manages them as the code changes.*)
  fun casebind_tests() = (
    print ">>> testing case bind...\n";
    ce("z:p ((x->x)->x) ((y:z->y))", AST.Free "p", "cbtest1");
    ce("y:p ((x->y) z -> z) ((y->y) a)", AST.Free("a"), "cbtest2");
    ce("((x->x) z -> z) ((y->y) a)", AST.Free("a"), "cbtest3");
    ce("z:q ((x->x) z -> z) ((y->z) a)", AST.None, "cbtest4");
    print "\n")

  fun recursion_tests() = (
        print ">>> testing recursion...\n";
        expect(Interpret.file "numbers.txt", AST.App(AST.App(AST.Free "1", AST.Free "1"), AST.Free "0"), "numbers");
        expect(Interpret.file "length.txt", AST.App(AST.Free "s",AST.App( AST.Free "s", AST.Free "o")), "length"); 
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
    ce("((x -> (((p -> (y -> s)) x) (r x))) d)", AST.Free "s", "test8");
    ce("((x -> (((a b) -> yes) x)) (q p))", AST.Free "yes", "test9");
    ce("(x->x y->y) z", AST.Case(AST.App(AST.Free "z", AST.Wildcard), AST.Bound(1, [AST.Right])), "test10");
    ce("((p z)-> ((a->(a->a) z) p)) (p p)", AST.Free "p", "test11");
    ce("(x->((x -> y) z)) i", AST.None, "test12");
    ce("(x->y a->(x y)) z", AST.Case(AST.App(AST.Wildcard, AST.Wildcard), AST.App(AST.Free "z", AST.Bound(1, [AST.Left]))), "test13");
    ce("((x y)->y) ((_->(a b)) e)", AST.Free "b", "test14");
    or_tests();
    casebind_tests();
    recursion_tests();
    print "<<< tests done.\n")
    

end






