structure Eval : sig

  val eval : AST.term -> AST.term
  val step : AST.term -> AST.term option
  val match: (AST.term * AST.term) -> bool
end = struct

  fun err info = raise Fail ("runtime error: " ^ info)  

  fun followpath p t =
    (case (p, t) of
      ([], _) => t
      | (AST.Left :: p', AST.App(l, _)) => followpath p' l
      | (AST.Right :: p', AST.App(_, r)) => followpath p' r
      | (_, AST.Or(t1, t2)) => AST.Or(followpath p t1, followpath p t2)
      | _ => AST.None)

  fun opening u t =
    let
      fun sub k u t = (case t of
        AST.Bound(i,p) => if i = k then followpath p u else t
      | AST.App(t1,t2) => AST.App(sub k u t1, sub k u t2)
      | AST.Or(t1,t2) => AST.Or(sub k u t1, sub k u t2)
      | AST.Case(l,r) => AST.Case(sub k u l, sub (k+1) u r)
      | _ => t
    )
    in
      sub 1 u t
    end
    

  fun match (l, a) =
    let
      val v = (case (l, a) of
          (AST.Wildcard, _) => true
        | (_, AST.Wildcard) => true
        | (AST.Free x, AST.Free y) => x = y
        | (AST.App(l1, l2), AST.App(a1, a2)) => match(l1, a1) andalso match(l2, a2)
        | (AST.Or(t1, t2), t) => match(t1, t) orelse match(t2, t)
        | (t, AST.Or(t1, t2)) => match(t, t1) orelse match(t, t2)
        | _ => false
      )
    in
      (*print ("match " ^AST.tos l ^ AST.tos a ^ (if v then " true\n" else " false\n"));*)
      v
    end

  (*step when in left side of case.
    Should not reduce right sides of case
    or reduce or, so that it can be properly matched later.*)
  and casestep t = 
    (case t of
      AST.Or (t1, t2) => 
        (case casestep t1 of
          SOME t1' => SOME (AST.Or(t1', t2))
          | NONE => NONE)
          
      | AST.Case (t1, t2) =>
        (case casestep t1 of
            SOME t1' => SOME (AST.Case(t1', t2))
            | NONE => NONE)
      | _ => step t)

  and step t =
    (case t of        
        AST.Free x => NONE
      | AST.Bound(i, p) => NONE
      | AST.Wildcard => NONE
      | AST.None => NONE
      | AST.Or (AST.None, t2) => SOME t2
      | AST.Or (t1, t2) => 
        (case step t1 of
          SOME t1' => SOME (AST.Or(t1', t2))
        | NONE => SOME t1)

      | AST.Case (t1, t2) => 
        (case casestep t1 of
          SOME t1' => SOME (AST.Case(t1', t2))
          | NONE => (case step t2 of
            SOME t2' => SOME (AST.Case(t1, t2'))
            | NONE => NONE))

      | AST.App(AST.None, _) => SOME AST.None
      | AST.App(_, AST.None) => SOME AST.None
      
      | AST.App(t1, t2) =>
          (case casestep t2 of
            SOME t2' => SOME (AST.App(t1, t2'))
            | NONE => 
              (case casestep t1 of
                SOME t1' => SOME (AST.App(t1', t2))
                | NONE => (case (t1, t2) of
                  (AST.Case(l,r), _) =>
                    if match(l, t2) 
                    then SOME (opening t2 r)
                    else SOME AST.None
                  | (AST.Or(o1, o2), _) => SOME (AST.Or(AST.App(o1, t2), AST.App(o2, t2)))
                  | (_, AST.Or(o1, o2)) => SOME (AST.Or(AST.App(t1, o1), AST.App(t1, o2)))
                  | (_, _) => NONE)
                  )))

  fun eval t =
   ( case step t of
         NONE => t
         | SOME t' => eval t')
      
end
