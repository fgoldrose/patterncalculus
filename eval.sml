structure Eval : sig

  val eval : AST.term -> AST.term
  val step : AST.term -> AST.term option
  val match: (AST.term * AST.term) -> bool
end = struct

  fun err info = raise Fail ("runtime error: " ^ info)

  fun match (l, a) =
    (case (l, a) of
          (AST.Wildcard, _) => true
        | (AST.Free x, AST.Free y) => x = y
        | (AST.App(l1, l2), AST.App(a1, a2)) => match(l1, a1) andalso match(l2, a2)
        | (AST.Or(t1, t2), t) => match(t1, t) orelse match(t2, t) 
        | _ => false
      )


  fun followpath p t =
    (case p of
            [] => t
          | AST.Left :: p' => (case t of
                              AST.App(l, _) => followpath p' l
                              | _ => AST.None)
          | AST.Right :: p' => (case t of
                              AST.App(_, r) => followpath p' r
                              | _ => AST.None)
    )

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
      sub 0 u t
    end
    

  (*step when in left side of case.
    Should not reduce right sides of case
    or reduce or, so that it can be properly matched later.*)
  fun casestep t = 
    (case t of
      AST.Or (t1, t2) => NONE
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

      
      | AST.App(AST.Or(t1, t2), t3) => 
          SOME (AST.Or(AST.App(t1, t3), AST.App(t2, t3)))
      | AST.App(t1, AST.Or(t2, t3)) => 
          SOME (AST.Or(AST.App(t1, t2), AST.App(t1, t3)))

      | AST.App(AST.None, _) => SOME AST.None
      | AST.App(_, AST.None) => SOME AST.None
      
      | AST.App(t1, t2) =>
          (case (t1, step t2) of
              (_, SOME t2') => SOME (AST.App(t1, t2'))
            | (AST.Case(l,r), NONE) =>
                      (case casestep l of
                        SOME l' => SOME (AST.App( AST.Case(l',r), t2))
                        | NONE => 
                          if match(l, t2) 
                          then SOME (opening t2 r)
                          else SOME AST.None)
            | (_, NONE) =>
                (case step t1 of 
                    SOME t1' => SOME (AST.App(t1',t2))
                  | NONE => NONE)))

  fun eval t =
   ( case step t of
         NONE => t
         | SOME t' => eval t')
      
end
