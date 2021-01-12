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
        | _ => false
      )


  fun followpath p t =
    case p of
        [] => t
      | AST.Left :: p' => (case t of
                          AST.App(l, _) => followpath p' l
                          | _ => err "path not valid")
      | AST.Right :: p' => (case t of
                          AST.App(_, r) => followpath p' r
                          | _ => err "path not valid")


  fun opening u t =
    let
      fun sub k u t = (case t of
        AST.Bound(i,p) => if i = k then followpath p u else t
      | AST.App(t1,t2) => AST.App(sub k u t1, sub k u t2)
      | AST.Case(l,r) => AST.Case(sub k u l, sub (k+1) u r)
      | _ => t
    )
    in
      sub 0 u t
    end
    

  fun step t =
    (case t of        
        AST.Free x => NONE
      | AST.Bound(i, p) => NONE
      | AST.Wildcard => NONE
      | AST.Case (t1, t2) => 
      (case step t1 of
        SOME t1' => SOME (AST.Case(t1', t2))
        | NONE => (case step t2 of
          SOME t2' => SOME (AST.Case(t1, t2'))
          | NONE => NONE))

      | AST.App(t1, t2) =>
          (case step t2 of
            SOME t2' => SOME (AST.App(t1, t2'))
            | NONE => 
                (case t1 of
                  AST.Case(l,r) => 
                      (case step l of
                        SOME l' => SOME (AST.App( AST.Case(l',r), t2))
                        | NONE => 
                          if match(l, t2) 
                          then SOME (opening t2 r)
                          else SOME (AST.Case(AST.Wildcard, AST.Bound(0, []))))
                  | _ => (case step t1 of
                            SOME t1' => SOME (AST.App(t1',t2))
                          | NONE => NONE ))))

  fun eval t =
   ( case step t of
         NONE => t
         | SOME t' => eval t')
      
end
