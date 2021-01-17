structure Eval : sig

  val eval : AST.term -> bool -> AST.term
  val ev : AST.term -> bool -> AST.term 
  val match: (AST.term * AST.term) -> bool -> bool
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
      fun sub k u t = 
        (case t of
        AST.Bound(i,p) => if i = k then followpath p u else t
      | AST.App(t1,t2) => AST.App(sub k u t1, sub k u t2)
      | AST.Or(t1,t2) => AST.Or(sub k u t1, sub k u t2)
      | AST.Case(l,r) => AST.Case(sub k u l, sub (k+1) u r)
      | _ => t
    )
    in
      sub 1 u t
    end
    

  fun match (l, a) debug =
    let
      val v = (case (l, a) of
          (AST.Wildcard, _) => true
        | (_, AST.Wildcard) => true
        | (AST.Free x, AST.Free y) => x = y
        | (AST.App(l1, l2), AST.App(a1, a2)) => match(l1, a1) debug andalso match(l2, a2) debug
        | (AST.Or(t1, t2), t) => match(t1, t) debug orelse match(t2, t) debug
        | (t, AST.Or(t1, t2)) => match(t, t1) debug orelse match(t, t2) debug
        | _ => false
      )
    in
      if debug
      then (print ("match " ^AST.tos l ^ " with " ^ AST.tos a ^ (if v then " = true\n" else " = false\n"));
              v)
      else v
    end

  and ev t debug =
      let
        val v = 
        (case t of        
        AST.Free x => AST.Free x
      | AST.Bound(i, p) => AST.Bound(i, p) (*(case Map.find(env, i) of
                              NONE => t
                              | SOME v => followpath p v)*)

      | AST.Wildcard => AST.Wildcard 
      | AST.None => AST.None
      | AST.Or (t1, t2) => 
        (case ev t1 debug of
          AST.None => ev t2 debug
          | t1' => AST.Or(t1', t2))

      | AST.Case (t1, t2) => (AST.Case(ev t1 debug, t2))

      | AST.App(t1, t2) =>
          (case (ev t1 debug, ev t2 debug) of
            (AST.Case(l,r), t2') =>
              if match(l, t2') debug
              then ev (opening t2' r) debug
              else AST.None
            | (AST.None, _) => AST.None
            | (_, AST.None) => AST.None
            | (t1', AST.Or(o1, o2)) => ev (AST.Or((AST.App(t1', o1)), (AST.App(t1', o2)))) debug 
            | (AST.Or(o1, o2), t2') => ev (AST.Or((AST.App(o1, t2')), (AST.App(o2, t2')))) debug
            | (t1', t2') => AST.App(t1', t2')))
      
      in
        if debug 
        then (print("eval " ^ AST.tos t ^ " = " ^ AST.tos v ^ "\n");
                v)
      else v
      end
                 

  fun resolveor (AST.Or (t1, _)) = resolveor t1
    | resolveor t = t

  fun eval t debug = resolveor (ev t debug)


end
