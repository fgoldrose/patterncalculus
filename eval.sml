structure Eval : sig

  val eval : AST.term -> bool -> AST.term
  val ev : AST.term -> AST.term list -> bool -> AST.term 
  val match: (AST.term * AST.term) -> AST.term list -> bool -> bool
end = struct

  fun err info = raise Fail ("runtime error: " ^ info)  

  fun followpath p t =
    (case (p, t) of
      ([], _) => SOME t
      | (AST.Left :: p', AST.App(l, _)) => followpath p' l
      | (AST.Right :: p', AST.App(_, r)) => followpath p' r
      | (AST.Left :: p', AST.Case(l, _)) => followpath p' l
      | (AST.Right :: p', AST.Case(_, r)) => followpath p' r
      | (_, AST.Or(t1, t2)) => (case (followpath p t1) of
        SOME t1' => SOME t1'
        | NONE => followpath p t2)
       
      | _ => NONE)    

  fun getbound ls i p =
    (case ls of
      [] => NONE
      | x :: xs => if i = 0 then (followpath p x) else getbound xs (i - 1) p)

  fun sub t env k =
    (case t of 
      AST.Bound(i, p) => (case getbound env (i-k) p of
                              NONE => t
                              | SOME v => v)
      | AST.Case (t1, t2) => AST.Case (sub t1 env k, sub t2 env (k + 1))
      | AST.Or (t1, t2) => AST.Or (sub t1 env k, sub t2 env k)
      | AST.App (t1, t2) => AST.App (sub t1 env k, sub t2 env k)
      | _ => t
      )

  fun match (l, a) env debug =
    let
      val v = (case (l, a) of
        (AST.None, AST.None) => true
        | (AST.None, _) => false
        |  (AST.Wildcard, _) => true
        | (_, AST.None) => false
        | (_, AST.Wildcard) => true
        | (AST.Free x, AST.Free y) => x = y
        | (AST.Bound (i, p), _) => (case getbound env i p of
                                      NONE => false
                                      | SOME v => match(ev v env debug, a) env debug)
        | (_, AST.Bound (i, p)) => (case getbound env i p of
                                      NONE => false
                                      | SOME v => match(l, ev v env debug) env debug)
        | (AST.App(l1, l2), AST.App(a1, a2)) => match(l1, a1) env debug andalso match(l2, a2) env debug
        | (AST.Case(l1, l2), AST.Case(a1, a2)) => match(l1, a1) env debug andalso match(l2, a2) env debug
        | (AST.Or(t1, t2), t) => match(t1, t) env debug orelse match(t2, t) env debug
        | (t, AST.Or(t1, t2)) => match(t, t1) env debug orelse match(t, t2) env debug
        | _ => false
      )
    in
      if debug
      then (print ("match " ^AST.tos l ^ " with " ^ AST.tos a ^ (if v then " = true\n" else " = false\n"));
              v)
      else v
    end


  and ev t env debug =
      let
        val v = 
        (case t of        
        AST.Free x => AST.Free x
      | AST.Bound(i, p) => (case getbound env i p of
                              NONE => AST.None
                              | SOME v => ev v env debug)

      | AST.Wildcard => AST.Wildcard 
      | AST.None => AST.None
      | AST.Or (t1, t2) => 
        (case ev t1 env debug of
          AST.None => ev t2 env debug
          | t1' => AST.Or(t1', sub t2 env 0))

      | AST.Case (t1, t2) => AST.Case(ev t1 env debug, sub t2 env 1)

      | AST.App(t1, t2) =>
          (case (ev t1 env debug, ev t2 env debug) of
            (AST.Case(l,r), t2') =>
              if match(l, t2') env debug
              then ev r (t2' :: env) debug
              else AST.None 
            | (t1', AST.Or(o1, o2)) => ev (AST.Or((AST.App(t1', o1)), (AST.App(t1', o2)))) env debug 
            | (AST.Or(o1, o2), t2') => ev (AST.Or((AST.App(o1, t2')), (AST.App(o2, t2')))) env debug
            | (AST.None, _) => AST.None
            | (_, AST.None) => AST.None
            | (t1', t2') => AST.App(t1', t2')))
      
      in
        if debug 
        then (print("eval " ^ AST.tos t ^ " env: " ^concat (map AST.tos env) ^ " = " ^ AST.tos v ^ "\n");
                v)
      else v
      end
                 

  fun resolveor (AST.Or (t1, _)) = resolveor t1
    | resolveor t = t

  fun eval t debug = resolveor (ev t [] debug)


end
