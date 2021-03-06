structure Parse : sig

  val parse : Token.token list -> SAST.term

end = struct

  structure T = Token
  structure SAST = SAST

  fun err info = raise Fail ("parse error " ^ info)
            
  fun toksStr toks = "[" ^ String.concatWith "," (List.map T.tos toks) ^ "]"
            
  fun nextTerm toks =
    let
      fun parserecord ts =
        (case ts of
          T.RBrace :: ts' => ([], ts')
          | T.Var x :: T.Equals :: ts' => 
            (case nextTerm ts' of 
              (t, T.Comma :: rest) => 
                (case parserecord rest of
                  (ls, r) => ((x, t) :: ls, r))
              | (t, T.RBrace :: rest) => ([(x, t)], rest)
              | _ => err ("in record parse at " ^ (toksStr ts)))

          | _ => err ("in record parse at " ^ (toksStr ts)))

      fun lp (T.Var x :: ts) =  (SAST.Var x, ts)
        | lp (T.Underscore :: ts) = (SAST.Wildcard, ts)
        | lp (T.LParen :: T.RParen :: ts) = (SAST.None, ts)
        | lp (T.LParen :: ts) =
          (case nextTerm ts of
            (t, T.RParen :: ts1) => (t, ts1)
            | _ => err "error at end of parens")
        | lp (T.LBrace :: ts) = 
           (case parserecord ts of
            (ls, rest) => (SAST.Record ls, rest))
        | lp ts = err ("error at " ^ toksStr ts)

      and combine (t1, T.RParen :: ts) = (t1, T.RParen :: ts)
        | combine (t1, T.RBrace :: ts) = (t1, T.RBrace :: ts)
        | combine (t1, T.Comma :: ts) = (t1, T.Comma :: ts)
        

        | combine (t1, T.Dot :: T.Var x :: ts) = combine (SAST.Access(t1, x), ts)

        | combine (t1, (T.RightArrow :: ts)) =
            (case lp ts of
                  (t2, []) => (SAST.Case(t1, t2), [])
                  | (t2, r) => (case combine (t2, r) of
                                (t3, r') => (SAST.Case(t1, t3), r')))
            
        | combine (t1, (T.Bar :: ts)) =
            (case lp ts of
                  (t2, []) => (SAST.Or(t1, t2), [])
                  | (t2, r) => (case combine (t2, r) of
                                (t3, r') => (SAST.Or(t1, t3), r')))
            
        | combine (t1, (T.Colon :: ts)) =
            (case lp ts of
                  (t2, r) => combine (SAST.Def(t1, t2), r))

        | combine (SAST.Def(t1, t2), []) = (SAST.Def(t1, t2), [])
        | combine (SAST.Def(t1, t2), r) =
          (case nextTerm r of
            (t3,r')=> (SAST.Let(t1, t2, t3), r'))


        | combine (t1, []) = (t1, [])          
        | combine (t1, ts) = 
            (case lp ts of
                  (t2, r) => combine(SAST.App(t1, t2), r))
            
  in
    combine(lp toks)
  end

  fun parse tokens =
    (case nextTerm tokens of
        (t, []) => t
      | (t, ts) => err (": too many tokens " ^ toksStr ts)
    )
end
