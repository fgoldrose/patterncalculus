structure Parse : sig

  val parse : Token.token list -> AST.term

end = struct

  structure T = Token
  structure AST = AST

  fun err info = raise Fail ("parse error " ^ info)
            
  fun toksStr toks = "[" ^ String.concatWith "," (List.map T.tos toks) ^ "]"
            
  fun nextTerm toks =
    let
      fun lp (T.Var x :: ts) =  (AST.Var x, ts)
        | lp (T.LParen :: ts) =
          (case nextTerm ts of
            (t, T.RParen :: ts1) => (t, ts1)
            | _ => err "error at end of parens"
          )
        | lp ts = err "error at " toksStr ts

      and combine (t1, []) = (t1, [])
        | combine (t1, T.RParen :: ts) = (t1, T.RParen :: ts)
        | combine (t1, (T.RightArrow :: ts)) =
            (case lp ts of
                  (t2, []) => (AST.Case(t1, t2), [])
                  | (t2, r) => (case combine (t2, r) of
                                (t3, r') => (AST.Case(t1, t3), r'))
            )
        | combine (t1, ts) = 
            (case lp ts of
                  (t2, r) => combine(AST.App(t1, t2), r)
            )
  in
    combine(lp toks)
  end

  fun parse tokens =
    (case nextTerm tokens of
        (t, []) => t
      | (t, ts) => err (": too many tokens " ^ toksStr ts)
    )
end
