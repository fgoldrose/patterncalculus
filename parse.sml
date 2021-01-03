structure Parse : sig

  val parse : Token.token list -> AST.term

end = struct

  structure T = Token
  structure AST = AST

  fun err info = raise Fail ("parse error " ^ info)
            
  fun toksStr toks = "[" ^ String.concatWith "," (List.map T.tos toks) ^ "]"
            
  fun nextTerm toks =
    let
      fun lp (T.Var x :: ts) = SOME (AST.Var x, ts) 
        | lp (T.LParen :: ts) =
          (case nextTerm ts of
                SOME (t1, T.RightArrow :: ts1) =>
                  (case nextTerm ts1 of
                      SOME (t2, T.RParen :: ts2) => 
                        SOME (AST.Case (t1, t2), ts2)
                      | _ => err "at end of case")
                | SOME (t1, ts1) =>
                  (case nextTerm ts1 of
                      SOME (t2, T.RParen :: ts2) => 
                        SOME (AST.App (t1, t2), ts2)
                      | _ => err "at end of application")
                | _ => err "within application or case")
        | lp ts = err ("in term at " ^ toksStr ts)
  in
    lp toks
  end

  fun parse tokens =
    (case nextTerm tokens of
        SOME (t, []) => t
      | SOME (t, ts) => err (": too many tokens " ^ toksStr ts)
      | NONE => err "no main term?")
end
