structure Scan : sig

  val scan : string -> Token.token list

end = struct

  structure T = Token

  fun collect test items =
    let
      fun cons1 x (xs, ys) = (x::xs, ys)
      fun lp [] = ([], [])
    | lp (all as x::xs) =
        if test x
        then cons1 x (lp xs)
        else ([], all)
    in
      lp items
    end

  fun var f cs = (* pass first char as f *)
    (case collect Char.isLower cs of
     (lowers, cs') => SOME (T.Var (implode (f::lowers)), cs'))

  fun nextToken [] = NONE
    | nextToken (#" " :: cs) = nextToken cs
    | nextToken (#"\t":: cs) = nextToken cs
    | nextToken (#"\n":: cs) = nextToken cs
    | nextToken (#"(" :: cs) = SOME (T.LParen, cs)
    | nextToken (#")" :: cs) = SOME (T.RParen, cs)
    | nextToken (#"|" :: cs) = SOME (T.Bar, cs)
    | nextToken (#"_" :: cs) = SOME (T.Underscore, cs)
    | nextToken (#"-" :: #">" :: cs) = SOME (T.RightArrow, cs)
    | nextToken (c :: cs) =
      if Char.isLower c then var c cs
      else raise Fail ("scan error: " ^ implode (c::cs))

  fun scan code =
    let
      fun lp [] = []
    | lp cs = (case nextToken cs of
               SOME (tok, cs') => tok :: lp cs'
             | NONE => [])
    in
      lp (explode code)         
    end

end
