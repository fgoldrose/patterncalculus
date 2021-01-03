structure Interpret : sig

  val interpret        : string -> AST.term

end = struct

  fun interpret code =
    let
      val tokens = Scan.scan code
      val ast    = Parse.parse tokens
      val v      = Eval.eval ast
    in
      v
    end

end
