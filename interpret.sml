structure Interpret : sig

  val interpret        : string -> AST.term
  val show        : string -> string
  val ienv        : string -> AST.term list -> AST.term 

end = struct

  fun interpret code =
    let
      val tokens = Scan.scan code
      val sast   = Parse.parse tokens
      val ast    = Desugar.desugar sast
      val v      = Eval.eval ast
    in
      v
    end

    fun ienv code env =
        let
          val tokens = Scan.scan code
          val sast   = Parse.parse tokens
          val ast    = Desugar.desugar sast
          val v      = Eval.ev (ast, env)
        in
          v
        end

    fun show code = AST.tos (interpret code)

end
