structure Interpret : sig

  val interpret   : string -> AST.term
  val show        : string -> string
  val showstep    : string -> string

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

  fun showstep code =
    let
      val tokens = Scan.scan code
      val sast   = Parse.parse tokens
      val ast    = Desugar.desugar sast
      fun showstep t = 
          (case Eval.step t of
                NONE => t
              | SOME t' => (print(AST.tos t' ^ "\n"); showstep t'))      
    in
      print(AST.tos ast ^ "\n");
      AST.tos (showstep ast)
    end

    fun show code = AST.tos (interpret code)

end
