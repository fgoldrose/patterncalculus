structure Interpret : sig

  val interpret    : string -> AST.term
  val show         : string -> string
  val showstep     : string -> AST.term
  val file         : string -> AST.term
  val fileshowstep : string -> AST.term
  val parsed : string -> AST.term

end = struct

  fun parsed code =
    let
      val tokens = Scan.scan code
      val sast   = Parse.parse tokens
      val ast    = Desugar.desugar sast
    in
      print(SAST.tos sast);
      ast
    end
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
      fun dostep t = 
          (case Eval.step t of
                NONE => t
              | SOME t' => (print(AST.tos t' ^ "\n"); dostep t'))      
    in
      print(AST.tos ast ^ "\n");
      dostep ast
    end

    fun show code = AST.tos (interpret code)

    fun file filename = interpret (Read.file filename)
    fun fileshowstep filename = showstep (Read.file filename)

end
