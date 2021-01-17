structure Interpret : sig

  val interpret    : string -> AST.term
  val show         : string -> string
  val debug        : string -> AST.term
  val file         : string -> AST.term
  val debugfile : string -> AST.term
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
      val v      = Eval.eval ast false
    in
      v
    end

  fun debug code =
    let
      val tokens = Scan.scan code
      val sast   = Parse.parse tokens
      val ast    = Desugar.desugar sast
      val v      = Eval.eval ast true 
    in
      v
    end

    fun show code = AST.tos (interpret code)

    fun file filename = interpret (Read.file filename)
    fun debugfile filename = debug (Read.file filename)

end
