structure Token = struct

  datatype token
    = Var of string 
    | LParen
    | RParen
    | RightArrow
    | Bar
    | Underscore
    | Colon
    | LBrace
    | RBrace
    | Comma
    | Equals
    | Dot
    
  fun tos (Var x) = "Var(" ^ x ^ ")"
    | tos LParen = "LParen"
    | tos RParen = "RParen"
    | tos RightArrow = "RightArrow"
    | tos Bar = "Bar"
    | tos Underscore = "Underscore"
    | tos Colon = "Colon"
    | tos LBrace = "LBrace"
    | tos RBrace = "RBrace"
    | tos Comma = "Comma"
    | tos Equals = "Equals"
    | tos Dot = "Dot"
end

