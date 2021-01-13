structure Token = struct

  datatype token
    = Var of string 
    | LParen
    | RParen
    | RightArrow
    | Bar
    | Underscore
    | Colon
    
  fun tos (Var x) = "Var(" ^ x ^ ")"
    | tos LParen = "LParen"
    | tos RParen = "RParen"
    | tos RightArrow = "RightArrow"
    | tos Bar = "Bar"
    | tos Underscore = "Underscore"
    | tos Colon = "Colon"
end
