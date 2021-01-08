structure Token = struct

  datatype token
    = Var of string 
    | LParen
    | RParen
    | RightArrow
    | Bar
    
  fun tos (Var x) = "Var(" ^ x ^ ")"
    | tos LParen = "LParen"
    | tos RParen = "RParen"
    | tos RightArrow = "RightArrow"
    | tos Bar = "Bar"
end
