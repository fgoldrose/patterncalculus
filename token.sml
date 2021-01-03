structure Token = struct

  datatype token
    = Var of string 
    | LParen
    | RParen
    | RightArrow
    
  fun tos (Var x) = "Var(" ^ x ^ ")"
    | tos LParen = "LParen"
    | tos RParen = "RParen"
    | tos RightArrow = "RightArrow"
end
