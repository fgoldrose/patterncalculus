structure Map = RedBlackMapFn (struct
    type ord_key = string
    val compare = String.compare
  end)

 