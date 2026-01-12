datatype mytype = TwoInts of int*int
                | Str of string
                | MyList of int list
                | Pizza

fun f (x: mytype) = 
  case x of
    Pizza => 3
    | Str s => 8
    | TwoInts(i1, i2) => i1 + i2
    | MyList(list) => 1
