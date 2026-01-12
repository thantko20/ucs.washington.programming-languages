datatype mytype = TwoInts of int*int
                | Str of string
                | MyList of int list
                | Pizza

val a = TwoInts(1,2)
val b = Str "hi"
val c = MyList [1,2,3,4]
val d = Pizza