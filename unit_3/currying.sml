fun sorted3_tuple (x, y, z) = z >= y andalso y >= x

val t1 = sorted3_tuple(1,2,3)

val sorted3 = fn x => fn y => fn z => z >= y andalso y >= x

val t2 = sorted3 1 2 3

fun sorted3_nicer x y z = z >= y andalso y >= x

val t3 = sorted3_nicer 1 2 3

fun fold f acc xs =
  case xs of
    [] => acc
  | x::xs' => fold f (f(acc, x)) xs'

fun sum xs = fold (fn (x, y)=> x + y) 0 xs