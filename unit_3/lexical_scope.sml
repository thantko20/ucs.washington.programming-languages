val x = 1
fun f y = x + y
val x = 2
val y = 3
val z = f (x + y)
(* value of z is actually 6. Functions are evaluated in the environment created, not where the functions are called in the run time *)
