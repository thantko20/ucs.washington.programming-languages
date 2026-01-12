fun compose(f, g) = fn x => f(g x)

fun sqrt_of_abs i = Math.sqrt (Real.fromInt (abs i))

val sqrt_of_abs = Math.sqrt o Real.fromInt o abs

infix !>

fun x !> f = f x

fun sqrt_of_abs i = i !> abs !> Real.fromInt !> Math.sqrt