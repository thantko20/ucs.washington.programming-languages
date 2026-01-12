exception UndesirableException

fun mydiv(x, y) =
  if y = 0
  then raise UndesirableException
  else x div y

mydiv(1,2)