fun fact n =
  if n = 0
  then 1
  else n * (fact(n - 1) : IntInf.int);

fun tail_fact n = 
  let 
    fun aux_fact(n, acc) =
      if n = 0 then acc else aux_fact(n - 1, n * acc)
  in
    aux_fact(n, 1 : IntInf.int)
  end
