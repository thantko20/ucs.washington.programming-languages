datatype ('a, 'b) tree = Node of 'a * ('a, 'b) tree * ('a, 'b) tree
                        | Leaf of 'b

fun sum_tree tr =
  case tr of
    Leaf i => i
  | Node(i, lft, rght) => i + sum_tree lft + sum_tree rght