(* CSE341, HW3 Provided Code *)

exception NoAnswer

datatype pattern = WildcardP
                 | VariableP of string
                 | UnitP
                 | ConstantP of int
                 | ConstructorP of string * pattern
                 | TupleP of pattern list

datatype valu = Constant of int
              | Unit
              | Constructor of string * valu
              | Tuple of valu list

fun g f1 f2 p =
    let 
        val r = g f1 f2 
    in
        case p of
            WildcardP         => f1 ()
          | VariableP x       => f2 x
          | ConstructorP(_,p) => r p
          | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
          | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = AnythingT
             | UnitT
             | IntT
             | TupleT of typ list
             | DatatypeT of string

(**** you can put all your code here ****)
val only_lowercase = List.filter(fn x => Char.isLower(String.sub(x, 0)));

val longest_string1 = foldl (fn (acc, x) => if String.size acc > String.size x then acc else x) ""

val longest_string2 = foldl (fn (acc, x) => if String.size acc >= String.size x then acc else x) ""

fun longest_string_helper f = foldl(fn (acc, x) => if f(String.size acc,String.size x) then acc else x) ""

val longest_string3 = longest_string_helper (fn(s1, s2) => s1 > s2)

val longest_string4 = longest_string_helper (fn(s1, s2) => s1 >= s2)

val longest_lowercase = longest_string1 o only_lowercase

val caps_no_X_string = String.implode o List.map Char.toUpper o List.filter (fn x => #"X" <> x andalso #"x" <> x) o String.explode

(* 'a -> 'b option -> 'a list -> 'b *)
fun first_answer f xs =
    case xs of
        [] => raise NoAnswer
    |   x::xs' => case f x of
                    SOME b => b
                |   NONE => first_answer f xs'

(* ('a -> 'b list option) -> 'a list -> 'b list option *)
fun all_answers f xs =
    let 
        fun aux(xs, acc) =
            case xs of
                [] => SOME acc
            |   x::xs' =>
                    case f x of
                        NONE => NONE
                    |   SOME ys => aux(xs', acc @ ys)
    in
        aux(xs, [])
    end

val count_wildcards = g (fn () => 1) (fn x => 0)

val count_wild_and_variable_lengths = g (fn () => 1) String.size
val cwvl = count_wild_and_variable_lengths(TupleP [WildcardP, VariableP("asdf"), UnitP, WildcardP]);

fun count_a_var (s, p) = g (fn () => 0) (fn x => if x = s then 1 else 0) p
val cav = count_a_var("asdf", TupleP [WildcardP, VariableP("asdf"), UnitP, WildcardP, VariableP("jkl;")])

fun check_pat p =
    let
        fun take_names p =
            case p of
                VariableP x => x::[]
            |   TupleP ([]) => []
            |   TupleP (x::xs) => take_names x @ take_names(TupleP xs)
            |   _ => []
        
        fun check_repeats xs =
            case xs of
                [] => true
            |   x::xs' => if List.exists (fn s => s = x) xs' then false else check_repeats(xs')
    in
        check_repeats(take_names(p))
    end

fun match (v, p) =
    case (v, p) of
        (_, WildcardP) => SOME []
    |   (v, VariableP x) => SOME [(x, v)]
    |   (Unit, UnitP) => SOME []
    |   (Constant i, ConstantP j) => if i = j then SOME [] else NONE
    |   (Constructor(s1, v), ConstructorP(s2, p)) => if s1 = s2 then match(v, p) else NONE
    |   (Tuple vs, TupleP ps) =>
            if length vs = length ps
            then all_answers match (ListPair.zip (vs, ps))
            else NONE
    |   _ => NONE

fun first_match (v, ps) =
    SOME (first_answer (fn p => match (v, p)) ps) 
    handle NoAnswer => NONE