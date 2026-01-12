(* CSE 341, HW2 Provided Code *)

(* main datatype definition we will use throughout the assignment *)
datatype json =
         Num of real (* real is what SML calls floating point numbers *)
       | String of string
       | False
       | True
       | Null
       | Array of json list
       | Object of (string * json) list

(* some examples of values of type json *)
val json_pi    = Num 3.14159
val json_hello = String "hello"
val json_false = False
val json_array = Array [Num 1.0, String "world", Null]
val json_obj   = Object [("foo", json_pi), ("bar", json_array), ("ok", True)]

(* some provided one-liners that use the standard library and/or some features
   we have not learned yet. (Only) the challenge problem will need more
   standard-library functions. *)

(* dedup : string list -> string list -- it removes duplicates *)
fun dedup xs = ListMergeSort.uniqueSort String.compare xs

(* strcmp : string * string -> order compares strings alphabetically
   where datatype order = LESS | EQUAL | GREATER *)
val strcmp = String.compare                                        
                        
(* convert an int to a real *)
val int_to_real = Real.fromInt

(* absolute value of a real *)
val real_abs = Real.abs

(* convert a real to a string *)
val real_to_string = Real.toString

(* return true if a real is negative : real -> bool *)
val real_is_negative = Real.signBit

(* We now load 3 files with police data represented as values of type json.
   Each file binds one variable: small_incident_reports (10 reports), 
   medium_incident_reports (100 reports), and large_incident_reports 
   (1000 reports) respectively.

   However, the large file is commented out for now because it will take 
   about 15 seconds to load, which is too long while you are debugging
   earlier problems.  In string format, we have ~10000 records -- if you
   do the challenge problem, you will be able to read in all 10000 quickly --
   it's the "trick" of giving you large SML values that is slow.
*)

(* Make SML print a little less while we load a bunch of data. *)
       ; (* this semicolon is important -- it ends the previous binding *)
Control.Print.printDepth := 3;
Control.Print.printLength := 3;

use "parsed_small_police.sml";
use "parsed_medium_police.sml";

(* uncomment when you are ready to do the problems needing the large report*)
use "parsed_large_police.sml"; 

val large_incident_reports_list =
    case large_incident_reports of
        Array js => js
      | _ => raise (Fail "expected large_incident_reports to be an array")


(* Now make SML print more again so that we can see what we're working with. *)
; Control.Print.printDepth := 20;
Control.Print.printLength := 20;

(**** PUT PROBLEMS 1-8 HERE ****)

fun make_silly_json i = 
  let
    fun make (i) =
      case i of
        0 => []
      | x => Object([("b", True), ("n", Num(int_to_real(x - 1)))])::make(x - 1)
  in Array(make(i)) end

fun assoc (k, xs) =
  case xs of
    [] => NONE
  | (b,c)::rest => if k = b then SOME c else assoc (k, rest)

fun dot (j, f) =
  case j of
    Object([]) => NONE   
  | Object((a, b)::rest) => if a = f then SOME b else dot(Object(rest), f)
  | _ => NONE

fun one_fields (obj) =
  let
    fun aux (obj, acc) =
      case obj of
        Object([]) => acc
      | Object((f, _)::rest) => aux(Object(rest), f::acc)
      | _ => acc
  in aux(obj, []) end

fun no_repeats xs = List.length(xs) = List.length(dedup(xs))

fun recursive_no_field_repeats j =
  let
    fun check_obj fs =
      case fs of
        [] => true
      | (_, v)::fs' => recursive_no_field_repeats(v) andalso check_obj(fs')
    
    fun check_arr xs =
      case xs of
        [] => true
      | x::xs' => recursive_no_field_repeats(x) andalso check_arr(xs')
  in
    case j of
      Array(xs) =>  check_arr(xs)
    | Object(fs) => no_repeats(one_fields j) andalso check_obj(fs)
    | _ => true
  end

exception TestFail

fun count_occurrences (xs, ex) =
  let
    fun aux(xs, curr_str, curr_count, acc) =
      case xs of
        [] => (curr_str, curr_count)::acc
      | x::xs' => case strcmp(curr_str, x) of EQUAL => aux(xs', x, curr_count + 1, acc) | LESS => aux(xs', x, 1, (curr_str, curr_count)::acc) | GREATER => raise ex
  in
    case xs of
      [] => []
    | x::xs' => aux(xs', x, 1, [])
  end

fun string_values_for_field(s, j) =
  let
    fun loop_fields(fs, acc) =
      case fs of
        [] => acc
      | (f, String(v))::xs => if f = s then loop_fields(xs, v::acc) else loop_fields(xs, acc)
      | _::xs => loop_fields(xs, acc)

    fun loop_array(a, acc) =
      case a of
        [] => acc
      | (Object(fs)::xs) => loop_array(xs, loop_fields(fs, acc))
      | _::xs => loop_array(xs, acc)
  in
    loop_array(j, [])
  end

fun filter_field_value(f, v, jl) =
  let
    fun has_fields_values(fields) =
      case fields of
        [] => false
      | (f', String v')::xs => if f' = f andalso v' = v then true else has_fields_values(xs)
      | _::xs => has_fields_values(xs)

    fun loop_array xs =
      case xs of
        [] => []
      | Object fields :: rest => if has_fields_values fields then Object fields :: loop_array(rest) else loop_array(rest)
      | _ :: rest => loop_array(rest)
  in loop_array jl end


(* histogram and historgram_for_field are provided, but they use your 
   count_occurrences and string_values_for_field, so uncomment them 
   after doing earlier problems *)

(* histogram_for_field takes a field name f and a list of objects js and 
   returns counts for how often a string is the contents of f in js. *)
exception SortIsBroken

fun histogram (xs : string list) : (string * int) list =
  let
    fun compare_strings (s1 : string, s2 : string) : bool = s1 > s2

    val sorted_xs = ListMergeSort.sort compare_strings xs
    val counts = count_occurrences (sorted_xs,SortIsBroken)

    fun compare_counts ((s1 : string, n1 : int), (s2 : string, n2 : int)) : bool =
      n1 < n2 orelse (n1 = n2 andalso s1 < s2)
  in
    ListMergeSort.sort compare_counts counts
  end

fun histogram_for_field (f,js) =
  histogram (string_values_for_field (f, js))

(**** PUT PROBLEMS 9-11 HERE ****)

val large_event_clearance_description_histogram = histogram_for_field("event_clearance_description", large_incident_reports_list)

val large_hundred_block_location_histogram = histogram_for_field("hundred_block_location", large_incident_reports_list)

val forty_third_and_the_ave_reports = filter_field_value("hundred_block_location", "43XX BLOCK OF UNIVERSITY WAY NE", large_incident_reports_list)


;Control.Print.printDepth := 3;
Control.Print.printLength := 3;

(**** PUT PROBLEMS 12-15 HERE ****)

val forty_third_and_the_ave_event_clearance_description_histogram = histogram_for_field("event_clearance_description", forty_third_and_the_ave_reports)

val nineteenth_and_forty_fifth_reports = filter_field_value("hundred_block_location", "45XX BLOCK OF 19TH AVE NE", large_incident_reports_list)

val nineteenth_and_forty_fifth_event_clearance_description_histogram = histogram_for_field("event_clearance_description", nineteenth_and_forty_fifth_reports)

;Control.Print.printDepth := 20;
Control.Print.printLength := 20;

(**** PUT PROBLEMS 16-19 HERE ****)

fun concat_with (sep, xs) =
  case xs of
    [] => ""
  | x::[] => x
  | x::xs' => x ^ sep ^ concat_with(sep, xs')

fun quote_string x = "\""^ x ^ "\""

fun real_to_string_for_json n =
  if real_is_negative n then "-" ^ real_to_string (real_abs n) else real_to_string n

fun json_to_string j =
  let
    fun obj_to_string (fields) =
      case fields of
        [] => []
      | (field, value)::xs =>  (quote_string field ^ " : " ^ json_to_string value)::obj_to_string(xs)

    fun array_to_string (xs) =
      case xs of
        [] => []
      | x::xs' => json_to_string x :: array_to_string(xs')
  in
    case j of
      Num(x) => real_to_string_for_json x
    | String(x) => quote_string x
    | True => "true"
    | False => "false"
    | Null => "null"
    | Array(xs) => "[" ^ concat_with(", ", array_to_string xs) ^ "]"
    | Object(fields) => "{" ^ concat_with(", ", obj_to_string fields) ^ "}"
  end

(* For CHALLENGE PROBLEMS, see hw2challenge.sml *)

