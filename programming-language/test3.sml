(* use "hw3.sml"; *)
use "hw3.sml";

val test9a = count_wildcards Wildcard = 1
val test9a2 = count_wildcards (TupleP [Wildcard, Wildcard, Wildcard, Variable "x"]) = 3

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1
val test9b2 = count_wild_and_variable_lengths (TupleP [Wildcard, Wildcard, Wildcard, Variable "xabc"])

val test9c = count_some_var ("x", Variable("x")) = 1
val test9c2 = count_some_var ("x", (TupleP [Wildcard, Variable "x", Variable "x", Variable "abc"]))
val test9c3 = count_some_var ("x", ConstructorP ("x",  Variable("x")))

val test10 = check_pat (Variable("x"))
val test10a = check_pat (TupleP [ConstructorP("a", Variable "x"), Variable "b", Variable "x"])


signature DIGIT = 
sig
type digit = int
val make_digit : int -> digit
val increment : digit -> digit
val decrement : digit -> digit
val down_and_up : digit -> digit
val test : digit -> unit
end


structure Digit :> DIGIT =
struct
type digit = int
exception BadDigit
exception FailTest
fun make_digit i = if i < 0 orelse i > 9 then raise BadDigit else i
fun increment d = if d=9 then 0 else d+1
fun decrement d = if d=0 then 9 else d-1
val down_and_up = increment o decrement (* recall o is composition *)
fun test d = if down_and_up d = d then () else raise FailTest
end