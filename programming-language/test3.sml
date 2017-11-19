(* use "hw3.sml"; *)
use "hw3ch.sml";

val test9a = count_wildcards Wildcard = 1
val test9a2 = count_wildcards (TupleP [Wildcard, Wildcard, Wildcard, Variable "x"]) = 3

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1
val test9b2 = count_wild_and_variable_lengths (TupleP [Wildcard, Wildcard, Wildcard, Variable "xabc"])

val test9c = count_some_var ("x", Variable("x")) = 1
val test9c2 = count_some_var ("x", (TupleP [Wildcard, Variable "x", Variable "x", Variable "abc"]))
val test9c3 = count_some_var ("x", ConstructorP ("x",  Variable("x")))

val test10 = check_pat (Variable("x"))
val test10a = check_pat (TupleP [ConstructorP("a", Variable "x"), Variable "b", Variable "x"])