(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
         | Variable of string
         | UnitP
         | ConstP of int
         | TupleP of pattern list
         | ConstructorP of string * pattern

datatype valu = Const of int
          | Unit
          | Tuple of valu list
          | Constructor of string * valu

fun g f1 f2 p =
    let 
    val r = g f1 f2 
    in
    case p of
        Wildcard          => f1 ()
      | Variable x        => f2 x
      | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
      | ConstructorP(_,p) => r p
      | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
         | UnitT
         | IntT
         | TupleT of typ list
         | Datatype of string

(**** you can put all your code here ****)

fun only_capitals strs =
    List.filter (fn str => Char.isUpper (String.sub (str, 0))) strs
    (* List.filter (Char.isUpper (String.sub (str, 0))) strs *)

fun longest_string1 strs =
    foldl (fn (str, res) => if String.size str > String.size res
                            then str
                            else res) "" strs

fun longest_string2 strs =
    foldl (fn (str, res) => if String.size str >= String.size res
                            then str
                            else res) "" strs

fun longest_string_helper f strs =
    foldl (fn (str, res) =>  if f (String.size str, String.size res)
                             then str
                             else res) "" strs


val longest_string3 = longest_string_helper (fn (a, b) => a > b)
val longest_string4 = longest_string_helper (fn (a, b) => a >= b)

val longest_capitalized = (longest_string1 o only_capitals)

val rev_string = (implode o rev o explode)

fun first_answer f strs =
    case strs of
        [] => raise NoAnswer
      | str::strs' => (case f str of
                          NONE => first_answer f strs'
                        | SOME v => v)

fun all_answers f strs = 	
    let fun aux rest res = case rest of 
                            [] => SOME res
                          | x::xs => (case f x of
                                NONE => NONE
                              | SOME v => aux xs (res @ v))
    in aux strs []
    end

val count_wildcards = g (fn () => 1) (fn _ => 0)

val count_wild_and_variable_lengths = g (fn () => 1) String.size

fun count_some_var (str, p) = 
    g (fn () => 0) (fn x => if x = str then 1 else 0) p

fun check_pat p = 
    let
        fun get_vals p = case p of 
                            Variable x => [x]
                          | TupleP ps => foldl (fn (p, res) => get_vals p @ res) [] ps
                          | ConstructorP (_, p) => get_vals p
                          | _ => []
        fun is_note_repeat strs = 
            case strs of
                [] => true
              | x::xs => List.exists (fn _ => foldl (fn (x', res) => res andalso x <> x') true xs) strs
              (* | x::xs => List.exists (fn x' => x' <> x) xs andalso is_note_repeat xs *)
    in
        (is_note_repeat o get_vals) p
    end

fun match (v, p) = 
    case (v, p) of
        (_, Wildcard) => SOME []
      | (Unit, UnitP) => SOME []
      | (Const i, ConstP j) => if i = j then SOME [] else NONE
      | (v, Variable x) => SOME [(x, v)]
      | (Tuple vs, TupleP ps) => if List.length vs = List.length ps
                                 then all_answers match (ListPair.zip (vs, ps))
                                 else NONE
      | (Constructor (s, v), ConstructorP (s',p)) => if s = s' then match (v , p) else NONE
      | _ => NONE

fun first_match v ps =
    SOME (first_answer (fn p => match (v,p)) ps)
    handle NoAnswer => NONE
