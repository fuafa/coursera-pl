(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (str: string, strs: string list) = 
    case strs of 
        [] => NONE
        | x::xs => if same_string(str, x)
                   then SOME xs
                   else case all_except_option(str, xs) of
                        NONE => NONE
                        | SOME x' => SOME (x::x')
;;

fun get_substitutions1 (list_of_strs: string list list, str: string) = 
    case list_of_strs of
        [] => []
        | x::xs => case all_except_option(str, x) of
            NONE => get_substitutions1(xs, str)
            | SOME x' => x' @ get_substitutions1(xs, str)
;;

fun get_substitutions2 (list_of_strs: string list list, str: string) =
    let fun aux (list_of_strs, res) =
        case list_of_strs of
            [] => res
            | x::xs => case all_except_option (str, x) of 
                NONE => aux (xs, res)
                | SOME x' => aux (xs, x' @ res)
    in
        aux (list_of_strs, [])
    end
;;

fun similar_names (list_of_strs, {first = first, middle = middle, last = last}: {first:string,middle:string,last:string}) = 
    let fun aux(names, res) = 
        case names of
            [] => res
            | x::xs => aux (xs, {first = x, middle = middle, last = last}::res)
    in
        aux (get_substitutions2(list_of_strs, first), [{ first = first, middle = middle, last = last }])
    end
;;

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color (s, r) = 
    case s of
        Clubs => Black
        | Spades => Black
        | _ => Red
;;

fun card_value (s, r) =
    case r of
        Ace => 11
        | Num i => i
        | _ => 10
;;

fun remove_card (cs, c, e) =
    let fun aux (cs, res) =
        case cs of
            [] => raise e
            | x::xs => if x = c
                then res @ xs
                else aux (xs, x::res)
    in
        aux (cs, [])
    end
;;
fun all_same_color (cards) =
    case cards of
        [] => true
        | _::[] => true
        | head::neck::rest => card_color(head) = card_color(neck) andalso all_same_color(neck::rest)
;;
fun sum_cards (cards) =
    let fun aux (cards, res) =
        case cards of
            [] => res
            | x::xs => aux (xs, res + card_value x)
    in
        aux (cards, 0)
    end
;;

fun score (cards, goal) =
    let val sum = sum_cards (cards)
    in case all_same_color cards of
        true => if sum > goal
            then 3 * (sum - goal) div 2
            else (goal - sum) div 2
        | false => if sum > goal
            then 3 * (sum - goal)
            else goal - sum
    end
;;
fun officiate (cards, moves, goal) =
    let fun aux (card_list, moves, held_cards) =
        let val scores = score (held_cards, goal)
        in 
            if sum_cards held_cards > goal
            then scores
            else case moves of
                [] => scores
                | x::xs => (case x of
                    Draw => (case card_list of 
                        [] => scores
                        | head_card::tl_card_list => aux (tl_card_list, xs, head_card::held_cards)
                    )
                    | Discard c => let
                        val remain_held_cards = remove_card (held_cards, c, IllegalMove)
                        in
                            aux (card_list, xs, remain_held_cards)
                        end
                )
        end
    in
        aux (cards, moves, [])
    end
;;