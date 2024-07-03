(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (str, sl) =
    case sl of
       [] => NONE
      | x::xs => case same_string(str, x) of
                    true => SOME(xs)
                   | false => case all_except_option(str, xs) of
                                 NONE => NONE
                                | SOME y => SOME(x::y)

fun get_substitutions1(sl, str) =
    case sl of
       [] => []
      | x::xs => case all_except_option(str, x) of
                    NONE => [] @ get_substitutions1(xs, str)
                   | SOME y => y @ get_substitutions1(xs, str)

fun get_substitutions2(sl, str) = 
    let
       fun aux(sl, str, acc) =
           case sl of
              [] => acc
             | x::xs => case all_except_option(str, x) of
                           NONE => aux(xs, str, acc)
                          | SOME ys => aux(xs, str, ys@acc)
    in
       aux(sl, str, [])
    end

fun similar_names (subs, name) =
  let fun aux(subs, acc) =
    case subs of
      [] => acc
      | (x::xs) => aux(xs, acc @ [{first=x, middle=(#middle name), last=(#last name)}]) 
  in
    aux(get_substitutions2(subs, #first name), [name])
  end


datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

fun card_color(card) = 
    case card of 
       (Clubs, _) => Black
      | (Spades, _) => Black
      | (Diamonds, _) => Red
      | (Hearts, _) => Red

fun card_value(card) = 
    case card of
       (_, Ace) => 11
      | (_, Jack) => 10
      | (_, Queen) => 10
      | (_, King) => 10
      | (_, Num n) => n

fun remove_card(cs, c, e) = 
    case cs of
       [] => raise e
       | x::xs => case c = x of
                    true => xs
                    | false => case remove_card(xs, c, e) of
                                 [] => [x]
                                 | y::ys => x::y::ys

fun all_same_color(cs) = 
    case cs of 
       [] => true
       | a::[] => true
       | a::b::tail => case card_color(a) = card_color(b) of
                          true => all_same_color(b::tail)
                          | false => false

fun sum_cards(cs) = 
  let fun aux(cs, acc) = 
          case cs of
            [] => acc
            | c::cs' => aux(cs', card_value(c) + acc)
   in
     aux(cs, 0)
   end

fun score(cs, goal) = 
  let
     val is_same = all_same_color(cs)
     val sum = sum_cards(cs)
     val preliminary_score = if sum > goal then 3 * (sum - goal) else goal - sum
   in
     if is_same then preliminary_score div 2
     else preliminary_score
   end

fun officiate(cl, ml, goal) =
  let
     fun aux(cl, ml, goal, hs, e) =
         case ml of
            [] => hs
            | m::ml' => case m of
                           Discard c => if List.exists (fn (card) => c = card) hs then aux(cl, ml', goal, remove_card(hs, c, e), e) else raise e
                           | Draw => case cl of
                                        [] => hs
                                        | c::cl' => case sum_cards(c::hs) > goal of
                                                        true => c::hs
                                                        | false => aux(cl', ml', goal, c::hs, e)
   in
     score(aux(cl, ml, goal, [], IllegalMove), goal)
   end



val test1 = all_except_option ("string", ["string"]) = SOME []
val test1_1 = all_except_option ("string", ["string", "hello", "world"]) = SOME ["hello", "world"]
val test1_2 = all_except_option ("string", []) = NONE
val test1_3 = all_except_option ("string", hd([["string"]])) = SOME []
val test2 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test2_1 = get_substitutions1 ([], "foo") = []
val test2_2 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") = ["Fredrick","Freddie","F"]
val test2_3 = get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],"Jeff") = ["Jeffrey","Geoff","Jeffrey"]
val test3 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test3_1 = get_substitutions1 ([], "foo") = []
val test3_2 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") = ["Fredrick","Freddie","F"]
val test3_3 = get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],"Jeff") = ["Jeffrey","Geoff","Jeffrey"]
val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred",last="Smith",middle="W"},
   {first="Freddie",last="Smith",middle="W"},
   {first="F",last="Smith",middle="W"},
   {first="Fredrick",last="Smith",middle="W"}]
val test5 = card_color (Clubs, Num 2) = Black
val test6 = card_value (Clubs, Num 2) = 2
val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true
val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4
val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4
val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6
val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3
val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)