(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* a.all_except_option *)
fun all_except_option(str, sl) =
    case sl of
       [] => NONE
      | x::xs => case same_string(str, x) of
                    true => SOME(xs)
                   | false => case all_except_option(str, xs) of
                                 NONE => NONE
                                | SOME y => SOME(x::y)
(* b.get_substitutions1 *)
fun get_substitutions1(sl, str) =
    case sl of
       [] => []
      | x::xs => case all_except_option(str, x) of
                    NONE => [] @ get_substitutions1(xs, str)
                   | SOME y => y @ get_substitutions1(xs, str)

(* c.get_substitutions2 *)
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

(* d.similar_names *)
fun similar_names (subs, name) =
  let fun aux(subs, acc) =
    case subs of
      [] => acc
      | (x::xs) => aux(xs, acc @ [{first=x, middle=(#middle name), last=(#last name)}]) 
  in
    aux(get_substitutions2(subs, #first name), [name])
  end
   

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
(* a.card_color *)
fun card_color(card) = 
    case card of 
       (Clubs, _) => Black
      | (Spades, _) => Black
      | (Diamonds, _) => Red
      | (Hearts, _) => Red

(* b.card_value *)
fun card_value(card) = 
    case card of
       (_, Ace) => 11
      | (_, Jack) => 10
      | (_, Queen) => 10
      | (_, King) => 10
      | (_, Num n) => n

(* c.remove_card *)
fun remove_card(cs, c, e) = 
    case cs of
       [] => raise e
       | x::xs => case c = x of
                    true => xs
                    | false => case remove_card(xs, c, e) of
                                 [] => [x]
                                 | y::ys => x::y::ys

(* d.all_same_color *)
fun all_same_color(cs) = 
    case cs of 
       [] => true
       | a::[] => true
       | a::b::tail => case card_color(a) = card_color(b) of
                          true => all_same_color(b::tail)
                          | false => false

(* e.sum_cards *)
fun sum_cards(cs) = 
  let fun aux(cs, acc) = 
          case cs of
            [] => acc
            | c::cs' => aux(cs', card_value(c) + acc)
   in
     aux(cs, 0)
   end

(* f.score *)
fun score(cs, goal) = 
  let
     val is_same = all_same_color(cs)
     val sum = sum_cards(cs)
     val preliminary_score = if sum > goal then 3 * (sum - goal) else goal - sum
   in
     if is_same then preliminary_score div 2
     else preliminary_score
   end 

(* g.officiate *)
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
    
  


    