(*
1 • val card_color = fn : card -> color
2 • val card_value = fn : card -> int
3 • val remove_card = fn : card list * card * exn -> card list
4 • val all_same_color = fn : card list -> bool
5 • val sum_cards = fn : card list -> int
6 • val score = fn : card list * int -> int
7 • val officiate = fn : card list * move list * int -> int
*)
datatype suit = Clubs
              | Diamonds
              | Hearts
              | Spades;
datatype rank = Jack
              | Queen
              | King
              | Ace
              | Num of int;
type card = suit * rank;
datatype color = Red | Black
datatype move = Discard of card | Draw
exception IllegalMove


fun card_color c =
    case c of
       (Clubs, _) => Black
     | (Diamonds, _) => Red
     | (Hearts, _) => Red
     | (Spades, _) => Black;


fun card_value c =
    case c of
       (_,Jack)  => 10
     | (_,Queen) => 10
     | (_,King) => 10
     | (_,Ace) => 11
     | (_, Num n)=> n;


fun remove_card (cl, c, exc) =
  let fun found ([], c) = false
    | found (cl as hd::tl, c) = if hd = c then true else found (tl, c)
    in
      if found(cl, c) then
        (case cl of
            []=>[]
            | xs::ys => if c = xs then remove_card(ys,c, exc)
                        else xs::remove_card(ys,c, exc))
      else raise exc
    end

fun all_same_color cl =
    case cl of
       [] => true
       | hd::nk::tl => (case card_color hd of
                          Black => (card_color nk = Black) andalso (all_same_color (nk::tl))
                        | Red => (card_color nk = Red) andalso (all_same_color (nk::tl)))
      | hd::[] => true


fun sum_cards [] = 0
  | sum_cards (c as hd::tl) = card_value hd + sum_cards tl;


fun score ([], _) = 0
  | score (held_cards as hd::tail, goal) =
    let fun preliminaryScore (sum, goal) =
      if sum > goal then 3*(sum-goal)
      else (goal - sum);
    in
      if all_same_color held_cards then floor (real( preliminaryScore(sum_cards held_cards, goal))/ real 2)
      else preliminaryScore(sum_cards held_cards, goal)
    end



(* 7 • val officiate = fn : card list * move list * int -> int *)
fun officiate ([], [], _) = 0
  | officiate (card_list, move_list, goal) =
    (* Si move_list es vacía, retorna el score*)
    let fun officiate_helper (card_list, [], goal, held_cards) =  score(card_list, goal)

    (* Si vienen todos los parámetros *)
      | officiate_helper (card_list as card_list_hd::card_list_tl, move_list as move_list_hd::move_list_tl, goal, held_cards) =
        if sum_cards card_list > goal orelse move_list = [] then  score(card_list, goal)
        else
          (case move_list_hd of
            (* Si es discard, llamo a la recursión llamando a remove_card sobre card_list *)
            Discard c => officiate_helper(card_list, move_list_tl, goal, remove_card(card_list, c, IllegalMove))
            (* Si es Draw,
            si la card_list está vacía, retorna score,
            si no llamo a la recursión con el tail de card_list y el cons de card_list_hd en held_cards *)
          | Draw => if card_list = [] then  score(card_list, goal)
                    else officiate_helper(card_list_tl, move_list_tl, goal, card_list_hd::held_cards));
    in
      officiate_helper(card_list, move_list, goal, [])
    end



val test1 = card_color (Clubs, Num 2) = Black;
val test2 = card_value (Clubs, Num 2) = 2;
val test3 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = [];
val test4 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true;
val test5 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4;
val test6 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4;
val test7 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6;
val test8 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],[Draw,Draw,Draw,Draw,Draw],42)= 3;
