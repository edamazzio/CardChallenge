(*
Esteban González Damazio
Alejandro Jimenez Gamboa
Lenguajes de programacion
Proyecto 2 "Card Challenge"
*)

(*Tipos de datos usados para el proyecto, simulan una baraja de naipes
  dados por el profesor*)
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

(*Estos tipos de datos son parte de las mecanicas del juego*)
datatype color = Red | Black
datatype move = Discard of card | Draw
(*Excepcion usada cuando el jugador intenta hacer un movimiento no permitido*)
exception IllegalMove

(*Funcion 1 Recibe una carta y dice si es de color Roja o Negra*)
fun card_color c =
    case c of
       (Clubs, _) => Black
     | (Diamonds, _) => Red
     | (Hearts, _) => Red
     | (Spades, _) => Black;

(*Funcion 2 Recibe una carta y retorna que valor tiene, los numeros valen igual, el as vale 11
  y las demas valen 10*)
fun card_value c =
    case c of
       (_,Jack)  => 10
     | (_,Queen) => 10
     | (_,King) => 10
     | (_,Ace) => 11
     | (_, Num n)=> n;

(*Funcion 3 Recibe una lista de cartas y una carta, busca la carta en la lista y la elimina,
  si la carta aparece mas de una vez solo elimina la primera aparicion, tira una excepcion
  si la carta no esta*)
(*fun remove_card (cl, c) =
    case cl of
      [] => []
      | xs::ys => if c = xs then remove_card(ys,c)
                  else xs::remove_card(ys,c);*)

fun remove_card (cl: card list, c : card, e) =
  let fun found ([], c) = false
    | found (cl as hd::tl, c) = if hd = c then true else found (tl, c)
    in
      if found(cl, c) then
        (case cl of
            []=>[]
            | xs::ys => if c = xs then remove_card(ys,c,e)
                        else xs::remove_card(ys,c,e))
      else raise e
    end

(*Funcion 4 Recibe una lista de cartas, revisa los colores de cada carta de la lista,
  retorna true si tienen el mismo color, apenas encuentra una de diferente color
  retorna false*)
fun all_same_color cl =
    case cl of
       [] => true
       | hd::nk::tl => (case card_color hd of
                          Black => (card_color nk = Black) andalso (all_same_color (nk::tl))
                        | Red => (card_color nk = Red) andalso (all_same_color (nk::tl)))
      | hd::[] => true

(*Funcion 5 Recibe una lista, utiliza recursion de cola para sumar
  los valores de las cartas que vengan en la lista*)
fun sum_cards [] = 0
  | sum_cards (c as hd::tl) = card_value hd + sum_cards tl;

(*Funcion 6 Recibe una lista y un comodin
  El score funciona de la siguiente manera:
    sea sum la suma de los valores de held-cards. Si sum es mayor que goal,
    entonces el preliminary score es tres veces (sum – goal), si no, entonces el preliminay score es
    (goal – sum). Finalmente, se puede decir que score (el resultado final) es el preliminary score, a
    menos de que todas las cartas en held-cards sean del mismo color, en ese caso, el score sería el
    preliminary score dividido entre 2 (con redondeo de piso).
*)
fun score (held_cards, goal) =
    let fun preliminaryScore (sum, goal) =
      if (sum > goal) then (3*(sum-goal))
      else (goal - sum);
    in
      if all_same_color held_cards then floor (real( preliminaryScore(sum_cards held_cards, goal))/ real 2)
      else preliminaryScore(sum_cards held_cards, goal)
    end


fun officiate (card_list, move_list, goal) =
    (* Si move_list es vacía, retorna el score*)
    let fun officiate_helper (card_list, [], goal, held_cards) =  score(held_cards, goal)
     | officiate_helper ([], move_list as move_list_hd::move_list_tl, goal, held_cards) =
     (case move_list_hd of
       (* Si es discard, llamo a la recursión llamando a remove_card sobre card_list *)
       Discard c => officiate_helper(card_list, move_list_tl, goal, remove_card(card_list, c, IllegalMove))
       (* Si es Draw,
       si la card_list está vacía, retorna score,
       si no llamo a la recursión con el tail de card_list y el cons de card_list_hd en held_cards *)
     | Draw => score(held_cards, goal))


    (* Si vienen todos los parámetros *)
      | officiate_helper (card_list as card_list_hd::card_list_tl, move_list as move_list_hd::move_list_tl, goal, held_cards) =
        if (sum_cards held_cards > goal) then  score(held_cards, goal)
        else
          (case move_list_hd of
            (* Si es discard, llamo a la recursión llamando a remove_card sobre card_list *)
            Discard c => officiate_helper(card_list, move_list_tl, goal, remove_card(card_list, c, IllegalMove))
            (* Si es Draw,
            si la card_list está vacía, retorna score,
            si no llamo a la recursión con el tail de card_list y el cons de card_list_hd en held_cards *)
          | Draw => officiate_helper(card_list_tl, move_list_tl, goal, card_list_hd::held_cards));
    in
      officiate_helper(card_list, move_list, goal, [])
    end

(*Pruebas de cada funcion, especificadas por el profesor*)
(* val test1 = card_color (Clubs, Num 2)
val test2 = card_value (Clubs, Num 2)
val test3 = remove_card ([(Clubs, Num 3),(Hearts, Ace),(Clubs, Num 2)], (Hearts, Ace),IllegalMove)
	    handle IllegalMove => []
val test4 = all_same_color [(Hearts, Ace), (Hearts, Ace)]
val test5 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)]
val test6 = score ([(Hearts, Num 2),(Clubs, Num 4)],10)
val test7 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15)
val test8 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],[Draw,Draw,Draw,Draw,Draw],42) *)
