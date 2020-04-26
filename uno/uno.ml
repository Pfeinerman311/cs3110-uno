type color = 
  | Red
  | Yellow
  | Green
  | Blue
  | Wild
type action = Zero | One | Two | Three | Four | Five | Six | Seven | Eight 
            | Nine | Skip | Reverse | Draw of int | None

type card = color * action

type deck = card list

type hand = card list

type player = {name : string; hand : hand}

type uno = {draw : deck; discard : deck; players : player list}

exception IllegalMove of card
exception NoCards
exception UnknownCard

let dbls =
  [One; Two; Three; Four; Five; Six; Seven; Eight; Nine; Skip; Reverse; Draw 2]

let rec deck_builder a = 
  match a with
  | [] -> []
  | h::t -> if h != Draw 4 || h != None 
    then [(Red, h); (Yellow, h); (Green, h); (Blue, h)]@(deck_builder t)
    else let x = [(Wild, h)] in x@x@x@x 

let full_deck = 
  let x = [Zero]@dbls@dbls@[Draw 4; None] in
  deck_builder x

let shuffle d = 
  let arr = Array.of_list d in
  for x = (Array.length arr - 1) downto 1 do
    let a = Random.int (x + 1) in
    let b = arr.(a) in
    arr.(a) <- arr.(x);
    arr.(x) <- b
  done;
  Array.to_list arr
(**
   let val_of_action actn = 
   match actn with
   | Zero -> 0
   | One -> 1
   | Two -> 2
   | Three -> 3
   | Four -> 4
   | Five -> 5
   | Six -> 6
   | Seven -> 7
   | Eight -> 8
   | Nine -> 9
   | Skip -> 10
   | Reverse -> 11
   | Draw h -> h
   | None -> 99


   let deal d p =
   let l = List.length p
      match p with 
*)

let next_card d =
  match d with
  | [] -> raise NoCards
  | h::t -> h

let card_color c =
  match c with
  | (h, t) -> h

let card_action c =
  match c with
  | (h, t) -> t

let playable c1 c2 =
  if card_color c1 = card_color c2 || card_action c1 = card_action c2 
  then true else false

let rec rem_card c d =
  match d with
  | [] -> raise (IllegalMove c)
  | h::t -> if h = c then t else
      h::(rem_card c t)

let add_card c d =
  c::d

let count d =
  List.length d

let rec playable_cards c d =
  match d with
  | [] -> []
  | h::t -> h