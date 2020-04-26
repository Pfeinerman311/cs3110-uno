open Uno

exception UnknownPlayer
exception EmptyPile

let ah_players = [{name = "Gavin" ; hand = []}; {name = "Jeremy" ; hand = []}; 
                  {name = "Geoff" ; hand = []}]

type order =
  | CW
  | CCW

type t = {draw_pile : deck; discard_pile : deck; 
          players: player list; order : order}

let top_card st = 
  match next_card st.discard_pile with
  | h -> h
  | exception NoCards -> raise EmptyPile

let next_draw st =
  match next_card st.draw_pile with
  | h -> h
  | exception NoCards -> raise EmptyPile

let rec rem_wilds d =
  match d with
  | [] -> []
  | h::t -> if card_action h = Draw 2 || card_action h = None then
      (Wild, card_action h)::(rem_wilds t) else h::(rem_wilds t)

let fresh st = {
  draw_pile = shuffle st.draw_pile@(st.discard_pile |> last_cards |> rem_wilds);
  discard_pile = [next_card st.discard_pile];
  players = st.players;
  order = st.order
}

let rec player_update plist p =
  match plist with
  | [] -> raise (UnknownPlayer)
  | h::t -> if h.name = p.name then p::t 
    else h::(player_update t p)

let rec draw st p n =
  if n = 0 then st 
  else if List.length st.draw_pile <= n 
  then draw (fresh st) p n 
  else
    let st' = {
      draw_pile = last_cards st.draw_pile;
      discard_pile = st.discard_pile;
      players = player_update st.players 
          {name = p.name; hand = (next_card st.draw_pile)::p.hand};
      order = st.order
    } in draw st' p (n-1)

let rec deal st n =
  match n with
  | 4 -> st
  | h -> deal (draw st (List.nth st.players n) 7) (n+1)

let init p1 = 
  let st = {
    draw_pile = shuffle full_deck;
    discard_pile = [];
    players = {name = p1 ; hand = []}::ah_players;
    order = CW
  } in deal st 0

let rec keep_drawing st p =
  match next_draw st with
  | h -> let st' = draw st p 1 in 
    if playable h (top_card st)
    then st' else keep_drawing st' p
  | exception EmptyPile -> keep_drawing (fresh st) p

let rec playable_cards clist c = 
  match clist with
  | [] -> []
  | h::t -> if playable h c
    then h::(playable_cards t c) else playable_cards t c

let wild_set st clr =
  match top_card st with
  | (h, t) -> let x = (clr, t) in 
    {
      draw_pile = st.draw_pile;
      discard_pile = x::(last_cards st.discard_pile);
      players = st.players;
      order = st.order
    }

let play st p c =
  if playable c (top_card st) then 
    let x = {name = p.name; hand = rem_card c p.hand} in
    {
      draw_pile = st.draw_pile;
      discard_pile = c::st.discard_pile;
      players = player_update st.players x;
      order = st.order
    }
  else raise (IllegalMove c)

let auto_play st p =
  let x = playable_cards p.hand (top_card st) in
  play st p (next_card x)

let next_player st = 
  match st.players with
  | [] -> raise UnknownPlayer
  | h::t -> h

let last_players st =
  match st.players with
  | [] -> raise UnknownPlayer
  | h::t -> t

let turn st = {
  draw_pile = st.draw_pile;
  discard_pile = st.discard_pile;
  players = (next_player st)::(last_players st);
  order = st.order
}