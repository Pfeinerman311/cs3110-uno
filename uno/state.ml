open Uno

exception UnknownPlayer of player

let names = ["Parker"; "David"; "Holly"; "Tyler"; "Jordan"; "Renee"]

let ah_players = [{name = "Jeremy" ; hand = []}; {name = "Gavin" ; hand = []}; 
                  {name = "Geoff" ; hand = []}]

type order =
  | CW
  | CCW

type t = {draw_pile : deck; discard_pile : deck; 
          players: player list; order : order}

let fresh st = {
  draw_pile = shuffle st.draw_pile@(List.tl st.discard_pile);
  discard_pile = [List.hd st.discard_pile];
  players = st.players;
  order = st.order
}

let rec player_update plist p =
  match plist with
  | [] -> raise (UnknownPlayer p)
  | h::t -> if h.name = p.name then p::t 
    else h::(player_update t p)

let rec draw st p n =
  if n = 0 then st 
  else if List.length st.draw_pile <= n 
  then draw (fresh st) p n 
  else
    let st' = {
      draw_pile = List.tl st.draw_pile;
      discard_pile = st.discard_pile;
      players = player_update st.players 
          {name = p.name; hand = (List.hd st.draw_pile)::p.hand};
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