type color =
  | Red
  | Yellow
  | Green
  | Blue
  | Wild

type action = Zero | One | Two | Three | Four | Five | Six | Seven | Eight 
            | Nine | Skip | Reverse | Draw of int | None

type card = color * action

type hand = card list

type player = {name : string; hand : hand}

type deck = card list

exception IllegalMove of card
exception NoCards
exception UnknownCard

val full_deck : deck

val shuffle : deck -> deck

val next_card : deck -> card

val last_cards : deck -> deck

val card_color : card -> color

val card_action : card -> action

val playable : card -> card -> bool

val rem_card : card -> deck -> deck

val add_card : card -> deck -> deck

val count : deck -> int