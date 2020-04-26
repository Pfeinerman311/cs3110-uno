open Uno

exception UnknownPlayer
exception EmptyPile

type order =
  | CW
  | CCW

type t = {draw_pile : deck; discard_pile : deck; 
          players: player list; order : order}

val top_card : t -> card

val next_draw : t -> card

val draw : t -> player -> int -> t

val init : string -> t

val playable_cards : card list -> card -> card list

val wild_set : t -> color -> t

val play : t -> player -> card -> t

val auto_play : t -> player -> t

val next_player : t -> player

val last_players : t -> player list

val turn : t -> t