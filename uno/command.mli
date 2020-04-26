type object_phrase = string list

type command = 
  | Play of object_phrase
  | Draw of object_phrase
  | Set of object_phrase
  | Playable

exception Empty

exception Malformed

val parse : string -> command