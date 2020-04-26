type object_phrase = string list

type command = 
  | Play of object_phrase
  | Draw of object_phrase
  | Set of object_phrase

exception Empty

exception Malformed

let rec parse_helper inp =
  match inp with
  | [] ->[]
  | h::t -> if h = "" then parse_helper t else
      h::(parse_helper t)

let parse str =
  let inp  = String.split_on_char ' ' str in
  match (parse_helper inp) with
  | [] -> raise Empty
  | h::t -> if h = "play" && t != [] then Play t else
    if h = "draw" then Draw t else
    if h = "set" && t!= [] then Set t else
      raise Malformed