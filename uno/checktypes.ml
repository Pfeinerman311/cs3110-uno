(**
   module type UnoSig = sig
   type t
   type room_id = string
   type exit_name = string
   exception UnknownRoom of room_id
   exception UnknownExit of exit_name
   val start_room : t -> room_id
   val room_ids : t -> room_id list
   val description : t -> room_id -> string
   val exits : t -> room_id -> exit_name list
   val next_room : t -> room_id -> exit_name -> room_id
   val next_rooms : t -> room_id -> room_id list
   end

   module UnoCheck : UnoSig = Uno

   module type CommandSig = sig
   type object_phrase = string list
   type command = 
    | Play of object_phrase
    | Draw of object_phrase
    | Set of object_phrase
   exception Empty
   exception Malformed
   val parse : string -> command
   end

   module CommandCheck : CommandSig = Command

   module type StateSig = sig
   type t 
   val init_state : Adventure.t -> t
   val current_room_id : t -> string
   val visited : t -> string list
   type result = Legal of t 
              | Illegal
              | Win
   val go : Adventure.exit_name -> Adventure.t -> t -> result
   end

   module StateCheck : StateSig = State

*)

module type AuthorSig = sig
  val hours_worked : int
end

module AuthorCheck : AuthorSig = Author
