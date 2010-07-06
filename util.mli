open StdLabels

(** Pads a string to a given length *)
val pad_right : string -> int -> string

val group_by : f:('a -> 'b) -> 'a list -> ('b * 'a list) list

val find_by_name : string -> (string * 'a) list ->
  [ `Ambiguous of string * (string * 'a) list
  | `Found of 'a
  | `Unknown of string ]

val (^%^) : string -> string -> string

val program_name : unit -> string

val help_msg :
  ?header:string
  -> ?footer:string
  -> usage:string
  -> group:('a -> string option)
  -> name:('a -> string)
  -> descr:('a -> string)
  -> string
  -> 'a list
  -> unit
