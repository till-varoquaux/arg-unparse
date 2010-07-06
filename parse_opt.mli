type t = {
  descr   : string;
  arg     : string option;
  group   : string option;
  long    : string;
  short   : char option
}

type res =
  | Eof
  | Annon of string
  | Flag of t * string option

type acc

val input : t list -> string list -> acc

val get : acc -> res
val help_msg : ?header:string -> ?footer:string -> usage:string -> t list -> unit
val to_string : t -> string

val get_all : t list -> string list -> string list * (t * string option) list

(** Multi.... *)
module Multi : sig

  type 'a t = {
    name : string;
    group : string option;
    choice : string list -> 'a;
    descr : string
  }

  val run : ?name:string -> ?args:string list -> 'a t list -> 'a
end
