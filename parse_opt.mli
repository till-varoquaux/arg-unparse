(*
  Posix specification:

  http://www.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap12.html

  GNU specification (adds long options to posix):
  http://www.gnu.org/s/libc/manual/html_node/Argument-Syntax.html#Argument-Syntax

*)

(* An option. *)
type 'a t

val create :
  ?group:string
  -> ?short:char
  -> arg:[ `No_arg of 'a | `Arg of string * (string -> 'a) ]
  -> descr:string
  -> string
  -> 'a t

val long : _ t -> string

val to_string : _ t -> string

type 'a res =
  | Eof
  | Annon of string
  | Flag of 'a

type 'a cursor

(* Cursor based reading... *)
val init : 'a t list -> string list -> 'a cursor
val get : 'a cursor -> 'a res

val print_help_msg :
  ?header:string
  -> ?footer:string
  -> ?program_name:string
  -> ?usage:string
  -> _ t list
  -> unit

val get_all : 'a t list -> string list -> string list * 'a list

(** Multi.... *)
module Multi : sig

  type 'a t = {
    name : string;
    group : string option;
    choice : string list -> 'a;
    descr : string
  }

  val run :
    ?program_name:string
    -> ?args:string list
    -> 'a t list
    -> 'a
end
