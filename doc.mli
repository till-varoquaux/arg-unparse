type t

val create : string -> t
val list : t -> t
val (++) : t -> t -> t
val (<|>) : t -> t -> t
val opt : t -> t
val empty : t
val to_string : t -> string
