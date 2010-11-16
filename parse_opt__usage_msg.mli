type group = string
type name = string
type descr = string

(** [usage ~header ~footer ~synopsis ~name arguments]


    Contructs a usage message 
*)
val usage :
  ?header:string ->
  ?footer:string ->
  ?synopsis:string ->
  name:string ->
  (string * (group option * name * descr) list) list -> string
