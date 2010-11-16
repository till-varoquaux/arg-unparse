type group = string
type name = string
type descr = string
val usage :
  ?header:string ->
  ?footer:string ->
  ?synopsis:string ->
  name:string ->
  (string * (group option * name * descr) list) list -> string
