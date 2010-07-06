(* The staged module is used to force computations to be staged. It takes a
   a function and allows to "freeze" a function and delay all computations
   whilst still applying it to arguments. *)

type +'a t
val apply : ('a -> 'b) t -> 'a -> 'b t
val map : f:('a -> 'b) -> 'a t -> 'b t
val run : 'a t -> 'a
val create : 'a -> 'a t
