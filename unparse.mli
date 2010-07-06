 (* Scalar means it can be given as an argument to a flag: it consumes one and
   only one argument*)
type perm = [ `Parse_opt | `Scalar | `Non_linear | `Linear ]

type ('perm,'acc,'cont) t constraint 'perm = [< perm > `Linear ]

val int : string -> (_,int -> 'k,'k) t
val string : string -> (_,string -> 'k,'k) t
val float : string -> (_,float -> 'k,'k) t

val const : 'a -> ([< `Scalar | `Non_linear | `Linear ],'a -> 'k,'k) t


(* Sequence; we are restricting the type of 'perm via
   the [... as 'perm] (i.e. the 'perm in the result will be forced to be subset
   of [ `Non_linear | `Linear ]) *)
val (++) : ([< `Non_linear | `Linear ] as 'perm, 'a, 'b) t
  -> ('perm, 'b, 'c) t
  -> ('perm, 'a, 'c) t

(* Or choice; doesn't work on flags (see <!>) *)
(* This annoyingly kills the variance *)
val (<|>) :
  ([> `Non_linear | `Linear ] as 'perm,'b,'a) t
  -> ([> `Non_linear | `Linear ],'b,'a) t
  -> ([< `Non_linear | `Linear ],'b,'a) t

val flag :
  ?short:char
  -> ?group : string
  -> descr : string
  -> string
  -> ([> `Scalar],'a -> 'k,'k) t
  -> ([< `Parse_opt | `Linear],'a option -> 'k,'k) t

val bool_flag :
  ?short:char
  -> ?group : string
  -> descr : string
  -> string
  -> ([< `Parse_opt | `Linear],bool -> 'k,'k) t

val (<!>):
  ([`Parse_opt | `Linear],'a option -> 'k,'k) t
  -> ([`Parse_opt | `Linear],'a option -> 'k,'k) t
  -> ([< `Parse_opt | `Linear],'a option -> 'k,'k) t

val option : ([>`Non_linear ],'a -> 'k Staged.t,'k Staged.t) t
  -> ([< `Non_linear | `Linear],'a option -> 'k,'k) t

val list : ([> `Non_linear],'a -> 'a list,'a list) t
  -> ([< `Non_linear | `Linear > `Linear ],'a list -> 'k,'k) t

val non_empty_list : ([> `Non_linear ] ,'a -> 'a list,'a list) t
   -> ([< `Non_linear | `Linear],'a list -> 'k,'k) t

val mapf : f:('a -> 'b) -> ('perm, 'b, 'c) t -> ('perm, 'a, 'c) t

val iter : f:('a -> unit)
  -> ('perm,'a -> 'k Staged.t,'k Staged.t) t
  -> ('perm,'k,'k) t

val map : f:('a -> 'b)
  -> ('perm,'a -> 'k Staged.t,'k Staged.t) t
  -> ('perm,'b -> 'k,'k) t

val map2 : f:('a -> 'b -> 'c)
  -> ('perm,'a -> 'b -> 'k Staged.t,'k Staged.t) t
  -> ('perm,'c -> 'k,'k) t

val map3 : f:('a -> 'b -> 'c -> 'd)
  -> ('perm,'a -> 'b -> 'c -> 'k Staged.t,'k Staged.t) t
  -> ('perm,'d -> 'k,'k) t

val map' :
  f:('a Staged.t -> 'b)
  -> ('perm, 'b, 'k Staged.t) t
  -> ('perm, 'a, 'k) t

val run : ?name:string -> ?args:string list -> ([ `Linear ],'a,'b) t -> 'a -> 'b

type 'a choice

val choice :
  ?group:string
  -> descr:string
  -> name:string
  -> f:'a
  -> ([ `Linear ],'a,'b) t
  -> 'b choice

val multi_run : ?name:string -> 'a choice list -> 'a
