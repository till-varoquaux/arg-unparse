type flag_value

type perm = [ `Parse_opt | `Scalar | `Non_linear | `Linear ]
type scalar = perm
type normal = [ `Non_linear | `Linear ]
type linear = [ `Linear ]
type flag = [ `Parse_opt | `Linear ]

type (+'perm,'acc,'cont) t constraint 'perm = [< perm > `Linear]

exception Not_enough_arguments
exception Extra_arguments of string list

(* Force the scalar flag to go *)
val (++) : ([< normal] as 'perm,'a,'b) t -> ('perm,'b,'c) t -> ('perm,'a,'c) t

val (<|>) :
  ([> normal ],'b,'a) t
  -> ([> normal ],'b,'a) t
  -> ([< normal],'b,'a) t

val (<!>) :
  (flag,'a option -> 'k,'k) t
  -> (flag,'a option -> 'k,'k) t
  -> ([< flag],'a option -> 'k,'k) t

val const : 'a -> ([< scalar],'a -> 'k,'k) t

val create : name:string -> (string -> 'a) -> ([< scalar ],'a -> 'b,'b) t

(* TODO: Delete me *)
val gram : (_,_,_) t -> Doc.t
val flags : (_,_,_) t -> flag_value Parse_opt.flag list

(* TODO: delete me *)
val set_gram : ('a,'b,'c) t -> Doc.t -> ('a,'b,'c) t

val map_cont :
  f:('a Staged.t -> 'b Staged.t) -> ('perm, 'c, 'a) t -> ('perm, 'c, 'b) t

val cps_map:
  f:('a Staged.t -> 'b Staged.t) -> ('perm, 'b, 'c) t -> ('perm, 'a, 'c) t

val flag :
  ?short:char
  -> ?group : string
  -> descr : string
  -> string
  -> ([> `Scalar],'a -> 'k,'k) t
  -> ([< flag ],'a option -> 'k,'k) t

val list :
  ([> normal],'a -> 'a list,'a list) t
  -> ([< normal ],'a list -> 'k,'k) t

val parse :
  flags: flag_value list
  -> string list
  -> (_,'a,'b) t
  -> 'a
  -> 'b Staged.t
