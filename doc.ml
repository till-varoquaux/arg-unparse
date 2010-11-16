(** Used to document modules...*)
type t =
  | Seq of t * t
  | Or of t * t
  | List of t
  | Opt of t
  | Leaf of string
  | Empty

let rec to_string__loop ?(in_seq=false) = function
  | Seq (l,r) ->
      to_string__loop ~in_seq:true l ^" "^ to_string__loop ~in_seq:true r
  | Leaf s -> s
  | Or (_) as x when in_seq -> "(" ^ to_string__loop x ^ ")"
  | Or (l,r) -> to_string__loop l ^ "|" ^ to_string__loop r
  | List x -> "[" ^ to_string__loop x ^ "]..."
  | Opt x -> "[" ^ to_string__loop x ^ "]"
  | Empty -> ""

let to_string v = to_string__loop v

let (++) x y =
  match x,y with
  | x,Empty | Empty,x -> x
  | _ -> Seq (x,y)

let (<|>) x y =
  match x,y with
  | x,Empty | Empty,x -> x
  | _ ->  Or (x,y)

let create s = Leaf s

let seq x y =
  match x,y with
  | x,Empty | Empty,x -> x
  | _ -> Seq (x,y)

let opt = function
  | Empty -> Empty
  | x -> Opt x

let list = function
  | Empty -> Empty
  | x -> List x

let empty = Empty
