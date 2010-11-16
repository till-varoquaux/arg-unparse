(**
   Explanation
   _ Basic
   _ Adding unit: working around the value restriction
   _ Normal map -> Cps map
   _ Adding fail: (choice combinators) -> option
   _ Using a forall type: Hiding the ret type
   _ Adding flags
   _ Phantom type
*)
open StdLabels
open MoreLabels

exception Not_enough_arguments
exception Extra_arguments of string list

let option_map ~f = function
  | None ->  None
  | Some v ->  Some (f v)

type flag_value = string * string list

let flag_to_string = function
  | f,[]  -> f
  | f,[v] -> Printf.sprintf "%s:%s\n%!" f v
  | _,_   -> assert false

let inputs_to_string ~flagged ~annon =
  Printf.sprintf "%s||%s"
    (String.concat ~sep:"," annon)
    (String.concat ~sep:"," (List.map ~f:flag_to_string flagged))

type perm = [ `Parse_opt | `Scalar | `Non_linear | `Linear ]
type scalar = perm
type normal = [ `Non_linear | `Linear ]
type linear = [ `Linear ]
type flag = [ `Linear | `Parse_opt ]

(*
  If we could mark all the arguments as covariant we would be able to relax the
  value restriction (like in many holes in hindley milner)
*)
type ('a,'b) simple = acc:'a Staged.t ->  flag_value list -> string list ->  'b
type (+'perm,'acc,'cont) t = {
  f:'ret.cont:('cont,'ret) simple
    -> ?fallback:('acc,'ret) simple
    ->  ('acc,'ret) simple;
  flags : (string*flag_value Parse_opt.flag) list;
  gram  : Doc.t }
constraint 'perm = [< perm > `Linear ]

let dbg name v =
  let print flagged annon =
    Printf.printf "%s::%s::%s::%s\n%!"
      name
      (String.concat ~sep:"," (List.map ~f:fst v.flags))
      (Doc.to_string v.gram)
      (inputs_to_string ~flagged ~annon)
  in
  { v with f =
      (fun ~cont ?fallback ~acc flagged annon ->
         print flagged annon;
         v.f ~cont ?fallback ~acc flagged annon)}

let (++) f g =
  { flags =f.flags @ g.flags;
    gram = Doc.(f.gram ++ g.gram);
    f = (fun ~cont ->  f.f ~cont:(g.f ~cont ?fallback:None))}

let map_cont ~f v =
  { v with f = fun ~cont ->  v.f ~cont:(fun ~acc -> cont ~acc:(f acc)) }

let cps_map ~f v =
  { v with f =
      fun ~cont ?fallback ~acc ->
        let fallback = option_map fallback ~f:(fun f ->  fun ~acc:_ ->  f ~acc) in
        v.f ~acc:(f acc) ~cont ?fallback}

let create ~name f =
  { gram = Doc.create name;
    flags=[];
    f = fun ~cont ?fallback ~acc flagged annon ->
      let run annon =
        try
          match annon with
          | []  ->  `Err Not_enough_arguments
          | h::t -> `Ok (Staged.apply acc (f h),t)
        with e -> `Err e
      in
      match run annon with
      | `Ok (acc,l) -> cont ~acc flagged l
      | `Err e      ->
          match fallback with
          | Some v ->  v ~acc flagged annon
          | None   ->  raise e }

let const v = {
  gram = Doc.empty;
  flags = [];
  f = fun ~cont ?fallback:_ ~acc ->  cont ~acc:(Staged.apply acc v)}

let gram v = v.gram
let flags v = List.map ~f:snd v.flags
let set_gram v gram = {v with gram}

let flag ?short ?group ~descr long v =
  let arg =
    (* TODO: there's got to be a better way to do this *)
    if v.gram = Doc.empty then `No_arg (long,[])
    else `Arg ((Doc.to_string v.gram),(fun v -> long,[v]))
  in
  let flag = Parse_opt.flag ?short ?group ~arg ~descr long in
  { gram = Doc.empty;
    flags = [long,flag];
    f = fun ~cont ?fallback:_ ~acc flagged annon ->
        if List.mem_assoc long ~map:flagged then
          let args_list = List.assoc long flagged in
          v.f [] args_list
            ~acc:(Staged.map acc ~f:(fun f v -> f (Some v)))
            ~cont:(fun ~acc _flagged left_over ->
              assert (left_over = []);
              cont ~acc flagged annon)
        else
          cont ~acc:(Staged.apply acc None) flagged annon}

let (<|>) x y =
  { gram = Doc.(x.gram <|> y.gram);
    flags = [];
    f = fun ~cont ?fallback ->  x.f ~cont ~fallback:(y.f ~cont ?fallback)}

(* TODO: flag choice using lists*)
(* Flag choice. (Fiendish one...)
   x and y are both flags (or list of flags) that injects towards the same value
   we will keep the one that corresponds to the last CLI flag to match...

*)
let (<!>) x y =
  let flags = x.flags @ y.flags in
  { gram = Doc.empty; (* We are working on flags; the usage gram is empty*)
    flags;
    f = fun ~cont ?fallback ~acc flagged annon ->
      let matches long (long',_flag) =  long' = long in
      let v =
        List.fold_left flagged
          ~f:(fun acc (long,_) ->
                if List.exists ~f:(matches long) x.flags then
                  x
                else if List.exists ~f:(matches long) y.flags then
                  y
                else
                  acc)
          ~init:(const None)
      in
      v.f ~acc ~cont ?fallback flagged annon}

let list v =
  let rec get_args flag_acc ~acc ~cont =
    v.f
      ~acc:(Staged.create (fun x ->  x::flag_acc))
      ~cont:(fun ~acc:flag_acc ->  get_args ~cont ~acc (Staged.run flag_acc))
      ~fallback:(fun ~acc:_ -> cont ~acc:(Staged.apply acc (List.rev flag_acc)))
  in
  { gram = Doc.(list v.gram);
    flags = [];
    f = fun ~cont ?fallback:_ ~acc ->  get_args [] ~acc ~cont }

let endf ~acc _flagged annon =
  if annon <> [] then
    raise (Extra_arguments annon);
  acc

let parse ~flags annon fmt f =
  fmt.f ~acc:(Staged.create f) ~cont:endf flags annon
