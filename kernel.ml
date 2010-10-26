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

type option_value = string * string option

type inputs = {
  i_annon : string list;
  i_flags : option_value list
}

let flag_to_string = function
  | f,None   -> f
  | f,Some v -> Printf.sprintf "%s:%s\n%!" f v

let inputs_to_string inputs =
  Printf.sprintf "%s||%s"
    (String.concat ~sep:"," inputs.i_annon)
    (String.concat ~sep:"," (List.map ~f:flag_to_string inputs.i_flags))

type perm = [ `Parse_opt | `Scalar | `Non_linear | `Linear ]
type scalar = perm
type normal = [ `Non_linear | `Linear ]
type linear = [ `Linear ]
type flag = [ `Linear | `Parse_opt ]

(*
  If we could mark all the arguments as covariant we would be able to relax the
  value restriction (like in many holes in hindley milner)
*)
type (+'perm,-'acc,+'cont) t = {f:'ret.
  inputs
  -> acc:'acc Staged.t
  -> cont:(inputs -> acc:'cont Staged.t -> fail:(exn -> 'ret) -> 'ret)
  -> fallback:(unit -> 'ret) option
  -> fail:(exn ->'ret)
  -> 'ret;
  flags: option_value Parse_opt.t list;
  gram  : Doc.t }
constraint 'perm = [< perm > `Linear ]

let dbg name v =
  let print inputs =
    Printf.printf "%s::%s::%s::%s\n%!"
      name
      (String.concat ~sep:"," (List.map ~f:Parse_opt.to_string v.flags))
      (Doc.to_string v.gram)
      (inputs_to_string inputs)
  in
  { v with f =
      (fun inputs ~acc ~cont ~fallback ~fail ->
         print inputs;
         v.f inputs ~acc ~cont ~fallback ~fail)}

let (++) f g =
  { flags =f.flags @ g.flags;
    gram = Doc.(f.gram ++ g.gram);
    f =
      (fun inputs ~acc ~cont ~fallback ~fail ->
        f.f ~fallback inputs
          ~acc
          ~cont:(fun inputs ~acc ~fail ->
                   g.f inputs ~acc ~fallback:None ~cont ~fail) ~fail)}

let map_cont ~f v = { v with
    f = fun inputs ~acc ~cont ~fallback ~fail ->
      v.f
        ~fail
        ~fallback inputs
        ~acc
        ~cont:(fun inputs ~acc ~fail ->
                 let v = try `Ok (f acc) with e -> `Err e in
                 match v with
                 | `Ok acc -> cont inputs ~acc ~fail
                 | `Err e -> fail e )}

let create ~name f =
  { gram = Doc.create name;
    flags=[];
    f = fun inputs ~acc ~cont ~fallback ~fail ->
      let run inputs =
        try
          let (v,l) =
            match inputs.i_annon with
            | []  -> raise Not_enough_arguments
            | h::t -> f h,t
          in `Ok (Staged.apply acc v,l)
        with e -> `Err e
      in
      match run inputs with
      | `Ok (acc,l) -> cont {inputs with i_annon = l} ~acc ~fail
      | `Err e      ->
          match fallback with
          | Some v -> v ()
          | None   -> fail e }

let const v = {
  gram = Doc.empty;
  flags = [];
  f = fun inputs ~acc ~cont ~fallback:_ ~fail ->
        cont inputs ~acc:(Staged.apply acc v) ~fail }

let gram v = v.gram
let flags v = v.flags
let set_gram v gram = { v with gram}

let flag ?short ?group ~descr long v =
  let arg =
    if v.gram = Doc.empty then `No_arg (long,None)
    else `Arg ((Doc.to_string v.gram),(fun v -> long,Some v))
  in
  let flag = Parse_opt.create ?short ?group ?arg ~descr long in
  { gram = Doc.empty;
    flags = [flag];
    f =  fun inputs ~acc ~cont ~fallback:_ ~fail ->
        if List.mem_assoc long ~map:inputs.i_flags then
          let args_list = match List.assoc long inputs.i_flags with
            | Some v -> [v]
            | None -> []
          in
          v.f { i_flags = [] ; i_annon = args_list }
            ~fallback:None
            ~fail
            ~acc:(Staged.map acc ~f:(fun f v -> f (Some v)))
            ~cont:(fun left_over_inputs ~acc ~fail:_ ->
                     assert
                       (left_over_inputs.i_annon = []);
                   cont ~acc ~fail inputs)
        else
          cont inputs ~acc:(Staged.apply acc None) ~fail }

let cps_map ~f v =
  { v with
    f = fun inputs ~acc ~cont ~fallback ~fail ->
      let res = try `Ok (f acc) with e -> `Err e in
      match res with
      | `Ok acc -> v.f inputs ~acc ~cont ~fallback ~fail
      | `Err e -> fail e}

let (<|>) x y =
  { gram = Doc.(x.gram <|> y.gram);
    flags = [];
    f = fun inputs ~acc ~cont ~fallback ~fail ->
          x.f inputs ~acc ~cont
            ~fallback:(Some (fun () -> y.f inputs ~acc ~cont ~fallback ~fail))
            ~fail }

(* Flag choice. (Fiendish one...) *)
let (<!>) x y =
  let flags = x.flags @ y.flags in
  { gram = Doc.empty;
    flags;
    f = fun inputs ~acc ~cont ~fallback ~fail ->
      let matches long (f:_ Parse_opt.t) =
        Parse_opt.long f = long
      in
      let v =
        List.fold_left inputs.i_flags
          ~f:(fun acc (long,_) ->
                if List.exists ~f:(matches long) x.flags then
                  x
                else if List.exists ~f:(matches long) y.flags then
                  y
                else
                  acc
             )
          ~init:(const None)
      in
      v.f inputs ~acc ~cont ~fallback ~fail }

let list v =
  let rec get_args ~fail acc inputs =
    v.f inputs
      ~acc:(Staged.create (fun x -> x::acc))
      ~cont:(fun inputs ~acc ~fail -> get_args ~fail (Staged.run acc) inputs)
      ~fallback:(Some (fun () -> (List.rev acc),inputs))
      ~fail
  in
  { gram = Doc.(list v.gram);
    flags = [];
    f = fun inputs ~acc ~cont ~fallback:_ ~fail ->
      let v =
        try
          `Ok (get_args ~fail:raise [] inputs)
        with e -> `Fail e
      in
      match v with
      | `Fail e -> fail e
      | `Ok (v,inputs) -> cont inputs ~acc:(Staged.apply acc v) ~fail }

let endf inputs ~acc ~fail =
  if inputs.i_annon <> [] then
    fail (Extra_arguments inputs.i_annon)
  else
    Staged.run acc

let parse ?(fail=raise) ~flags annon fmt f =
  let inputs = {
    i_flags = flags;
    i_annon = annon }
  in
  fmt.f inputs ~fallback:None ~acc:(Staged.create f) ~cont:endf ~fail
