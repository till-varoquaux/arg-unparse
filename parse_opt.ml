open StdLabels
open MoreLabels
exception Unknown_flag     of string
exception Missing_argument of string
exception Flag_doesnt_take_an_argument of string

type 'a flag_arg_spec = [
  | `No_arg of 'a
  | `Arg of string * (string -> 'a)
]

type 'a t = {
  long    : string;
  descr   : string;
  arg     : 'a flag_arg_spec;
  group   : string option;
  short   : char option;
}

let create ?group ?short ~arg ~descr long = {
  long;descr;arg;group;short
}

let long v = v.long

let help_msg ?header ?footer ~usage flags =
  let has_short =
    List.exists flags ~f:(fun f -> f.short <> None)
  in
  Util.help_msg
    ?header
    ?footer
    ~usage
    ~name:(fun flag ->
             let long =
               match flag.arg with
               | `No_arg _  -> Printf.sprintf "--%s" flag.long
               | `Arg (v,_) -> Printf.sprintf "--%s=%s" flag.long
                   (String.uppercase v)
             in
             match flag with
             | {short = None;_} when has_short -> "   " ^ long
             | {short = None;_} -> long
             | {short = Some short;_} ->
                 Printf.sprintf "-%c %s" short long)
    ~group:(fun v -> v.group)
    ~descr:(fun v -> v.descr)
    "Flags"
    flags

let to_string_main f =
  match f with
  | {short = None;_} -> Printf.sprintf "--%s" f.long
  | {short = Some short;_} ->
      Printf.sprintf "-%c --%s" short f.long

let to_string f =
  let main = to_string_main f in
  match f with
  | {arg = `No_arg _ ;_} -> main
  | {arg = `Arg (g,_);_} -> main ^ " " ^ g

let is_short s =
  let len = String.length s in
  len >= 2 && s.[0] = '-' && s.[1] <> '-'

let is_long s =
  let len = String.length s in
  len >= 3 && s.[0] = '-' && s.[1] = '-'

let find_long ~flags name =
  let stripped = String.sub name ~pos:2 ~len:(String.length name - 2) in
  try
    List.find ~f:(fun x -> x.long = stripped) flags
  with Not_found ->
    raise (Unknown_flag name)

let find_short ~flags name =
  try
    List.find ~f:(fun x -> x.short = Some name) flags
  with Not_found ->
    raise (Unknown_flag (Printf.sprintf "-%c" name))

type state =
  | St_normal
  | St_short of (int * string)
  | St_annon (* We've already seen -- all arguments from now on are anonymous *)

type 'a res =
  | Eof
  | Annon of string
  | Flag of 'a

type 'a cursor = {
  flags : 'a t list;
  mutable state : state;
  mutable remaining : string list;
}

let short_arg flag state = match state with
  | {state=St_short (pos,f);_} ->
      state.state <- St_normal;
      String.sub f ~pos ~len:(String.length f - pos)
  | {state=St_normal;remaining=h::t;_} ->
      state.remaining <- t;
      h
  | {state=St_annon;_} ->
      (* TODO: comment why this is impossible *)
      assert false
  | {state=St_normal;remaining = [];_} ->
      match flag.short with
      | None -> assert false
      | Some c -> raise (Missing_argument ( Printf.sprintf "-%c" c))

(** Split a long flag aroung the = sign *)
let split_long s =
  try
    let idx = String.index s '=' in
    let flag = String.sub s ~len:idx ~pos:0 in
    let len = String.length s in
    let arg = String.sub s ~pos:(idx+1) ~len:(len -idx-1) in
    flag,Some arg
  with Not_found ->
    s,None

let rec get: 'a.'a cursor -> 'a res = fun state ->
  match state with
  | {state=St_short (pos,f);flags;_} ->
      if pos+1 >= String.length f then begin
        state.state <- St_normal
      end else begin
        state.state <- St_short (pos+1,f)
      end;
      let flag = find_short ~flags f.[pos] in
      Flag (begin match flag.arg with
            | `Arg (_,f) -> f (short_arg flag state)
            | `No_arg v  -> v
            end)
  | { remaining = h::t; state = St_annon;_ } ->
      state.remaining <- t;
      Annon h
  | { remaining = []; state = (St_normal | St_annon);_ } -> Eof
  | { remaining = "--"::x;_ } as v ->
      v.state <- St_annon;
      v.remaining <- x;
      get state
  | { remaining = h::t;state=St_normal;flags} as v ->
      if is_short h then begin
        v.state <- St_short (1,h);
        v.remaining <- t;
        get v
      end else if is_long h then
        let flag_name,arg = split_long h in
        let flag = find_long ~flags flag_name in
        Flag (match flag.arg,arg,t with
              | `Arg (_,f),Some v,l | `Arg (_,f),None,v::l ->
                  state.remaining <- l;
                  f v
              | `No_arg v,None,l ->
                  state.remaining <- l;
                  v
              | `Arg _,None,[] ->
                  raise (Missing_argument flag_name)
              | `No_arg _ ,Some _,_ ->
                  raise (Flag_doesnt_take_an_argument flag_name))
      else begin
        state.remaining <- t;
        Annon h
      end

let init flags remaining = {
  remaining;
  flags;
  state = St_normal
}

(** Grouping *)
let get_all : 'a.'a t list -> string list -> string list * 'a list =
  fun flags args ->
    let input = init flags args in
    let rec loop flags annon =
      match get input with
      | Annon a -> loop flags (a :: annon)
      | Flag v -> loop (v :: flags) annon
      | Eof ->   List.rev annon,List.rev flags
    in
    loop [] []

module Multi = struct
  type 'a t = {
    name : string;
    group : string option;
    choice : string list -> 'a;
    descr : string }

  let argv () = List.tl (Array.to_list Sys.argv)

  let run
      ?(name=Util.program_name ())
      ?(args=argv ())
      choices =
    match args with
    | [] | "help"::_ | "--help"::_ ->
        Util.help_msg
          ~usage:("usage: " ^ name)
          ~name:(fun v -> v.name)
          ~descr:(fun v -> v.descr)
          ~group:(fun v -> v.group)
          "Commands"
          choices;
        exit 0
    | h::t ->
        match Util.find_by_name
          h
          (List.map ~f:(fun v -> v.name,v) choices)
        with
        | `Ambiguous (_,possible) ->
            Printf.eprintf "Ambiguous command\n\n%S could be one of\n%s\n"
              h
              (String.concat ~sep:" " (List.map ~f:fst possible));
            exit 1
        | `Unknown _ ->
            Printf.eprintf "Unknown command %S\n" h;
            exit 1
        | `Found v -> v.choice t
end
