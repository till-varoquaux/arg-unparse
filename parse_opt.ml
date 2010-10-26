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

(** [ pad_right s len ]  returns the string s padded to the length [len] with
    whitespaces added to the left of the string *)
let pad_right (s:string) (length:int) : string =
  if  String.length s >= length then
    s
  else
    let res = String.make length ' ' in
    String.blit ~src:s ~src_pos:0 ~dst:res ~dst_pos:0 ~len:(String.length s);
    res

let group_by ~(f:'a -> 'b) (l:'a list) : ('b * 'a list) list =
  let ht = Hashtbl.create 17 in
  List.iter l ~f:(fun v ->
                  let key = f v in
                  try
                    let previous = Hashtbl.find ht key in
                    Hashtbl.replace ht ~key ~data:(v::previous)
                  with Not_found ->
                    Hashtbl.add ht ~key ~data:[v]
               );
  Hashtbl.fold ht
    ~f:(fun ~key ~data acc -> (key,data)::acc)
    ~init:[]


let program_name () : string =
  Filename.basename (Sys.executable_name)

(** Prints *)
let print_usage_msg
    ?(header:string option)
    ?(footer:string option)
    ?(program_name = program_name ())
    ?(usage:string option)
    ~(group:'a -> string option)
    ~(name:'a -> string)
    ~(descr:'a -> string)
    (typ:string)
    (multi:'a list) : unit =
  let padlen =
    List.fold_left multi
      ~f:(fun acc c -> max acc (String.length (name c)))
      ~init:0
  in
  Printf.printf "Usage: %s" program_name;
  begin match usage with
  | None           -> print_newline ()
  | Some usage_arg -> print_endline (" " ^ usage_arg)
  end;
  begin match header with
  | None -> ()
  | Some s ->
      print_newline ();
      print_endline s;
  end;
  List.iter (List.sort ~cmp:compare (group_by multi ~f:group))
    ~f:(fun (group,multi) ->
          print_newline ();
          begin match group with
          | Some v -> print_endline (v ^ ":")
          | None -> print_endline (typ ^ ":")
          end;
          List.iter multi
            ~f:(fun f ->
                  Printf.printf "%s  %s\n"
                    (pad_right (name f) padlen)
                    (descr f)));
  begin match footer with
  | None -> ()
  | Some s ->
      print_newline ();
      print_endline s
  end

let print_help_msg
    ?(header:string option)
    ?(footer:string option)
    ?(program_name:string option)
    ?(usage:string option)
    (flags:'a list) : unit =
  let has_short =
    List.exists flags ~f:(fun f -> f.short <> None)
  in
  print_usage_msg
    ?header
    ?footer
    ?program_name
    ?usage
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
  let is_prefix (needle:string) (haystack:string) : bool =
    let len = String.length needle in
    len <= String.length haystack && (
      String.sub haystack ~pos:0 ~len = needle  )

  let rec find_by_name__loop acc name = function
    | (k,v)::_ when k = name -> `Found v
    | (k,_ as v)::l when is_prefix name k -> find_by_name__loop (v::acc) name l
    | _::l -> find_by_name__loop acc name l
    | [] ->
        match acc with
        | [_,v] -> `Found v
        | [] ->  `Unknown name
        | l -> `Ambiguous (name,l)

  let find_by_name (name:string) (l:(string * 'a) list) =
    find_by_name__loop [] name l

  type 'a t = {
    name : string;
    group : string option;
    choice : string list -> 'a;
    descr : string }

  let argv () = List.tl (Array.to_list Sys.argv)

  let run
      ?(program_name:string option)
      ?(args=argv ())
      choices =
    match args with
    | [] | "help"::_ | "--help"::_ ->
        print_usage_msg
          ?program_name
          ~name:(fun v -> v.name)
          ~descr:(fun v -> v.descr)
          ~group:(fun v -> v.group)
          "Commands"
          choices;
        exit 0
    | h::t ->
        match find_by_name
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
