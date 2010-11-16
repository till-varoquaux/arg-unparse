open StdLabels
open MoreLabels

exception Bad of string
exception Help of string

let raise_bad fmt = Printf.ksprintf (fun s -> raise (Bad s)) fmt
let raise_help ?header ?footer ?synopsis ~name arguments =
  let msg = Parse_opt__usage_msg.usage ?header
    ?footer
    ?synopsis
    ~name
    arguments
  in raise (Help msg)

let argv () : string list = List.tl (Array.to_list Sys.argv)
let program_name () : string = Filename.basename (Sys.executable_name)

let take_from ~pos s = String.sub s ~pos ~len:(String.length s - pos)

(** Split a long flag aroung the = sign *)
let split_long (s:string) : string * string option =
  try
    let idx = String.index s '=' in
    let flag = String.sub s ~len:idx ~pos:0 in
    let len = String.length s in
    let arg = String.sub s ~pos:(idx+1) ~len:(len -idx-1) in
    flag,Some arg
  with Not_found ->
    s,None

let is_short_flag (s:string) : bool =
  String.length s >= 2 && s.[0] = '-' && s.[1] <> '-'

let is_long_flag (s:string) : bool =
  String.length s >= 3 && s.[0] = '-' && s.[1] = '-'

(* Parsing *)
type 'a res = (* Result of calling the state machine *)
  | Eof
  | Annon of string
  | Flag of 'a

type 'a arg_descr = [
  | `No_arg of 'a
  | `Arg    of string * (string -> 'a)
]

(** { Parsing the argument as a state machine.} *)
type state_no_error = [
| `St_normal of string list (* Starting state. *)
| `St_short  of (int * string * string list) (* Parsing a short argument *)
      (*
         ... -acjl ...
                ^
      *)
| `St_annon of string list (* *)
      (* We've already seen "--"
         all arguments from now on are anonymous
         ... -- ... v ...
                    ^
      *)
]

type state = [ `St_error of exn | state_no_error ]

(** This represents a type. All flags *)
type 'a flag = {
  long    : string;
  descr   : string;
  arg     : [ 'a arg_descr | `Help ];
  group   : string option;
  short   : char option;
}

type 'a cursor = {
  name : string;
  footer : string option;
  header : string option;
  synopsis : string option;
  flags : 'a flag list;
  mutable state : state;
}

let help_flag = {
  group=None;
  long="help";
  short=Some 'h';
  arg=`Help;
  descr="print this help message"
}

let find_long cursor (name:string) : 'a flag =
  let stripped = String.sub name ~pos:2 ~len:(String.length name - 2) in
  try
    List.find ~f:(fun x -> x.long = stripped) cursor.flags
  with Not_found ->
    raise_bad "Unknown flag %s" name

let find_short cursor (name:char) : 'a flag =
  try
    List.find ~f:(fun x -> x.short = Some name) cursor.flags
  with Not_found ->
    raise_bad "Unknown flag -%c" name

type 'a transition = state * 'a res

let help cursor =
  let has_short =
    List.exists cursor.flags ~f:(fun f -> f.short <> None)
  in
  let flag_usage flag =
    let long =
      match flag.arg with
      | `Help | `No_arg _  -> Printf.sprintf "--%s" flag.long
      | `Arg (v,_)         -> Printf.sprintf "--%s=%s" flag.long
          (String.uppercase v)
    in
    match flag with
    | {short = None;_} when has_short -> "   " ^ long
    | {short = None;_}               -> long
    | {short = Some short;_}         -> Printf.sprintf "-%c %s" short long
  in
  raise_help
    ?header:cursor.header
    ?footer:cursor.footer
    ?synopsis:cursor.synopsis
    ~name:cursor.name
    ["Flags",
     (List.map cursor.flags
        ~f:(fun f -> f.group,flag_usage f,f.descr))]

(** A short flag taking an argument:
    [f] : function to build the return value *)
let short_arg ~name f : _ -> _ transition = function
  | `St_short (pos,s,l) -> `St_normal l,Flag (f (take_from s ~pos))
  | `St_normal (arg::t) -> `St_normal t,Flag (f arg)
  | `St_normal []       -> raise_bad "missing argument for flag -%c" name

(** a short flag (in a string)
    [f] the string containing the short flag (e.g. -avz)
    [pos] the position in that string
    [l] the list of remaining arguments
*)
let short cursor pos f l : _ transition =
  let state =
    if pos+1 >= String.length f then `St_normal l
    else `St_short (pos+1,f,l)
  in
  let name = f.[pos] in
  let flag = find_short cursor name in
  match flag.arg with
  | `Arg (_,f) -> short_arg ~name f state
  | `No_arg v  -> state,Flag v
  | `Help -> help cursor

(* The start state of our FSM;
   [v] the argument we are currently parsing
   [args] the remaining arguments
*)
let normal cursor v args : _ transition =
  if is_short_flag v then
    short cursor 1 v args (* 1 is to skip the - sign *)
  else if is_long_flag v then
    let flag_name,arg = split_long v in
    let flag = find_long cursor flag_name in
    match flag.arg,arg,args with
    | `Help,_,_            -> help cursor
    | `Arg (_,f),Some v,l
    | `Arg (_,f),None,v::l -> `St_normal l,Flag (f v)
    | `No_arg v,None,l     -> `St_normal l,Flag v
    | `Arg _,None,[]       -> raise_bad "Missing argument for flag %s" flag_name
    | `No_arg _ ,Some _,_  -> raise_bad "Flag %s doesn't take an argument" flag_name
  else
    `St_normal args,Annon v

let next cursor = function
  | `St_annon (h::t)                      -> `St_annon t,Annon h
  | `St_normal ([]|["--"]) | `St_annon [] -> `St_annon [],Eof
  | `St_normal ("--"::v::l)               -> `St_annon l,Annon v
  | `St_normal (v::args)                  -> normal cursor v args
  | `St_short (pos,f,l)                   -> short cursor pos f l

let get cursor =
  match cursor.state with
  | `St_error e -> raise e
  | #state_no_error as state ->
      try
        let state,res = next cursor state in
        cursor.state <- state;
        res
      with e ->
        cursor.state <- `St_error e;
        raise e

let flag ?group ?short ~(arg:_ arg_descr) ~descr long = {
  long;descr;group;short;
  arg=(arg:>[`Help|_ arg_descr])
}

let long (v:_ flag) : string = v.long

let init ?synopsis ?header ?footer ?(name=program_name ()) ?(args=argv ()) flags
    =
  let flags = help_flag :: flags in
  { header; footer;synopsis;
    name;
    flags;
    state = `St_normal args }

let to_string f =
  let main =
    match f with
    | {short = None;_} -> Printf.sprintf "--%s" f.long
    | {short = Some short;_} ->
        Printf.sprintf "-%c --%s" short f.long
  in
  match f with
  | {arg = (`Help | `No_arg _) ;_} -> main
  | {arg = `Arg (g,_);_}           -> main ^ " " ^ g

let run ?synopsis ?header ?footer ?name ?(flags=[]) ?args ?(exit_on_error=true)
    ()
    =
  let cursor = init ?name ?args ?header ?footer ?synopsis flags in
  try
    let rec loop flags annon =
      match get cursor with
      | Annon a -> loop flags (a :: annon)
      | Flag v -> loop (v :: flags) annon
      | Eof ->   List.rev annon,List.rev flags
    in
    loop [] []
  with
  | Help s when exit_on_error ->
      print_endline s;
      exit 0
  | Bad s when exit_on_error  ->
      prerr_endline s;
      exit 1

let is_prefix (needle:string) (haystack:string) : bool =
  let len = String.length needle in
  len <= String.length haystack && (
    String.sub haystack ~pos:0 ~len = needle  )

(** Prints *)
type 'a cmd = {
  name : string;
  group : string option;
  choice : name:string -> args:string list -> 'a;
  descr : string }

let rec find_by_name__loop acc needle = function
  | ({name;_} as v)::_ when name = needle -> v
  | ({name;_} as v)::l when is_prefix needle name ->
      find_by_name__loop (v::acc) needle l
  | _::l -> find_by_name__loop acc needle l
  | [] ->
      match acc with
      | [v] -> v
      | []  -> raise_bad "Unknown command %S\n" needle
      | l   -> raise_bad "Ambiguous command\n\n%S could be one of\n%s\n"
          needle
          (String.concat ~sep:" " (List.map ~f:(fun v -> v.name) l))

let find_by_name (name:string) (l:'a cmd list) =
  find_by_name__loop [] name l

let multi_run
    ?(name=program_name ())
    ?(args=argv ())
    ?(exit_on_error=true)
    choices =
  try
    match args with
    | [] | "help"::_ | "--help"::_ ->
        raise_help
          ~name
          ["Commands",
           List.map choices
             ~f:(fun v -> v.group,v.name,v.descr)]
    | h::t ->
        let v = find_by_name h choices in
        v.choice ~name:(name ^ " " ^ h) ~args:t
  with
  | Help s when exit_on_error ->
      print_endline s;
      exit 0
  | Bad s when exit_on_error  ->
      prerr_endline s;
      exit 1
