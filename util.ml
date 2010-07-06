open StdLabels
open MoreLabels

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

let (^%^) x y =
  match x,y with
  | "",v | v,"" -> v
  | _ ->  x^" "^y

let program_name () =
  Filename.basename (Sys.executable_name)

let help_msg ?header ?footer ~usage ~group ~name ~descr typ multi =
  let padlen =
    List.fold_left multi
      ~f:(fun acc c -> max acc (String.length (name c)))
      ~init:0
  in
  print_endline usage;
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

