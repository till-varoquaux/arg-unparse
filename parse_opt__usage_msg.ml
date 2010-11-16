open StdLabels
open MoreLabels

let (|>) x f = f x

(** [ pad_right s len ]  returns the string s padded to the length [len] with
    whitespaces added to the left of the string *)
let pad_right (s:string) (length:int) : string =
  if  String.length s >= length then
    s
  else
    let res = String.make length ' ' in
    String.blit ~src:s ~src_pos:0 ~dst:res ~dst_pos:0 ~len:(String.length s);
    res

(** Prints *)
type group = string
type name = string
type descr = string

let by_groups (l:(group option * name * descr) list) :
    (group option * (name * descr) list) list =
  let ht = Hashtbl.create 17 in
  List.iter l ~f:(fun (group,name,descr) ->
                    let v = name,descr in
                  try
                    let previous = Hashtbl.find ht group in
                    Hashtbl.replace ht ~key:group ~data:(v::previous)
                  with Not_found ->
                    Hashtbl.add ht ~key:group ~data:[v]);
  Hashtbl.fold ht
    ~f:(fun ~key ~data acc -> (key,data)::acc)
    ~init:[]

let kv_grouped_to_string (multi:(group option * name * descr) list) : string
    =
  let max_name_len =
    List.fold_left multi
      ~f:(fun acc (_,name,_) -> max acc (String.length name))
      ~init:0
  in
  by_groups multi
  |> List.sort ~cmp:compare
  |> List.map
      (* TODO: float this out *)
    ~f:(fun (group,elts) ->
          let elts =
            List.map (List.sort ~cmp:compare elts)
              ~f:(fun (name,descr) ->
                    let name = pad_right name max_name_len in
                    Printf.sprintf "%s  %s" name descr)
            |> String.concat ~sep:"\n"
          in
          match group with
          | None -> elts
          | Some v -> Printf.sprintf "%s:\n%s" v elts)
   |> String.concat ~sep:"\n"

let usage
    ?(header:string option)
    ?(footer:string option)
    ?(synopsis:string option)
    ~(name:string)
    (multi:(string * (group option * name * descr) list) list)
    : string =
  let buf = Buffer.create 16 in
  let pf fmt = Printf.bprintf buf fmt in
  pf "Usage: %s" name;
  begin match synopsis with
  | None           -> pf "\n"
  | Some usage_arg -> pf " %s\n" usage_arg
  end;
  begin match header with
  | None -> ()
  | Some s -> pf "\n%s" s
  end;
  List.iter multi
    ~f:(fun (typ,multi) ->
          pf "\n%s\n%s\n"
            typ
            (String.make (String.length typ) '=');
          pf "%s" (kv_grouped_to_string multi));
  begin match footer with
  | None -> ()
  | Some s -> pf "\n%s" s
  end;
  Buffer.contents buf
