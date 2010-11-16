open StdLabels
open MoreLabels
include Kernel

let int name = create ~name int_of_string
let string name = create ~name (fun s -> s)
let float name = create ~name float_of_string

let mapf ~f v = Kernel.cps_map ~f:(fun v -> Staged.map ~f v) v

let bool_flag ?short ?group ~descr long =
  mapf (flag ?short ?group ~descr long (const ()))
    ~f:(fun f v ->
          match v with
          | Some () -> f true
          | None -> f false)

let map' ~f v =
  Kernel.(
    let s =
      cps_map v
        ~f:(fun acc -> (Staged.create (f acc)))
    in
    map_cont ~f:Staged.run s)

let iter ~f = map' ~f:(fun acc x -> f x;acc)

let map ~f =
  map' ~f:(fun acc x -> Staged.apply acc (f x))

let map2 ~f =
  map' ~f:(fun acc x y -> Staged.apply acc (f x y))

let map3 ~f =
  map' ~f:(fun acc x y z -> Staged.apply acc (f x y z))

let map_gram ~f v =
  Kernel.set_gram v (f (Kernel.gram v))

let option v =
  map_gram
    begin
      map v ~f:(fun v -> Some v)
      <|> const None
    end
    ~f:Doc.opt

let non_empty_list v =
  map (list v)
    ~f:(function
          | [] -> failwith "The argument list must not be empty"
          | l -> l)

let maybe_quote_filename s =
  let must_quote =
    try
      String.iter s ~f:(function
                          | '-' | '_' | '0'..'9' | 'a'..'z' | 'A'..'Z' -> ()
                          | _ -> raise Exit);
      false
    with Exit -> true
  in
  if must_quote then
    Filename.quote s
  else
    s

let err_handler = function
  | Kernel.Extra_arguments l ->
      prerr_string "Too many arguments provided on the command line; \
                     don't know what to do with: ";
      prerr_endline
        (String.concat ~sep:" " (List.map ~f:maybe_quote_filename l));
      exit 1
  | Kernel.Not_enough_arguments ->
      prerr_endline "Not enough arguments provided on the command line";
      exit 1
  | e ->
      prerr_endline (Printexc.to_string e);
      exit 1

let run ?name ?args fmt f =
  let flags = Kernel.flags fmt in
  let synopsis = match (gram fmt) with
    | x when x = Doc.empty -> None
    | v -> Some (Doc.to_string v)
  in
  let annon,flagged = Parse_opt.run ~flags ?args ?name ?synopsis () in
  let v =
    try
      Kernel.parse ~flags:flagged annon fmt f
    with e ->  err_handler e
  in
  Staged.run v

(** we take the accumulator and make it an int... *)
let set_acc fmt f = mapf fmt ~f:(fun () -> f)

type 'a choice = 'a Parse_opt.cmd

let choice ?group ~descr ~name ~f fmt =
  { Parse_opt.name;
    descr;
    group;
    choice = fun ~name ~args -> run ~name ~args fmt f}

let multi_run ?name (choices:'a choice list) : 'a =
 Parse_opt.multi_run
   ?name
   choices
