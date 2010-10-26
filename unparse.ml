open StdLabels
open MoreLabels
open Util
include Kernel
exception Help
exception In_flag of Kernel.option_value Parse_opt.t * exn

let int name = create ~name int_of_string
let string name = create ~name (fun s -> s)
let float name = create ~name float_of_string

let mapf ~f v =
  Kernel.cps_map ~f:(fun v -> Staged.map ~f v) v

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
        ~f:(fun acc ->
              (Staged.create (f acc)))
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

let help () =
  iter (bool_flag ~short:'h' ~descr:"show this help message" "help")
    ~f:(fun found -> if found then raise Help)

let () =
  Printexc.register_printer
    (function
       | In_flag (f,e) ->
           Some (Printf.sprintf "In flag [%s] : %s"
                   (Parse_opt.to_string f)
                   (Printexc.to_string e))
       | _ -> None)

let is_quote_char = function
  | '-' | '_' | '0'..'9' | 'a'..'z' | 'A'..'Z' -> false
  | _ -> true

let maybe_quote s =
  let must_quote =
    try
      String.iter s ~f:(fun c -> if is_quote_char c then raise Exit); false
    with Exit -> true
  in
  if must_quote then
    Filename.quote s
  else
    s

let err_handler name fmt exn =
  let gram = Kernel.gram fmt in
  let usage = "usage: " ^ name ^%^ String.uppercase (Doc.to_string gram) in
  match exn with
  | Help ->
      Parse_opt.help_msg ~usage (Kernel.flags fmt);
      exit 0
  | Kernel.Extra_arguments l ->
      prerr_endline usage;
      prerr_string "Too many arguments provided on the command line; \
                     don't know what to do with: ";
      prerr_endline (String.concat ~sep:" " (List.map ~f:maybe_quote l));
      exit 1
  | Kernel.Not_enough_arguments ->
      prerr_endline usage;
      prerr_endline "Not enough arguments provided on the command line";
      exit 1
  | e ->
      prerr_endline (Printexc.to_string e);
      exit 1

let argv () = List.tl (Array.to_list Sys.argv)

let run ?(name=program_name ()) ?(args=argv ()) fmt =
  let fmt = help () ++ fmt in
  let flags = Kernel.flags fmt in
  let annon,flagged = Parse_opt.get_all flags args in
  Kernel.parse ~fail:(err_handler name fmt) ~flags:flagged annon fmt

(** we take the accumulator and make it an int... *)
let set_acc fmt f = mapf fmt ~f:(fun () -> f)

type 'a choice = (string -> 'a) Parse_opt.Multi.t

let choice ?group ~descr ~name ~f fmt =
  { Parse_opt.Multi.name;
    descr;
    group;
    choice = fun args pgm_name ->
      run
        ~name:(pgm_name ^ " " ^ name)
        ~args
        fmt
        f }

let multi_run ?(name=Util.program_name ()) choices =
  let v = Parse_opt.Multi.run ~name choices in
  v name
