type 'a t = unit -> 'a

let apply v x = fun () -> v () x

let map ~f v = fun () -> f (v ())

let run v = v ()

let create v = fun () -> v
