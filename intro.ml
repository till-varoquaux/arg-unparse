(*begin
Command line parsing as a functional unparsing.

:language: {basebackend@docbook::caml}

Command line argument parsing is a puzzling subject: it seems simple and
small enough that it wouldn't gather much attention yet it pops up regularly
through the blogosphere. There are many libraries for argument parsing but
none of them seem to be unanimously loved. This has all the hallmarks of a
surprisingly sneaky (and thus interesting) problem.

A couple of long plane rides have allowed me to try taking a stab at argument
parsing. This is however not intended to be a fully maintained and polished
library; it is toy code used to test out a couple of ideas. The code uses a
couple of clever tricks (functional unparsing, staging of functions, phantom
types) but none of them are really new.  A seasoned ocaml programmers should be
able to tackle all the concepts involved without much of a headache.

So, without further ado:

Getting started
----------------

We can usually see writing a command line interfaces as making one or several
ocaml functions callable from the console (aka the entry points of our
program).  The library we've defined allows us to make those functions
callable by giving a specification of their arguments (using functional
unparsing).


Throughout this post we shall incrementally refine a given command line
interface. Each of those refinement will be exposed through the command line
as a separate functions

Our subject today is a basic `cp` function which takes its arguments in
reverse order (thus cp_into). It doesn't actually execute any action; instead
it prints to stdout. We'll get into why we are not using a function with the
arguments in the standard order for `cp` soon.
**)
let cp_into
    ?(recursive=false)
    ?(force=false)
    (dst:string)
    (src:string list) : unit =
  Printf.printf "cp_into\n\
    recursive:%b\n\
    force:%b\n\
    %s <- %s\n"
    recursive
    force
    dst
    (String.concat "," src)
(**
Now that we know what we'll be working on we can start looking at how to
define the command line interface. The main function we'll use is
`Unparse.choice` which takes two unlabelled arguments: A specification (of type
`Unparse.t`) which tells us which kind of command line arguments we expect and
how to parse them and the function that we are embedding.  `Unparse.choice`
doesn't actually parse the arguments straight away: it actually creates a value
that can be used to define command line interface with several sub commands
(à la busybox,git,hg etc..).

Embedding simple functions
--------------------------

The simple specifications are very straightforwardly driven by the type of the functions we
are embedding. We provide little more information than the type of the
function and names of the arguments (used for documentation purposes)
**)

let basic_fun : string -> string list -> unit = fun dst srcs -> cp_into dst srcs

(* In this post we will gloss over the first parameter of the type `Unparse.t`
   it is a phantom type used to enforce some constraints on the specification. *)
let basic_spec : (_,string -> string list -> 'a,'a) Unparse.t =
  Unparse.(string "tgt" ++ non_empty_list (string "src"))

let basic_choice : unit Unparse.choice =
  Unparse.choice basic_spec
    ~f:basic_fun
    ~descr:"copy (without any flags)"
    ~name:"cp_basic"
(**
At this point I owe you an explanation:
why did we not take the target as last argument like the unix `cp` command?

A note on the argument parsing heuristic
-----------------------------------------

The parsers we build are http://en.wikipedia.org/wiki/LL_parser[LL(1)] parser.
This means that there is no backtracking possible; if an operator
successfully consumes an argument any solution that implies that operator
failing will not even be considered.

The `list` operator is greedy and matches the longest possible list. If we
had try to embed the classical `cp` function.

    cp file... directory

with the specification:

   list (string "file") ++ string "directory"

this would have resulted in an unusable function (since the first the list
(`string ...`) would always have consumed all the remaining arguments).
**)
let cp (_src:string list) (_tgt:string) : unit =
  (* This function is never going to get successfully called... *)
  assert false

let non_ll1_spec : (_,string list -> string -> 'a,'a) Unparse.t =
  Unparse.(non_empty_list (string "src") ++ string "tgt")

let non_ll1_choice : unit Unparse.choice =
  Unparse.choice non_ll1_spec
    ~descr:"broken will always fail because of the way the spec was defined"
    ~f:cp
    ~name:"cp_non_ll1"

(**
Simple command line parsing with flags
--------------------------------------
Specifications for flags with no arguments match simple boolean values; iff
the flag is present on the command the specification evaluates to `true` when
parsing.
**)

let basic_flag_fun : bool -> bool -> string -> string list -> unit =
  fun recursive force dst src -> cp_into ~recursive ~force dst src

let basic_flag_spec :
    (_,bool -> bool -> string -> string list -> 'a,'a) Unparse.t
    = Unparse.(
     bool_flag "recursive" ~descr:"do a recursive copy"
     ++ bool_flag "force" ~descr:"overwrite target without warning"
     ++ string "tgt"
     ++ non_empty_list (string "src"))

let basic_flag_choice =
  Unparse.choice basic_flag_spec
    ~f:basic_flag_fun
    ~descr:"first attempt to have flags"
    ~name:"cp_basic_flags"

(**
More flags
------------

At this point incremental rewrites of the same code are getting a bit
tedious so I will introduce two concepts at once:

Non-boolean flags::
  will return of `Some` of a value when they are specified on the command line,
  This value can be produced by a specification that consumes an element from
  the command line.
Choice between flags::
  When put between two flags (`<!>`) this operator will
  return the value attached to the last flag specified on the command line. It
  can be chained used for more than two flags.
**)

let flag_fun : bool option
       -> bool option
       -> string
       -> string list
       -> unit =
  fun recursive force dst src -> cp_into ?recursive ?force dst src

let recursive_flag_spec : (_,bool option -> 'a,  'a) Unparse.t =
  Unparse.(
    flag "recursive" ~descr:"do a recursive copy" (const true)
    <!> flag "no-recursive" ~descr:"" (const false))

let force_flag_spec : (_,bool option -> 'a,  'a) Unparse.t =
  Unparse.(
   flag "force" ~descr:"overwrite target without warning" (const true)
   <!> flag "no-force" ~descr:"" (const false))

let flag_spec :
    (_,bool option
       -> bool option
       -> string
       -> string list
       -> 'a,  'a) Unparse.t =
  Unparse.
    (recursive_flag_spec
     ++ force_flag_spec
     ++ string "tgt"
     ++ non_empty_list (string "src"))

let flag_choice : unit Unparse.choice =
  Unparse.choice flag_spec
    ~f:flag_fun
    ~descr:"first attempt to have flags"
    ~name:"cp_into_with_flags"

(**
Labelling arguments of the called function
--------------------------------------------

What we did above in order to call the function is that we mapped the
function we were calling. This is still not a very satisfying solution
because we would like to specify those labels locally when we define the
spec.
This is made possible by the interface which allows us to map the
accumulator function locally.
**)
let final_spec :
    (_,?recursive:bool ->
      ?force:bool ->
      string ->
      string list
      -> 'a,  'a) Unparse.t
    =
  Unparse.(
    mapf ~f:(fun f v -> f ?recursive:v)
      (flag "recursive" ~descr:"do a recursive copy" (const true))
    ++ mapf ~f:(fun f v -> f ?force:v)
          (flag "force" ~descr:"overwrite target without warning" (const true))
    ++ string "tgt"
    ++ non_empty_list (string "src"))

let final_choice : unit Unparse.choice  =
  Unparse.choice final_spec
    ~descr:"Our last example function"
    ~f:cp_into
    ~name:"cp_final"

(**
Tying it all together
---------------------

All we need to make that command line interface is to actually tie all those
commands (i.e. `choice`s) we've defined together.
**)
let () =
  Unparse.multi_run [
    basic_choice;
    non_ll1_choice;
    basic_flag_choice;
    flag_choice;
    final_choice
  ]
(**
Conclusion
----------

This is only a quick introduction to how this library can be used. If you
want to understand what makes that library tick I invite you to read Olivier
Danvy excellent article on functional unparsing.

Hacks like this one might seem as a purely academical **synonym** exercise to
some but getting familiar with tricks like the ones used here makes them cheaper
to use and, in the long run, a sound investment. Argument parsing is a small
enough problem that allows you to add those techniques to your toolbox
pretty quickly.

end*)
