(*
  Posix specification (does not take long option):

  http://www.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap12.html#tag_12_02

  GNU specification (adds long options to posix):
  http://www.gnu.org/s/libc/manual/html_node/Argument-Syntax.html#Argument-Syntax

*)
(** Command line argument handling

    This module implements low-level command line argument handling for programs

    TODO: check for syntax of arguments.
    TODO: s/option/flag/
*)

(* - Differences: the gnu standard allows long options to have optional
   arguments (!= gnu).
   - No reordering (!= gnu).
   - Short option with arguments can be aglomerated (!= posix)
*)
(**
   {1 Syntax of the command line (parsing command line arguments). }

   There are two kind of command arguments options (e.g. [-a] or [--help] aka flags) and
   operands (a.k.a. anonymous arguments).
   - Short options are one alphanumerical character preceded by a a hyphen (eg [-a],[-7]...).
   - Long options are [--] followed by a name made of alphanumeric characters
   and dashes (e.g. [--long-option]).
   - Options can take an argument (referred to as option-argument). This option-argument
   cannot be optional.
   - Long options can take their argument either after an equal sign
   ([--name=value]) or as the next token on the command line ([--name value]).
   - Short option's argument may appear as a separate token ([-a value]) or come
   straight after them ([-avalue]) in the same token.
   - Multiple short options can be agglomerated in one token after the hyphen
   character ( [-a -b -c] is equivalent to [-abc]). Only the last one may accept
   an argument.

   @see "yada yada"
   {{:http://www.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap12.html} POSIX Utility argument syntax}

*)

(* CLI argument parsing:

    When parsing command line arguments we get a stream of flags (aka options)
    and anonymous arguments.

    A description of type ['a t] is used to translesate a given flags and (if it
    has one) its argument in a value of type ['a].
*)
(**
   A flag; when parsing this flag (and its argument if it has one) will be
   injected in a value of type ['a]*)
type 'a flag

type 'a arg_descr =
    [ `No_arg of 'a
    | `Arg    of string * (string -> 'a) ]

(** Create a option. *)
val flag :
  ?group:string
  -> ?short:char
  -> arg:'a arg_descr
  -> descr:string
  -> string
  -> 'a flag

(**
   [run ~flags ~exit_on_error]
   Parses a list of command line arguments.
   If [exit_on_error] is true then the program will print an error message
   and then exit if an error occurs. It will also print a help message and exit
   when seeing the [--help] message.
   If [exit_on_error] is [false] then, [Help] and [Bad] will be raised upon those cases
   They will contain the message that would have been printer.


   @param args arguments parsed by this call. Defaults to the arguments passed at the
   command line.
   {b option used for documentation}
   @param name name of the program (defaults to the filename of the program)
   @param synopsis used for the help message
   @param header header of the help message comes before the description of the
                  flags in the help message
   @param footer footer of the help message
*)
val run :
   ?synopsis:string
  -> ?header:string
  -> ?footer:string
  -> ?name:string
  -> ?flags:'a flag list
  -> ?args:string list
  -> ?exit_on_error:bool
  -> unit
  -> string list * 'a list


exception Help of string
exception Bad of string

(** {2 Low level interface } *)
(** We argument parsing is done via a cursor *)
type 'a cursor

(* An option. *)

(** The result of advancing the cursor. *)
type 'a res =
  | Eof
  | Annon of string
  | Flag of 'a

val init :
   ?synopsis:string
  -> ?header:string
  -> ?footer:string
  -> ?name:string
  -> ?args:string list
  -> 'a flag list
  -> 'a cursor

val get : 'a cursor -> 'a res

(** {2 multiple dispatch } *)

type 'a cmd = {
  name   : string;
  group  : string option;
  choice : name:string -> args:string list -> 'a;
  descr  : string
}

val multi_run :
  ?name:string
  -> ?args:string list
  -> ?exit_on_error:bool
  -> 'a cmd list
  -> 'a
