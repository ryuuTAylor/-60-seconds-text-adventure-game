(** Parsing of player commands.

    Most of this is pulled from A2, with some extra added functionality, namely
    Grab, Deposit, and Help.*)

type object_phrase = string list
(** The abstract type representing an object phrase that a player may go or
    grab. *)

type command =
  | Go of object_phrase
  | Grab of object_phrase
  | Deposit
  | Help
  | Quit
      (** The abstract type representing a potential command that could be typed *)

exception Empty
(** Raised when an empty command is parsed. *)

exception Malformed
(** Raised when a malformed command is parsed. *)

val parse : string -> command
(** [parse s] parses a string s and returns a command based on what it is.

    - Raises [Empty] if string s is empty.
    - Raises [Malformed] if string s does not match one of the accepted command
      formats. *)
