(** Representation of house data in the preparation stage of the game.

    This module represents the data stored in the house, including the rooms,
    items and their weights, and exits. It handles loading of that data from
    [preparation_data/house.json] as well as querying the data.

    Most of this code is pulled from A2, with added functionalities of the
    methods [items], [weights], [contains_item], and [get_weight].*)

type t
(** The abstract type of values representing rooms of the house. *)

exception UnknownRoom of string
(** Raised when an unknown room identifier is encountered. It carries the
    identifier of the unknown room. *)

exception UnknownExit of string
(** Raised when an unknown exit is encountered. It carries the name of the
    unknown exit. *)

exception UnknownItem of string
(** Raised when an unknown item is encountered. It carries the name of the
    unknown item*)

val from_json : Yojson.Basic.t -> t
(** [from_json j] is the rooms that [j] represents. Requires: [j] is a valid
    JSON house representation. *)

val start_room : t -> string
(** [start_room h] is the identifier of the starting room in house [h]. Example: *)

val room_ids : t -> string list
(** [room_ids h] is a set-like list of all of the room identifiers in house [h]. *)

val description : t -> string -> string
(** [description h r] is the description of the room with identifier [r] in
    house [h]. Raises [UnknownRoom r] if [r] is not a room identifier in [h]. *)

val exits : t -> string -> string list
(** [exits h r] is a set-like list of all exit names from the room with
    identifier [r] in house [h]. Raises [UnknownRoom r] if [r] is not a room
    identifier in [a]. *)

val exit_names : t -> string -> string
(** [exit_names h r] is a set-like list of all exit names from the room with
    identifier [r] in house [h]. This is meant for printing purposes. Raises
    [UnknownRoom r] if [r] is not a room identifier in [h]. *)

val items : t -> string -> string list
(** [items h r] is a set-like list of all item names from the room with
    identifier [r] in house [h]. Raises [UnknownRoom r] if [r] is not a room
    identifier in [h].*)

val weights : t -> string -> int list
(** [weights h r] is a set-like list of the weights of the items from the room
    with identifier [r] in house [h]. Raises [UnknownRoom r] if [r] is not a
    room identifier in [h].*)

val next_room : t -> string -> string -> string
(** [next_room h r e] is the identifier of the room in house [h] that is
    immediately reached by taking the exit named [e] from the room with
    identifier [r]. Raises [UnknownRoom r] if [r] is not a room identifier in
    house [h]. Raises [UnknownExit e] if [e] is not the name of an exit from the
    room with identifier [r] in [h]. *)

val next_rooms : t -> string -> string list
(** [next_rooms h r] is a set-like list of all the identifiers of rooms in house
    [a] that are immediately reachable by taking any exit from the room with
    identifier [r]. Raises [UnknownRoom r] if [r] is not a room identifier in
    [h]. *)

val contains_item : string -> t -> string -> bool
(** [contains_item i h r] returns true if the set-like list returned by
    [items h r] contains the item i, and returns false otherwise. Raises
    [UnknownRoom r] if [r] is not a room identifier in [a]. *)

val get_weight : string -> t -> string -> int
(** [get_weight i h r] returns the weight of the item if [contains_item i h r]
    returns true, if [contains_item i h r] returns false, this function would
    raises [UnknownItem i]. Raises [UnknownRoom r] if [r] is not a room
    identifier in [h].*)
