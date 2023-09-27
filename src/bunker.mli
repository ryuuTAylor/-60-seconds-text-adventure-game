(** Represention of dynamic bunker state.

    This module represents the state of a bunker as the game is played,
    including the items that are contained within the bunker and the condition
    of the character Ted, who resides within the bunker. *)

type t
(** The abstract type of values representing the bunker. *)

type condition
(** The type representing the condition of the player. *)

exception AlreadyDead

(** The type representing the outcome of an action. *)
type outcome =
  | Valid of t
  | Invalid

val init_bunker : Inventory.t -> t
(** [init_bunker inv] is the initial state of the bunker with inventory inv.
    Once the player is finished the preparation stage of the game, they will
    have put some items into the bunker, which is represented by inventory inv. *)

val add_item : string -> t -> t
(** [add_item itm b] adds item itm to the bunker b. Then, it returns an updated
    bunker b' that has the item added to its inventory. If an item that is
    already present within the bunker is added, then the quantity will simply
    increase by 1. *)

val use_item : string -> t -> t
(** [user_item itm b] uses item itm from the bunker b. This effectively removes
    the itm from the bunker b and then returns a new bunker b' without the item
    present. If the item is not present within the bunker, it simply returns b.*)

val get_status : t -> condition
(** [get_statu] returns an Inventory i that represents the items contained in
    bunker b. *)

val get_items : t -> Inventory.t
(** [get_items b] returns an Inventory i that represents the items contained in
    bunker b. *)

val item_present : string -> t -> bool
(** [item_present itm b] returns a boolean representing whether item itm is
    present within bunker b.*)

val get_health : string -> t -> int
(** [get_health c b] returns the health of character c in bunker b. *)

val get_health : t -> int
(** [get_health b] returns Ted's health in bunker b. *)

val get_hunger : t -> int
(** [get_hunger b] returns Ted's hunger in bunker b. *)

val get_sanity : t -> int
(** [get_sanity b] returns Ted's sanity in bunker b. *)

val get_thirst : t -> int
(** [get_thirst b] returns Ted's thirst in bunker b. *)

val get_dead : t -> bool
(** [get_survive b] returns whether or not Ted is dead. *)

val change_health : int -> t -> t
(** [change_health h b] changes Ted's health by h and returns a new bunker b'
    which has the health incremented or decremented by h. h can be a positive or
    negative integer, incrementing or decrementing health accordingly. *)

val change_hunger : int -> t -> t
(** [change_hunger h b] changes the Ted's hunger by h and returns a new bunker
    b' which has the health incremented or decremented by h. h can be a positive
    or negative integer, incrementing or decrementing the health accordingly. *)

val change_sanity : int -> t -> t
(** [change_sanity s b] changes Ted's sanity by s and returns a new bunker b'
    which has the health incremented or decremented by s. s can be a positive or
    negative integer, incrementing or decrementing sanity accordingly. *)

val change_thirst : int -> t -> t
(** [change_thirst t b] changes Ted's thirst by t and returns a new bunker b'
    which has the health incremented or decremented by t. t can be a positive or
    negative integer, incrementing or decrementing thirst accordingly. *)

val kill : t -> t
(** [death b] changes Ted's dead state to true, meaning that he has died and the
    game ends. If Ted was already dead, then failwith "Already Dead". *)

val to_string : t -> string
(** [to_string b] returns the string that expresses all the items in the bunker
    b. *)
