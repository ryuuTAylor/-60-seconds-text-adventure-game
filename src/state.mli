(** Representation of dynamic house state.

    This module represents the state of an house as it is being played,
    including the player's current room, the rooms that have been visited, and
    functions that cause the state to change.

    Most of this code is pulled from A2, with additional functionality including
    [weight], [held], [taken], [bunker_inventory], [prepared], [deposit], and
    [grab]. These functions allow you to see what items have been taken, the
    weights of said items, and what you are holding. Furthermore, you can
    deposit items into the bunker and see if the player is ready to move to the
    survival stage. *)

type t
(** The abstract type of values representing the game state. *)

val init_state : House.t -> t
(** [init_state h] is the initial state of the game when in the house h. In the
    inital state, state the player is currently located in the starting room,
    and they have visited only that room. They have not taken anything nor are
    holding anything and thus have no weight in their hands. They are not ready
    to move to the next stage and have nothing stored in the bunker. *)

val current_room_id : t -> string
(** [current_room_id st] is the identifier of the room in which the player is
    currently located in state [st]. *)

val visited : t -> string list
(** [visited st] is a set-like list of the room identifiers the player has
    visited in state [st]. The player has visited a room [rm] if their current
    room location is or has ever been [rm]. *)

val weight : t -> int
(** [weight st] return the total integer weight of the items that the player is
    currently holding in state [st]. [init_state h] sets weight to 0. *)

val held : t -> string list
(** [held st] is a set-like list of the items that the player is currently
    holding in state [st]. The player holds an item itm if [grab itm]
    successfully returned [Legal st'] at some point and have not run [deposit]. *)

val taken : t -> string list
(** [taken st] is a set-like list of the items that the player has taken. These
    items could be stored in the bunker or held by the player currently.
    Elements of [taken st] take the form of the room identifier followed by the
    name of the item, separated by an underscore. Example: If the player has
    picked up the axe and the boy scout handbook, then [taken st] is
    [foyer_axe; hallway_boy scout handbook]*)

val bunker_inventory : t -> Inventory.t
(** [bunker_inventory st] is an inventory containing the items that are
    currently stored by the bunker. [init_state h] sets the bunker_inventory to
    an empty Inventory*)

val prepared : t -> bool
(** [prepared st] returns a boolean that determines whether the preparation
    phase of the game has ended. This only occurs once the player has
    successfully entered the bunker. *)

type result =
  | Legal of t
  | Full
  | Illegal
  | Taken
      (** The type representing the result of an attempted movement.

          - [Legal of t] represents a legal movement.
          - [Full] represents the scenario when the player cannot pick up an
            item since doing so would exceed the maximum weight they can carry.
          - [Taken] represents the scenario where the player attempts to pick up
            an item that has already been taken.
          - [Illegal] represents an unparseable command such as [deposit 5]*)

val deposit : House.t -> t -> result
(** [deposit hse st] is the result of attempting to deposit items.

    - If the st.current_room_id = "hallway" (the player is in the hallway), then
      the result is [Legal st'] where st' transfers all the items that the
      player is holding from [held st] to [bunker_inventory st']

    - Otherwise, the result is [Illegal]

    Effects: none. No printing is done.*)

val go : string -> House.t -> t -> result
(** [go exit hse st] is the result of attempting to go through the exit named
    [exit] in state [st] and house [hse]:

    - If [exit] is the name of an exit from the player's current room, then the
      result is [Legal st'], where in [st'] the player is now located in the
      room to which [exit] leads.

    - Otherwise, the result is [Illegal].

    Effects: none. In particular, [go] does not print anything. *)

val grab : string -> House.t -> t -> result
(** [grab item hse st] is the result of attempting to grab the item named [item]
    in state [st] and adventure [hse]:

    - If [item] is the name of an item within the player's current room, then
      the result is [Legal st'], where in [st'] the player is now holding itm
      and their weight has increased by some positive integer.

    - If [item] has already been taken within the player's current room, then
      the result is [Taken] where [item] is present within the player's
      inventory or within the bunker.

    - If no space remains within the player's inventory, then [Full] is thrown.

    - Otherwise, the result is [Illegal]. This encompasses commands like [grab]
      with no item identifier or [grab i] where i is an item that does not exist
      within the current room.

    Effects: none. No printing is done.*)
