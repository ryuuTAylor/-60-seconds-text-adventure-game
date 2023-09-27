(** Representation of dynamic bunker inventory.

    This module represents the items contained within the bunker. It contains
    the ability to check the contents of the bunker inventory and to add and
    remove items from it.*)

type item = string
(** [item] represents an item in the player's inventory and is a string in our
    map. *)

type quantity = int
(** [quantity] represents the quantity of an item in the player's inventory and
    is a int in our map. *)

type t
(** [t] is the type of inventory. *)

val empty : t
(** [empty] is the empty inventory *)

val is_empty : t -> bool
(** [is_empty inv] is [true] iff [inv] is empty. *)

val size : t -> int
(** [size inv] is the number of item-quantity bindings in [inv]. [size empty] is
    [0]. *)

val insert : t -> item -> t
(** [insert inv item] is [inv] with [item] bound to [1]. If [item] was already
    bound, its previous quantity is incremented by 1. *)

val remove : t -> item -> t
(** [remove inv item] contains all the bindings of [inv] except a binding for
    [item] if [quantity = 1], otherwise quantity is decremented by 1. If [item]
    is not bound in [inv], then [remove] returns a inventory with the same
    bindings as [inv]. *)

val member : t -> item -> bool
(** [member inv item] is [true] iff [item] is bound in [inventory]. *)

val get_quantity : t -> item -> quantity
(** [get_quantity inv item] is [quantity] for [item] that is in [inventorty].
     If the [item] is not in [inventory], then returns 0.*)

val to_string : t -> string
(** [to_string inv] returns a string format of type t. *)
