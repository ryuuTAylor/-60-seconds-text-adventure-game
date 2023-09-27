(** Representation of event architecture. The events are stored in coordination.json.
    Note: Some of the event narratives in coordination.json were taken from the official 60 Seconds game.*)

type event
(** The abstract type that represents a single event which will occur. *)

type t
(** The abstract type of values representing events that occur during the days
    spend in the bunker. *)

type implication

exception UnknownEvent of string
(** Raised when an unknown event identifier is encountered. It carries the
    identifier of the unknown event. *)

exception UnknownChoice of string
(** Raised when an unknown choice is encountered. It carries the name of the
    unknown choice. *)

val from_json : Yojson.Basic.t -> t
(** [from_json j] is the events that [j] represents. Requires: [j] is a valid
    JSON coordination representation. *)

val get_events : t -> event list
(** [get_events t] exposes the list of events to the client. *)

val get_ids : t -> string list
(** [get_ids t] exposes the list of events's id to the client. *)

val get_choices : event -> string list
(** [get_choices t] exposes the list of event choices to the client. *)

val get_choices_helper : string -> t -> string list
(** [get_choices_helper event t] is a helper test function for get_choices. *)

val get_descriptions : event -> string list
(** [get_descriptions t] exposes the list of event descriptions to the client. *)

val get_descriptions_helper : string -> t -> string list
(** [get_descriptions_helper event t] is a helper test function for
    get_descriptions. *)

val get_event_name : event -> string
(** [get_event_name e] returns the id of event e. *)

val get_implications : event -> string -> implication list
(** [get_implications t name] exposes the list of choice implications to the
    client*)

val get_imp_text : implication -> string
(** [get_imp_text implications] exposes the implication text to the client*)

val get_imp_text_helper : string -> string -> t -> string list
(** [get_imp_text_helper event name t] is a helper test function for
    get_imp_text.*)

val get_imp_effect : implication -> string
(** [get_imp_effect implications] exposes the implication effect to the client*)

val get_imp_effect_helper : string -> string -> t -> string list
(** [get_imp_effect_helper event name t] is a helper test function for
    get_imp_effect.*)

val permute : t -> event list
(** [permute t] genenrates a randomly shuffled list of events for the game to
    happen randomly. *)

val permute_helper : t -> string list
(** [permute_helper t] is a helper test function for permute.*)
