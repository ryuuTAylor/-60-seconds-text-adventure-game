open Yojson.Basic.Util

exception UnknownEvent of string
exception UnknownChoice of string

type implication = {
  text : string;
  effect : string;
}

type choice = {
  name : string;
  implications : implication list;
}

type event = {
  id : string;
  descriptions : string list;
  choices : choice list;
}

type t = { events : event list }

let implications_of_json json =
  {
    text = json |> member "text" |> to_string;
    effect = json |> member "effect" |> to_string;
  }

let choices_of_json json =
  {
    name = json |> member "name" |> to_string;
    implications =
      json |> member "implications" |> to_list |> List.map implications_of_json;
  }

let descriptions_of_json json = json |> member "text" |> to_string

let events_of_json json =
  {
    id = json |> member "id" |> to_string;
    descriptions =
      json |> member "descriptions" |> to_list |> List.map descriptions_of_json;
    choices = json |> member "choices" |> to_list |> List.map choices_of_json;
  }

let coordination_of_json json =
  { events = json |> member "events" |> to_list |> List.map events_of_json }

let from_json json = coordination_of_json json
let get_events t = t.events
let get_event_name event = event.id
let get_ids t = List.map get_event_name (get_events t)
let get_choices event = event.choices |> List.map (fun choice -> choice.name)

let get_choices_helper event t =
  get_choices (List.hd (List.filter (fun x -> x.id = event) (get_events t)))

let get_descriptions event = event.descriptions

let get_descriptions_helper event t =
  get_descriptions
    (List.hd (List.filter (fun x -> x.id = event) (get_events t)))

let get_implications event name =
  (List.find (fun choice -> choice.name = name) event.choices).implications

let get_imp_text implication = implication.text

let get_imp_text_helper event name t =
  List.map get_imp_text
    (get_implications
       (List.hd (List.filter (fun x -> x.id = event) (get_events t)))
       name)

let get_imp_effect implication = implication.effect

let get_imp_effect_helper event name t =
  List.map get_imp_effect
    (get_implications
       (List.hd (List.filter (fun x -> x.id = event) (get_events t)))
       name)

let permute t : event list =
  Random.self_init ();
  let lst = get_events t in
  let arr = Array.of_list lst in
  let len = Array.length arr in
  for i = len - 1 downto 1 do
    let j = Random.int (i + 1) in
    let tmp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- tmp
  done;
  Array.to_list arr

let permute_helper t = List.map (fun x -> x.id) (permute t)
