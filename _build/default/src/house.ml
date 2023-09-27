open Yojson.Basic.Util

exception UnknownRoom of string
exception UnknownExit of string
exception UnknownItem of string

type exit = {
  name : string;
  room_id : string;
}

type item = {
  label : string;
  weight : int;
}

type room = {
  id : string;
  description : string;
  exit_names : string;
  exits : exit list;
  items : item list;
}

type t = {
  rooms : room list;
  start_room : string;
}

let items_of_json json =
  {
    label = json |> member "label" |> to_string;
    weight = json |> member "weight" |> to_int;
  }

let exits_of_json json =
  {
    name = json |> member "name" |> to_string;
    room_id = json |> member "room id" |> to_string;
  }

let rooms_of_json json =
  {
    id = json |> member "id" |> to_string;
    description = json |> member "description" |> to_string;
    exit_names = json |> member "exit_names" |> to_string;
    exits = json |> member "exits" |> to_list |> List.map exits_of_json;
    items = json |> member "items" |> to_list |> List.map items_of_json;
  }

let house_of_json json =
  {
    rooms = json |> member "rooms" |> to_list |> List.map rooms_of_json;
    start_room = json |> member "start room" |> to_string;
  }

let from_json json = house_of_json json

let get_room hse room =
  try List.find (fun r -> r.id = room) hse.rooms
  with Not_found -> raise (UnknownRoom room)

let start_room hse = hse.start_room
let room_ids hse = List.map (fun room -> room.id) hse.rooms
let description hse room = (get_room hse room).description
let exits hse room = List.map (fun exit -> exit.name) (get_room hse room).exits
let exit_names hse room = ((get_room hse room).exit_names [@coverage off])
let items hse room = List.map (fun item -> item.label) (get_room hse room).items

let weights hse room =
  List.map (fun item -> item.weight) (get_room hse room).items

let contains_item item hse room =
  List.exists (fun x -> x = item) (items hse room)

let get_item item hse room =
  if contains_item item hse room then
    List.find (fun i -> i.label = item) (get_room hse room).items
  else raise (UnknownItem item)

let get_weight item hse room = (get_item item hse room).weight

let next_room hse room ex =
  try (List.find (fun exit -> exit.name = ex) (get_room hse room).exits).room_id
  with Not_found -> raise (UnknownExit ex)

let next_rooms hse room =
  List.sort_uniq String.compare
    (List.map (fun exit -> exit.room_id) (get_room hse room).exits)
