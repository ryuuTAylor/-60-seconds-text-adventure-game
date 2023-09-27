type t = {
  current : string;
  visited : string list;
  weight : int;
  held : string list;
  taken : string list;
  prepared : bool;
  bunker_inventory : Inventory.t;
}

let init_state hse =
  {
    current = House.start_room hse;
    visited = [ House.start_room hse ];
    weight = 0;
    held = [];
    taken = [];
    prepared = false;
    bunker_inventory = Inventory.empty;
  }

let bunker_inventory st = st.bunker_inventory
let max_weight = 4
let current_room_id st = st.current
let visited st = st.visited
let weight st = st.weight
let held st = st.held
let taken st = st.taken

type result =
  | Legal of t
  | Full
  | Illegal
  | Taken

let deposit_helper items_held inv =
  List.fold_left (fun acc x -> Inventory.insert acc x) inv items_held

let deposit hse st =
  match current_room_id st with
  | "hallway" ->
      Legal
        {
          current = st.current;
          visited = st.visited;
          weight = 0;
          held = [];
          taken = st.taken;
          prepared = st.prepared;
          bunker_inventory = deposit_helper (held st) (bunker_inventory st);
        }
  | _ -> Illegal

let go ex hse st =
  match House.next_room hse st.current ex with
  | next ->
      if next = "bunker" then
        Legal
          {
            current = next;
            visited = List.sort_uniq String.compare (st.visited @ [ next ]);
            weight = 0;
            held = [];
            taken = st.taken;
            prepared = true;
            bunker_inventory = deposit_helper (held st) (bunker_inventory st);
          }
      else
        Legal
          {
            current = next;
            visited = List.sort_uniq String.compare (st.visited @ [ next ]);
            weight = st.weight;
            held = st.held;
            taken = st.taken;
            prepared = st.prepared;
            bunker_inventory = st.bunker_inventory;
          }
  | (exception House.UnknownRoom _) | (exception House.UnknownExit _) -> Illegal

let grab (itm : string) (hse : House.t) (st : t) =
  match House.contains_item itm hse st.current with
  | true ->
      let item_weight = House.get_weight itm hse st.current in
      let item_id = st.current ^ "_" ^ itm in
      if List.mem item_id st.taken then Taken
      else if st.weight + item_weight <= max_weight then
        Legal
          {
            current = st.current;
            visited = st.visited;
            weight = st.weight + item_weight;
            held = itm :: st.held;
            taken = item_id :: st.taken;
            prepared = st.prepared;
            bunker_inventory = st.bunker_inventory;
          }
      else Full
  | false -> Illegal

let prepared st = st.prepared
