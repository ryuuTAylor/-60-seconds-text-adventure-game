type condition = {
  health : int;
  hunger : int;
  sanity : int;
  thirst : int;
}

type t = {
  status : condition;
  inventory : Inventory.t;
  dead : bool;
}

exception UnknownCharacter of string
exception AlreadyDead

type outcome =
  | Valid of t
  | Invalid

let init_bunker inv =
  {
    status = { hunger = 0; thirst = 0; health = 100; sanity = 100 };
    inventory = inv;
    dead = false;
  }

let add_item item bunker =
  {
    status = bunker.status;
    inventory = Inventory.insert bunker.inventory item;
    dead = bunker.dead;
  }

let use_item item bunker =
  {
    status = bunker.status;
    inventory = Inventory.remove bunker.inventory item;
    dead = bunker.dead;
  }

let get_status bunker = bunker.status
let get_items bunker = bunker.inventory
let item_present item bunker = Inventory.member (get_items bunker) item
let get_health bunker = (get_status bunker).health
let get_hunger bunker = (get_status bunker).hunger
let get_sanity bunker = (get_status bunker).sanity
let get_thirst bunker = (get_status bunker).thirst
let get_dead bunker = bunker.dead

let change_health n bunker =
  let check_dead = get_health bunker + n <= 0 in
  let new_health = ref (get_health bunker + n) in
  if !new_health > 100 then new_health := 100;
  {
    status =
      {
        health = !new_health;
        hunger = get_hunger bunker;
        sanity = get_sanity bunker;
        thirst = get_thirst bunker;
      };
    inventory = get_items bunker;
    dead = get_dead bunker || check_dead;
  }

let change_hunger n bunker =
  let check_dead = get_hunger bunker + n >= 100 in
  let new_hunger = ref (get_hunger bunker + n) in
  if !new_hunger < 0 then new_hunger := 0;
  {
    status =
      {
        health = get_health bunker;
        hunger = !new_hunger;
        sanity = get_sanity bunker;
        thirst = get_thirst bunker;
      };
    inventory = get_items bunker;
    dead = get_dead bunker || check_dead;
  }

let change_thirst n bunker =
  let check_dead = get_thirst bunker + n >= 100 in
  let new_thirst = ref (get_thirst bunker + n) in
  if !new_thirst < 0 then new_thirst := 0;
  {
    status =
      {
        health = get_health bunker;
        hunger = get_hunger bunker;
        sanity = get_sanity bunker;
        thirst = !new_thirst;
      };
    inventory = get_items bunker;
    dead = get_dead bunker || check_dead;
  }

let change_sanity n bunker =
  let check_dead = get_sanity bunker + n <= 0 in
  let new_sanity = ref (get_sanity bunker + n) in
  if !new_sanity > 100 then new_sanity := 100;
  {
    status =
      {
        health = get_health bunker;
        hunger = get_hunger bunker;
        sanity = !new_sanity;
        thirst = get_thirst bunker;
      };
    inventory = get_items bunker;
    dead = get_dead bunker || check_dead;
  }

let kill bunker =
  if bunker.dead = true then raise AlreadyDead
  else { status = get_status bunker; inventory = get_items bunker; dead = true }

let to_string bunker : string =
  "Soup: "
  ^ string_of_int (Inventory.get_quantity bunker.inventory "soup")
  ^ ", Water: "
  ^ string_of_int (Inventory.get_quantity bunker.inventory "water")
  ^ "\nHunger: "
  ^ string_of_int (get_hunger bunker)
  ^ ", Thirst: "
  ^ string_of_int (get_thirst bunker)
  ^ ", Health: "
  ^ string_of_int (get_health bunker)
  ^ ", Sanity: "
  ^ string_of_int (get_sanity bunker)
  ^ "\nInventory: "
  ^ Inventory.to_string (get_items bunker)
