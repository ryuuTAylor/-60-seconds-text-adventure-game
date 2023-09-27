open Game
open House
open Command
open State
open Bunker
open Events

(** ------Helper Functions for Printing------ *)

(** [print_newline ()] prints a bunch of new lines, in order to reset the screen
    after each command. *)
let print_newline () =
  print_endline "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"

(** [print_help prints a help message. *)
let print_help () =
  print_endline "Commands:";
  print_endline
    "deposit - deposit the items you are holding into the bunker (you must be \
     in the hallway)";
  print_endline "go [room name] - go to a new room";
  print_endline "grab [item] - grab an item within the room";
  print_endline "quit - end the game"

(** [pp_held st] pretty prints [held st], what the player is holding on their
    person in preparation state st. It then prints a fraction of the total
    weight of the items you are carrying over 4 (the total weight you can
    carry). If [held st] is empty (the player isn't carrying anything), nothing
    is printed.

    Example: if [held st] is [soup; water], then [pp_held st] would print "Held:
    water, soup (2/4)" *)
let pp_held st =
  if (fun list -> if list <> [] then true else false) (held st) then
    print_endline
      ("Held: "
      ^ String.concat ", " (held st)
      ^ " ("
      ^ string_of_int (weight st)
      ^ "/4)\n")
  else ()

(* [quit_stage_1] is the message that will be printed when the player quits
   during the preparation stage. *)
let quit_stage_1 = "\nWell, nice playing with you, little coward~\n"

(* [quit_stage_2] is the message that will be printed when the player quits
   during the survival stage. *)
let quit_stage_2 =
  "\nSo sorry to see you leave. But at least you survived to see the bunker.\n"

(* [quit_death] is the message that will be printed when the player dies during
   the survival stage. *)
let quit_death =
  "\nUh oh, you are dead. Thanks for playing, and below's your final score.\n"

(* [quit_win] is the message that will be printed when the player survives
   during the survival stage. *)
let quit_win =
  "\n\
   Congratulations, you are rescued by the government on your 21st day of \
   survival. Thanks for playing, and below's your final score.\n"

(** -------Helper Functions for [pp_items]------- *)

(** [get_available st items] returns a list of items that are not present that
    aren't contained within [taken st]. In simple words, this filters out items
    that have already been taken.

    This method is a little complicated because of how item information is
    stored within [taken st] (a single element might look like hallway_boy scout
    handbook). *)
let rec get_available st items =
  match items with
  | [] -> []
  | h :: t -> (
      match String.split_on_char '(' h with
      | a :: b ->
          if List.mem (current_room_id st ^ "_" ^ String.trim a) (taken st) then
            get_available st t
          else h :: get_available st t
      | [] -> get_available st t)

(** [get_item_weights items weights] takes elements from both items and weights,
    set-like lists and superimposes them on top of each other. A list is
    returned where each element is an item and its associated weight.

    Requires: length(items) = length(weights) For example,
    [get_item_weights \[axe\] \[4\]] returns [axe (4)]*)
let rec get_item_weights items weights =
  match (items, weights) with
  | h1 :: t1, h2 :: t2 ->
      (h1 ^ " (" ^ string_of_int h2 ^ ")") :: get_item_weights t1 t2
  | _ -> []

(** [pp_items hse st] pretty prints [items st], the items present in the room
    that you are in. Once you have taken an item, then that item will no longer
    be printed. *)
let pp_items hse st =
  print_endline
    ("\nItems: "
    ^ String.concat ", "
        (get_available st
           (get_item_weights
              (items hse (current_room_id st))
              (weights hse (current_room_id st)))))

(** [foldi i f item acc] applies function f to an accumulator acc i times.
    function f should take in two parameters: item and acc. foldi is very
    specifically meant for repeated usage of add_item and remove_item. *)
let rec foldi i f item acc =
  if i <= 0 then acc else foldi (pred i) f item (f item acc)

(* print final score of the player when the game ends if survived to the second
   stage of the game. *)
let print_score_calc bunker day =
  let inv = get_items bunker in
  let multiplier = ref 5 in
  if get_dead bunker then (
    multiplier := 0;
    print_endline
      "\n\
       You died. And whatever items left in the bunker, stays in the bunker, \
       foever and permanently. Thus, they will not count towards your extra \
       credits. You will only be evaluated on how many days you survived.\n")
  else if Inventory.is_empty inv then
    print_endline
      "\n\
       You have survived the game empty handed. It seems that you have put all \
       your tools to good use.\n"
  else
    print_endline
      ("\nYou have survived the game with " ^ Inventory.to_string inv
     ^ ". Good for you, you win extra credits.\n");
  print_endline
    ("\nYour final score for this game is : "
    ^ string_of_int
        (min 100
           (int_of_float ((float_of_int day -. 1.) /. 20. *. 90.)
           + (!multiplier * Inventory.size inv)))
    ^ "\n\
       (Computed on a basis of your second stage's performance (weight = 90%) \
       with 5 additional bonus points for each unused items if you survive.)\n"
    )

(** The player will be rescued on the 21 st day if no special event happens. *)
let rescue_day = 21

(** [random_elem lst] returns a random element from lst. *)
let random_elem lst =
  Random.self_init ();
  List.nth lst (Random.int (List.length lst))

(** [random_elem lst] returns a random int with upper and lower bound. *)
let random_int min max =
  Random.self_init ();
  Random.int (max - min + 1) + min

(** [check_end_game bunker day] represents the ending of the survive stage and
    the entre game. state has type Bunker.t and is the player's state. Console
    will and the game and return back the player's performance. *)
let check_end_game bunker day =
  if day = rescue_day || get_dead bunker then
    if day < rescue_day then (
      if get_thirst bunker >= 100 then print_endline "You died of thirst.\n"
      else if get_hunger bunker >= 100 then
        print_endline "You died of hunger.\n"
      else if get_health bunker <= 0 then
        print_endline "You died of lack of health.\n"
      else if get_sanity bunker <= 0 then
        print_endline "You died of lack of sanity.\n"
      else print_endline "You died of sudden event.\n";
      print_endline quit_death;
      print_score_calc bunker day;
      exit 0)
    else (
      print_endline quit_win;
      print_score_calc bunker day;
      exit 0)
  else ()

(** [drink_soup b] gives the player the option to drink soup and taunts them if
    no soup remains. *)
let rec drink_soup (bunker : Bunker.t) =
  if Bunker.item_present "soup" bunker then (
    print_endline "Do you want to drink soup? (yes/no)";
    print_string "> ";
    match String.lowercase_ascii (read_line ()) with
    | "quit" ->
        print_endline quit_stage_2;
        exit 0
    | "yes" ->
        let change = random_int ~-45 ~-20 in
        print_endline
          ("\nYou drank a soup! "
          ^ string_of_int (~-1 * change)
          ^ " hunger was restored.");
        use_item "soup" (change_hunger change bunker)
    | "no" -> bunker
    | _ ->
        print_endline "\nInvalid option. Please try again.\n";
        drink_soup bunker)
  else bunker

(** [drink_water b] performs similarly to drink_soup.*)
let rec drink_water (bunker : Bunker.t) =
  if Bunker.item_present "water" bunker then (
    print_endline "Do you want to drink water? (yes/no)";
    print_string "> ";
    match String.lowercase_ascii (read_line ()) with
    | "quit" ->
        print_endline quit_stage_2;
        exit 0
    | "yes" ->
        let change = random_int ~-75 ~-45 in
        print_endline
          ("\nYou drank a water! "
          ^ string_of_int (~-1 * change)
          ^ " thirst was restored.");
        use_item "water" (change_thirst change bunker)
    | "no" -> bunker
    | _ ->
        print_endline "Invalid choice. Please try again.\n";
        drink_soup bunker)
  else bunker

(** [daily_tax b] takes in a bunker b and returns a bunker b' that has had the
    statuses of hunger, thirst, sanity changed due to the tax of daily
    activities. If the player's hunger and thirst are both less than 50,
    representing a state of semi-fullness, some health is restored. If not in a
    state of semi-fullness, then the health starts worsening. These numbers may
    need tuning. *)
let daily_tax bunker =
  let new_bunker =
    bunker
    |> change_thirst (random_int 13 19)
    |> change_hunger (random_int 6 12)
    |>
    if get_health bunker < 85 then change_sanity (random_int ~-4 ~-1)
    else change_sanity 0
  in
  if get_hunger new_bunker < 50 && get_thirst new_bunker < 50 then
    change_health (random_int 10 12) new_bunker
  else if get_hunger new_bunker >= 50 || get_thirst new_bunker >= 50 then
    change_health (random_int ~-11 ~-8) new_bunker
  else new_bunker

(** [possible_choices c b] takes in a list of choices that are allowed for an
    event and then looks at the bunker b to see what choices the player could
    actually make. Then, it returns these, filtering out choices for using items
    not present within the bunker inventory. *)
let rec possible_choices choices bunker =
  match choices with
  | "yes" :: t -> "yes" :: possible_choices t bunker
  | "no" :: t -> "no" :: possible_choices t bunker
  | "nothing" :: t -> "nothing" :: possible_choices t bunker
  | item :: t ->
      if item_present item bunker then item :: possible_choices t bunker
      else possible_choices t bunker
  | [] -> []

let rec print_list = function
  | [] -> ()
  | e :: l ->
      print_string e;
      print_string " "

(** [parse_effect c e b] updates bunker b with all of the effects from e. These
    effects could include updates to health, hunger, sanity, or thirst or could
    be the addition or removal of an item.

    Example of effects: [-20_sanity;-1_radio]*)
let rec parse_effect (choices : string list) (effects : string list)
    (bunker : Bunker.t) =
  match effects with
  | [] -> bunker
  | [ "none" ] ->
      print_endline "\nNothing happened.\n";
      bunker
  | effect :: t ->
      parse_effect choices t
        (match String.split_on_char '_' effect with
        | [ num; str ] -> (
            let amount = int_of_string num in
            match str with
            | "health" ->
                if amount > 0 then
                  print_endline
                    ("\nYou gained " ^ string_of_int amount ^ " health back.")
                else
                  print_endline
                    ("\nYou lost " ^ string_of_int (~-1 * amount) ^ " health.");
                change_health amount bunker
            | "hunger" ->
                if amount > 0 then
                  print_endline
                    ("\nYou gained " ^ string_of_int amount ^ " hunger back.")
                else
                  print_endline
                    ("\nYou lost " ^ string_of_int amount ^ " hunger.\n");
                print_endline
                  ("You lost " ^ string_of_int (~-1 * amount) ^ " hunger.");
                change_hunger amount bunker
            | "sanity" ->
                if amount > 0 then
                  print_endline
                    ("\nYou gained " ^ string_of_int amount ^ " sanity back.")
                else
                  print_endline
                    ("You lost " ^ string_of_int (~-1 * amount) ^ " sanity.");
                change_sanity amount bunker
            | "thirst" ->
                if amount > 0 then
                  print_endline
                    ("\nYou gained " ^ string_of_int amount ^ " thirst back.")
                else
                  print_endline
                    ("\nYou lost " ^ string_of_int (~-1 * amount) ^ " health.");
                change_thirst amount bunker
            | "rescued" ->
                print_endline
                  "\n\
                   You've reached a secret ending! You are rescued early and \
                   our game also comes to an end.\n\n\
                   Since this is a secret ending, you will not receive a final \
                   score for this game. Nice playing with you ~\n";
                exit 0
            | "killed" ->
                print_endline
                  "\n\
                   You got yourself killed. Sadly, it might be the end of your \
                   story.\n";
                kill bunker
            | item ->
                if Bunker.item_present item bunker then
                  if amount > 0 then (
                    print_endline
                      ("\nYou picked up " ^ string_of_int amount ^ " " ^ item
                     ^ ".");
                    foldi amount add_item item bunker)
                  else (
                    print_endline
                      ("\nYou lost "
                      ^ string_of_int (~-1 * amount)
                      ^ " " ^ item ^ ".");
                    foldi (~-1 * amount) use_item item bunker)
                else bunker)
        | _ ->
            print_endline "\nNothing happend.\n";
            bunker)

(** [event_instance e b d] represents a single event occurence on a single day.
    The head of the event list e is taken and used as the event for the day. *)
let rec event_instance (events : event list) (bunker : Bunker.t) (day : int) :
    Bunker.t =
  match events with
  | event :: t -> (
      print_endline ("Mystery Event: " ^ Events.get_event_name event);
      let descriptions = Events.get_descriptions event in
      let choices = Events.get_choices event in
      print_endline (random_elem descriptions);
      print_endline
        ("\nChoices: "
        ^ String.concat ", " (possible_choices choices bunker)
        ^ "\n");
      print_endline "Please make a choice or type quit to exit:";
      print_string "> ";
      match read_line () with
      | exception End_of_file -> bunker
      | "quit" ->
          print_endline quit_stage_2;
          exit 0
      | choice ->
          if
            List.mem
              (String.lowercase_ascii choice)
              (possible_choices choices bunker)
          then (
            let implications = Events.get_implications event choice in
            let impl = random_elem implications in
            print_endline ("\n" ^ get_imp_text impl);
            parse_effect choices
              (String.split_on_char '/' (get_imp_effect impl))
              bunker)
          else (
            print_newline ();
            print_endline "Invalid choice. Please try again.\n";
            event_instance events bunker day))
  | [] -> bunker

(** [survive e b d] represents a single day within the survival stage of the
    game where your bunker is b, the list of events is e and d is the current
    day you are on. *)
and survive (events : event list) (bunker : Bunker.t) (day : int) =
  let _ = check_end_game bunker day in
  print_newline ();
  print_endline ("Day " ^ string_of_int day ^ "\n");
  print_endline (Bunker.to_string bunker ^ "\n");
  let new_b = bunker |> drink_soup |> drink_water in
  print_endline "";
  if day mod 2 = 0 then (
    let soup_bunker = event_instance events new_b day in
    let _ = check_end_game soup_bunker day in
    print_endline "\nPress enter to go to the next day.";
    match read_line () with
    | _ -> survive (List.tl events) (daily_tax soup_bunker) (day + 1))
  else print_endline "\nPress enter to go to the next day.";
  match read_line () with
  | _ -> survive events (daily_tax new_b) (day + 1)

(** [start_survive inv] represents the beginning of the survive stage, where you
    have inventory inv. It prints out some flavor text for entering the bunker
    then your inventory as well as prompts the player to enter to the next part
    of the game. Then, survive is called with day = 1. *)
let start_survive (inv : Inventory.t) =
  print_newline ();
  print_endline
    "That was close! You barely manage to scramble inside before the ground \
     starts to rumble. The deafening explosion of a bomb erupts— the last \
     thing you hear before you black out. \n\
     -------------------\n\
     Your ears ring as the cold cement floor caresses you, a feeling you never \
     want to feel again. Your heart is surprisingly calm, considering what you \
     just underwent, but the danger has not passed yet. Your best hope is that \
     the government will come to rescue you, but who knows how far away that \
     is. You hope you can stay alive until then. ";
  if Inventory.is_empty inv then
    print_endline
      "\n\
       You entered the bunker with...nothing! Impressive. It's really not \
       looking good for you.\n"
  else
    print_endline
      ("\nYou have now entered the bunker with " ^ Inventory.to_string inv
     ^ ".\n");
  print_endline
    "\n\
     Here are some tips for you to survive the second stage of the game, so \
     read carefully.\n\n\
     You will start with 0 thirst, 0 hunger, 100 health, 100 sanity, and every \
     day, each of your conditions will change accordingly. If your thirst or \
     hunger rises above 100, or if your health and sanity drops below 0, then \
     you will die.\n\n\
     Every other day, you will encounter a mystery event, in which your \
     decision is crucial. Sometimes, it requires some specific tools from the \
     first stage of the game, and if you don't have them, well, you'll see \
     what happens.\n\n\
     Good luck, brave surviver, hope you survive to see the sunlight~";
  print_endline
    "\n\
     Are you ready to continue into the second phrase of the game? [Press \
     enter to play or type quit to exit]\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | "quit" ->
      print_endline quit_stage_2;
      exit 0
  | _ ->
      let events =
        Events.(
          Yojson.Basic.from_file "survival_data/coordination.json"
          |> from_json |> permute)
      in
      print_newline ();
      survive events (Bunker.init_bunker inv) 1

(** [prepare hse st start_time time_limit] represents a single action in the
    beginning preparation stage of the game. Every subsequent call comes from
    either an action completed or an unrecognizable command.

    hse is the house, st is the current state you are in, start_time represents
    the time that the player started the game, and time_limit represents how
    long the player has to play the preparation stage. *)
let rec prepare hse st start_time time_limit =
  if prepared st then start_survive (bunker_inventory st)
  else if time_limit -. (Unix.time () -. start_time) > 0.0 then (
    print_endline
      ("You have "
      ^ string_of_int
          (int_of_float (time_limit -. (Unix.time () -. start_time)))
      ^ " seconds left.\n");
    pp_held st;
    print_endline (description hse (current_room_id st));
    if current_room_id st != "bunker" then pp_items hse st;
    print_endline (exit_names hse (current_room_id st));
    print_endline
      "\n\
       Please make a decision. Commands: \"go [room]\", \"grab [object]\", \
       \"deposit\", \"help\", \"quit\".";
    print_string "> ";
    match parse (read_line ()) with
    | Go t -> (
        match go (String.concat " " t) hse st with
        | Legal state ->
            print_newline ();
            prepare hse state start_time time_limit
        | Illegal ->
            print_newline ();
            print_endline
              "The exit does not exist in the current room. Please try again.\n";
            prepare hse st start_time time_limit
        | Taken | Full ->
            failwith "Should not return taken or full when parsing go.")
    | Grab item -> (
        match grab (String.concat " " item) hse st with
        | Legal state ->
            print_newline ();
            print_endline ("You picked up a " ^ String.concat " " item ^ "!\n");
            prepare hse state start_time time_limit
        | Full ->
            print_newline ();
            print_endline
              "You don't have enough space in your inventory! Deposit your \
               items in the bunker first.\n";
            prepare hse st start_time time_limit
        | Illegal ->
            print_newline ();
            print_endline
              "The item does not exist in the current room. Please try again.\n";
            prepare hse st start_time time_limit
        | Taken ->
            print_newline ();
            print_endline
              "You've already picked this item up! Please try picking a \
               different item.\n";
            prepare hse st start_time time_limit)
    | Deposit -> (
        match deposit hse st with
        | Legal state ->
            print_newline ();
            print_endline
              "You have successfully deposited the items you're holding into \
               the bunker.\n";
            prepare hse state start_time time_limit
        | Illegal ->
            print_newline ();
            print_endline
              "You must be in the hallway to deposit items into the bunker.\n";
            prepare hse st start_time time_limit
        | Taken | Full ->
            failwith "Should not return taken or full when parsing deposit.")
    | Help ->
        print_newline ();
        print_help ();
        prepare hse st start_time time_limit
    | Quit ->
        print_endline quit_stage_1;
        exit 0
    | exception _ ->
        print_newline ();
        print_endline "Command can not be understood. Please try again.\n";
        prepare hse st start_time time_limit)
  else
    print_endline
      "BOOM! The nuclear bomb has exploded and unfortunately, you could not \
       escape. How. Unsuprising. I'm not sure how you're reading this \
       though... \n"

(** [play_game tl] starts a game that gives the player [tl] seconds to prepare. *)
let play_game time_limit =
  print_newline ();
  print_endline
    "\n\
     You are Ted. Today, as you were lazing away on the couch, a loud alarm \
     blared. A nuclear bomb is dropping just miles away from your house! You \
     must grab whatever you need to survive before everything is destroyed in \
     the explosion. Best of luck! You'll need it.\n";
  print_endline
    "Are you ready to play? [Press enter to play or type quit to exit]\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | "quit" ->
      print_endline quit_stage_1;
      exit 0
  | _ ->
      print_newline ();
      let house =
        House.from_json (Yojson.Basic.from_file "preparation_data/house.json")
      in
      prepare house (init_state house) (Unix.time ()) time_limit

(** [set_difficulty ()] prompts the player for a difficulty setting then runs
    the game based on it *)
let rec set_difficulty () =
  print_endline "Select your difficulty:";
  print_endline "\tLittle Boy: 120 seconds of preparation";
  print_endline "\tFat Man: 90 seconds of preparation";
  print_endline "\tTzar Bomba: 60 seconds of preparation\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | "quit" ->
      print_endline quit_stage_1;
      exit 0
  | (s : string) -> (
      match s |> String.lowercase_ascii |> String.trim with
      | "littleboy" | "little boy" -> play_game 120.0
      | "fat man" | "fatman" -> play_game 90.0
      | "tzar bomba" | "tsar bomba" -> play_game 60.0
      | "tzarbomba" | "tsarbomba" -> play_game 60.0
      | _ ->
          print_newline ();
          print_endline "Invalid difficulty setting. Please try again.\n\n";
          set_difficulty ())

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_newline ();
  print_endline
    "\n\n\
     Welcome to 60 Seconds — the thrilling text adventure game created by \
     group CS3110_OCCO!\n";
  print_endline
    "\n\
     You are Ted. Today, as you were lazing away on the couch, a loud alarm \
     blared. A nuclear bomb is dropping just miles away from your house! You \
     must grab whatever you need to survive before everything is destroyed in \
     the explosion. Best of luck! You'll need it.\n";
  print_endline
    "If it's your first time playing, please take your time to familiarize \
     yourself with the commands before beginning. You won't have time once the \
     game begins. \n";
  print_help ();
  print_endline
    "\n\
     House Map: \n\
    \               ┌------->kitchen\n\
    \               |          ↑\n\
    \  bunker <- hallway -> dining \n\
    \               ↑        room\n\
    \             foyer\n\
    \  ";
  print_endline
    "Tips for Success\n\
    \    * You can only carry so much, so be sure to deposit your items in the \
     bunker often. This can only be done from the hallway.\n\n\
    \    ** Going into the bunker via the command [go bunker] automatically \
     deposits all your items into it and forces the survival stage to start. \
     (you won't be able to collect any more items)";
  print_endline "";
  set_difficulty ()

(* Execute the game engine. *)
let () = main ()
