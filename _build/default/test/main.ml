open OUnit2
open Game
open House
open Command
open State
open Inventory
open Events
open Bunker

(********************************************************************
   Test Plan
 ********************************************************************)

(** Test Plan Questions: 1. which parts of the system were automatically tested
    by OUnit vs. manually tested?

    Response: All the functionalities that are involved in the first stage of
    preparation and the second stage of survival were tested by OUnit in the
    main.ml in the test folder. At the same time, how the text-based interface
    handles human decisions of preparation and survival were manually tested
    through running make play, which largely take place in the main.ml in the
    bin folder. *)

(** Test Plan Questions: 2. what modules were tested by OUnit and how test cases
    were developed (black box, glass box, randomized, etc.).

    Modules Bunker, Command, Events, House, Inventory, and State are all tested
    by OUnit. Test cases were developed througth black box and white box. For
    the black box testing, we write test cases according to the specification of
    the methods (paths through the specification). For the white box testing, we
    write test cases that checks out every if & else branches and every pattern
    matching options. Also, we develop the project in TDD (Test driven
    development). Before the actual implementation of each functionalities, we
    first write out the expected results through some simple tests, which is
    black box testing. After each functionality is implemented, we added more
    tests to test other potentially more complex situations, which is glass box
    testing. We did not use randomized testing. *)

(** Test Plan Questions: 3. provide an argument for why the testing approach
    demonstrates the correctness of the system.

    Our testing approach successfully demontrates the correctness of the system.
    This is because for most of the time, we tested every possible branch of the
    if expression and pattern matching and edge case in our codes to make sure
    that the program executes normally under all possible situations. Besides,
    the overall bisect coverage for Modules Bunker, Command, Events, House,
    Inventory, and State reaches 99.7%, which means we almost cover every
    possible cases of our functionalities. Further, we ran make play many times
    and tried out all potential inputs and made sure that the console returned
    the right feedback that we are expecting. By making sure that our program
    knows exactly what we return when it is prompted differntly, we've
    demonstrated the correctness of the system. *)

(********************************************************************
   Here are some helper functions for your testing of set-like lists.
 ********************************************************************)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether they are
    equivalent set-like lists. That means checking two things. First, they must
    both be "set-like", meaning that they do not contain any duplicates. Second,
    they must contain the same elements, though not necessarily in the same
    order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

(* These tests demonstrate how to use [cmp_set_like_lists] and [pp_list] to get
   helpful output from OUnit. *)
let cmp_demo =
  [
    ( "order is irrelevant" >:: fun _ ->
      assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
        [ "foo"; "bar" ] [ "bar"; "foo" ] )
    (* Uncomment this test to see what happens when a test case fails.
       "duplicates not allowed" >:: (fun _ -> assert_equal
       ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string) ["foo"; "foo"]
       ["foo"]); *);
  ]

(********************************************************************
       End helper functions.
 ********************************************************************)

let events_prefix = "survival_data" ^ Filename.dir_sep
let event = Yojson.Basic.from_file (events_prefix ^ "coordination.json")
let event_t : Events.t = event |> Events.from_json

let get_events_test (name : string) (event : Events.t)
    (expected_output : string list) =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
    expected_output (event |> get_ids)

let get_events_tests =
  [
    get_events_test
      "event's ids are [Unknown Benefactor; Boredom; Fighting the Tide; Dead \
       air; Trace of Human Beings]"
      event_t
      [
        "Dead Air";
        "Winner Takes it All";
        "Unknown Benefactor";
        "Trace of Human Beings";
        "Boredom";
        "Fighting the Tide";
        "Let US OUTTT!!!";
        "Soup Kitchen";
        "Silent as the Grave";
        "Beast Fight";
      ];
  ]

let get_choices_test (name : string) (event : string) (state : Events.t)
    (expected_output : string list) =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
    expected_output
    (get_choices_helper event state)

let get_choices_tests =
  [
    get_choices_test "event Unknown Benefactor's choice are [yes; no]"
      "Unknown Benefactor" event_t [ "yes"; "no" ];
    get_choices_test
      "event Fighting the Tide's choice are [map; cards; radio; nothing]"
      "Fighting the Tide" event_t
      [ "map"; "cards"; "radio"; "nothing" ];
  ]

let get_descriptions_test (name : string) (event : string) (state : Events.t)
    (expected_output : string list) =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
    expected_output
    (get_descriptions_helper event state)

let get_descriptions_tests =
  [
    get_descriptions_test "event Boredom's descriptions are [very long]"
      "Boredom" event_t
      [
        "Let's do something today. Something fun. If we don't, someone might \
         snap and that's the last thing we want.";
        "Where is a ball when you need one? We are really bored and we need \
         something to occupy ourselves with. Too many hours of counting pipe \
         droplets is not healthy. There has to be an alternative.";
        "There is not much you can do in this tiny bunker. We need to fight \
         the boredom, otherwise someone might feel like going out to admire \
         the radioactive landscape, or something equally crazy like learning \
         about foreign geography. We need to do something!";
        "We're bored. No, really. WE'RE BORED. And we have to do something \
         about it, or we'll end up as some cannibalistic, post-apocalyptic \
         savages armed with toasters. It's time to do something.";
      ];
    get_descriptions_test
      "event Trace of Human Beings's descriptions are [very long]"
      "Trace of Human Beings" event_t
      [
        "Ted must have been hallucinating when he said that he think he have \
         heard the sound of humans talking above them. How could anybody be \
         nearby? No, at best it is Ted hallucinating, most likely it is some \
         mutants outside! Should we go out and check? ";
        "Today when Ted was peaking ouside, he saw something that you would \
         never believe, some brand new tire marks on the road! Does that mean \
         the resue team is here? Should we scout around and yell for help?";
        "It is the sound of helicopters! We knew it! It must be the rescue \
         team sent by the government. It couldn't be some flying monsters, \
         could it? Do we risk it and go outside?";
      ];
  ]

let get_imp_text_test (name : string) (event : string) (name : string)
    (state : Events.t) (expected_output : string list) =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
    expected_output
    (get_imp_text_helper event name state)

let get_imp_text_tests =
  [
    get_imp_text_test
      "event Boredom name radio implication text are [very long]" "Boredom"
      "radio" event_t
      [
        "If you put a sea shell to your ear, you can hear the ocean. We \
         discovered if you put your ear next to a radio, you can hear static! \
         And it's almost like an ocean! Who needs seashells any more!";
        "Despite all the interference, our radio was able to get a charming \
         groove on. We even sang a bit about not setting the world on fire. \
         This is clearly an oldie.";
      ];
  ]

let get_imp_effect_test (name : string) (event : string) (name : string)
    (state : Events.t) (expected_output : string list) =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
    expected_output
    (get_imp_effect_helper event name state)

let get_imp_effect_tests =
  [
    get_imp_effect_test
      "event Boredom name rifle implication effect are [+1_soup; -50_health]"
      "Boredom" "rifle" event_t
      [ "+1_soup"; "-50_health" ];
  ]

let permute_test (name : string) (state : Events.t)
    (expected_output : string list) =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
    expected_output (permute_helper state)

let permute_tests =
  [
    permute_test
      "event's permute are [Trace of Human Beings; Boredom; Dead air; Unknown \
       Benefactor; Fighting the Tide]"
      event_t
      [
        "Dead Air";
        "Winner Takes it All";
        "Unknown Benefactor";
        "Trace of Human Beings";
        "Boredom";
        "Fighting the Tide";
        "Let US OUTTT!!!";
        "Soup Kitchen";
        "Silent as the Grave";
        "Beast Fight";
      ];
  ]

let events_tests =
  List.flatten
    [
      get_events_tests;
      get_choices_tests;
      get_descriptions_tests;
      get_imp_text_tests;
      get_imp_effect_tests;
      permute_tests;
    ]

let data_dir_prefix = "preparation_data" ^ Filename.dir_sep
let house = Yojson.Basic.from_file (data_dir_prefix ^ "house.json")
let house_t : House.t = house |> House.from_json

let start_room_test (name : string) (room : House.t) (expected_output : string)
    : test =
  name >:: fun _ ->
  assert_equal ~printer:pp_string expected_output (room |> start_room)

let start_room_tests = [ start_room_test "house's start room" house_t "foyer" ]

let room_ids_test (name : string) (room : House.t)
    (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
    expected_output (room |> room_ids)

let room_ids_tests =
  [
    room_ids_test "house's room ids 1" house_t
      [ "foyer"; "bunker"; "dining room"; "kitchen"; "hallway" ];
    room_ids_test "house's room ids 2" house_t
      [ "bunker"; "foyer"; "hallway"; "dining room"; "kitchen" ];
  ]

let description_test (name : string) (room : House.t) (identifier : string)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal ~printer:pp_string expected_output (description room identifier)

let description_tests =
  [
    description_test "house's description of foyer" house_t "foyer"
      "You are in the foyer. Panic sweeps over you as the very literal \
       bombshell is about to drop on you. Sweat drips from your palms as you \
       scramble to make the most of your time.";
    description_test "house's description of bunker" house_t "bunker"
      "You are in the bunker. You have escaped to safety. For now. Perhaps \
       greater dangers lurk within.";
  ]

let exits_test (name : string) (room : House.t) (identifier : string)
    (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
    expected_output (exits room identifier)

let exits_tests =
  [
    exits_test "house's foyer's exit" house_t "foyer"
      [ "hallway"; "hall way"; "doorway"; "door way" ];
    exits_test "house's hallway's exit" house_t "hallway"
      [
        "dining room";
        "dining";
        "distance";
        "kitchen";
        "kitchen door";
        "door";
        "chicken pot pie";
        "bunker";
        "entrance";
        "foyer";
      ];
    exits_test "house's bunker's exit" house_t "bunker" [];
  ]

let items_test (name : string) (room : House.t) (identifier : string)
    (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
    expected_output (items room identifier)

let items_tests =
  [
    items_test "house's foyer's items" house_t "foyer"
      [ "water"; "axe"; "soup"; "map" ];
    items_test "house's hallway's items" house_t "hallway"
      [ "cards"; "gun"; "boy scout handbook"; "soup"; "water" ];
    items_test "house's kitchen's items" house_t "kitchen"
      [ "bug spray"; "radio"; "soup"; "water" ];
    items_test "house's dining room's items" house_t "dining room"
      [ "water"; "soup"; "checkers"; "first aid kit" ];
    items_test "house's bunker's items" house_t "bunker" [];
  ]

let next_room_test (name : string) (room : House.t) (identifier : string)
    (exit : string) (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal ~printer:pp_string expected_output
    (next_room room identifier exit)

let next_room_exception1 (name : string) (room : House.t) (identifier : string)
    (exit : string) : test =
  name >:: fun _ ->
  assert_raises (UnknownRoom identifier) (fun () ->
      next_room room identifier exit)

let next_room_exception2 (name : string) (room : House.t) (identifier : string)
    (exit : string) : test =
  name >:: fun _ ->
  assert_raises (UnknownExit exit) (fun () -> next_room room identifier exit)

let next_room_tests =
  [
    next_room_test "house's foyer's next rooms 1" house_t "foyer" "hallway"
      "hallway";
    next_room_test "house's hallway's next rooms 1" house_t "hallway"
      "dining room" "dining room";
    next_room_test "house's hallway's next rooms 2" house_t "hallway" "kitchen"
      "kitchen";
    next_room_test "house's kitchen's next rooms 1" house_t "kitchen" "hallway"
      "hallway";
    next_room_exception1
      "house's masion's next room would raise UnknownRoom exception" house_t
      "masion" "whatever";
    next_room_exception2
      "house's foyer's next rooms through bedroom would raise UnknownExit \
       exception"
      house_t "foyer" "bedroom";
    next_room_exception2
      "house's kitchen's next rooms through front would raise UnknownExit \
       exception"
      house_t "kitchen" "front";
  ]

let next_rooms_test (name : string) (room : House.t) (identifier : string)
    (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
    expected_output
    (next_rooms room identifier)

let next_rooms_exception (name : string) (room : House.t) (identifier : string)
    : test =
  name >:: fun _ ->
  assert_raises (UnknownRoom identifier) (fun () -> next_rooms room identifier)

let next_rooms_tests =
  [
    next_rooms_test "house's foyer's next rooms" house_t "foyer" [ "hallway" ];
    next_rooms_test "house's hallway's next rooms" house_t "hallway"
      [ "dining room"; "bunker"; "kitchen"; "foyer" ];
    next_rooms_test "house's kitchen's next rooms" house_t "kitchen"
      [ "hallway"; "dining room" ];
    next_rooms_test "house's dining's next rooms" house_t "dining room"
      [ "hallway"; "kitchen" ];
    next_rooms_test "house's bunker's next rooms" house_t "bunker" [];
    next_rooms_exception
      "house's bedroom's next rooms would raise UnknownRoom exception" house_t
      "bedroom";
  ]

let contains_item_test (name : string) (item : string) (room : House.t)
    (identifier : string) (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (contains_item item room identifier)

let contains_item_tests =
  [
    contains_item_test "house's foyer's items contain axe" "axe" house_t "foyer"
      true;
    contains_item_test "house's foyer's items don't contain knife" "knife"
      house_t "foyer" false;
    contains_item_test "house's hallway's items contain cards" "cards" house_t
      "hallway" true;
    contains_item_test "house's hallway's items don't contain flower" "flower"
      house_t "hallway" false;
    contains_item_test "house's kitchen's items contain radio" "radio" house_t
      "kitchen" true;
    contains_item_test "house's kitchen's items don't contain ufo" "ufo" house_t
      "kitchen" false;
    contains_item_test "house's dining room's items contain checkers" "checkers"
      house_t "dining room" true;
    contains_item_test "house's dining room's items don't contain wine" "wine"
      house_t "dining room" false;
    contains_item_test "house's bunker's exit don't contain anything" "anything"
      house_t "bunker" false;
  ]

let get_weight_test (name : string) (item : string) (room : House.t)
    (identifier : string) (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (get_weight item room identifier)

let get_weight_exception (name : string) (item : string) (room : House.t)
    (identifier : string) : test =
  name >:: fun _ ->
  assert_raises (UnknownItem item) (fun () -> get_weight item room identifier)

let get_weight_tests =
  [
    get_weight_test "house's foyer's item axe's weight is 4" "axe" house_t
      "foyer" 4;
    get_weight_test "house's foyer's item map's weight is 2" "map" house_t
      "foyer" 2;
    get_weight_test "house's kitchen's item bug spray's weight is 2" "bug spray"
      house_t "kitchen" 2;
    get_weight_test "house's kitchen's item radio's weight is 3" "radio" house_t
      "kitchen" 3;
    get_weight_test "house's dining room's item soup's weight is 1" "soup"
      house_t "dining room" 1;
    get_weight_test "house's dining room's item first aid kit's weight is 4"
      "first aid kit" house_t "dining room" 4;
    get_weight_exception
      "house's hallway's item bottled water's weight would raise UnknownItem \
       exception"
      "bottled water" house_t "hallway";
    get_weight_exception
      "house's kitchen's item sunset's weight would raise UnknownItem exception"
      "sunset" house_t "kitchen";
  ]

let weights_test (name : string) (room : House.t) (identifier : string)
    (expected_output : int list) : test =
  name >:: fun _ -> assert_equal expected_output (weights room identifier)

let weights_tests =
  [
    weights_test "house's foyer's item weights" house_t "foyer" [ 1; 4; 1; 2 ];
    weights_test "house's hallway's item weights" house_t "hallway"
      [ 1; 3; 3; 1; 1 ];
    weights_test "house's kitchen's item weights" house_t "kitchen"
      [ 2; 3; 1; 1 ];
    weights_test "house's dining room's item weights" house_t "dining room"
      [ 1; 1; 4; 2 ];
    weights_test "house's bunker's item weights" house_t "bunker" [];
  ]

let adventure_tests =
  List.flatten
    [
      start_room_tests;
      room_ids_tests;
      description_tests;
      exits_tests;
      next_room_tests;
      next_rooms_tests;
      items_tests;
      contains_item_tests;
      get_weight_tests;
      weights_tests;
    ]

let parse_test (name : string) (command : string) (expected_output : command) :
    test =
  name >:: fun _ -> assert_equal expected_output (Command.parse command)

let parse_test' (name : string) (command : string) (expected_output : exn) :
    test =
  name >:: fun _ ->
  assert_raises expected_output (fun () -> Command.parse command)

let parse_tests =
  [
    parse_test "Go parse test 1" "    go   clock   tower   "
      (Go [ "clock"; "tower" ]);
    parse_test "Go parse test 2" "go clock tower" (Go [ "clock"; "tower" ]);
    parse_test "Grab parse test 1" "grab apple" (Grab [ "apple" ]);
    parse_test "Grab parse test 2" "grab a lot  of apples"
      (Grab [ "a"; "lot"; "of"; "apples" ]);
    parse_test "Deposit parse test " "deposit" Deposit;
    parse_test "Help parse test " "help" Help;
    parse_test "Quit parse test " "quit" Quit;
    parse_test' "empty command raises empty" " " Command.Empty;
    parse_test' "misspelled go command raises malformed" "Go" Command.Malformed;
    parse_test' "wrong verb command raises malformed" "kill" Command.Malformed;
    parse_test' "go with empty object phrase command raises malformed" "go"
      Command.Malformed;
    parse_test' "grab with empty object phrase command raises malformed" "grab"
      Command.Malformed;
    parse_test' "deposit with non-empty object phrase command raises malformed"
      "deposit food" Command.Malformed;
    parse_test' "help with non-empty object phrase command raises malformed"
      "help coding" Command.Malformed;
    parse_test' "quit with non-empty object phrase command raises malformed"
      "quit coding" Command.Malformed;
  ]

let command_tests = List.flatten [ parse_tests ]

let go_test_illegal (name : string) (exit : string) (room : House.t)
    (state : State.t) (expected_result : result) : test =
  name >:: fun _ -> assert_equal expected_result (go exit room state)

let go_current_room_id_test (name : string) (exit : string) (room : House.t)
    (state : State.t) (expected_result : string) : test =
  name >:: fun _ ->
  match go exit room state with
  | Legal t ->
      assert_equal ~printer:pp_string expected_result (current_room_id t)
  | Illegal ->
      assert_equal ~printer:pp_string expected_result (current_room_id state)
  | Full | Taken ->
      failwith "wrong results achieved by the go_current_room_id_test"

let go_visited_test (name : string) (exit : string) (room : House.t)
    (state : State.t) (expected_result : string list) : test =
  name >:: fun _ ->
  match go exit room state with
  | Legal t ->
      assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
        expected_result (visited t)
  | Illegal ->
      assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
        expected_result (visited state)
  | Full | Taken -> failwith "wrong results achieved by the go_visited_test"

let grab_state_1 =
  match grab "water" house_t (init_state house_t) with
  | Legal t -> t
  | Illegal | Full | Taken -> failwith "Not possible grab_state_1"

let go_state_1 =
  match go "hallway" house_t grab_state_1 with
  | Legal t -> t
  | Illegal | Full | Taken -> failwith "Not possible go_state_1"

let grab_state_2 =
  match grab "soup" house_t go_state_1 with
  | Legal t -> t
  | Illegal | Full | Taken -> failwith "Not possible grab_state_2"

let go_state_2 =
  match go "dining room" house_t grab_state_2 with
  | Legal t -> t
  | Illegal | Full | Taken -> failwith "Not possible go_state_2"

let go_tests =
  [
    go_test_illegal "inital state of house exits from east is illegal" "east"
      house_t (init_state house_t) Illegal;
    go_test_illegal "inital state of house exits from east is illegal" "east"
      house_t (init_state house_t) Illegal;
    go_current_room_id_test "initial state of house exits from hallway is legal"
      "hallway" house_t (init_state house_t) "hallway";
    go_visited_test "initial state of ho exits from southwest is legal"
      "hallway" house_t (init_state house_t) [ "hallway"; "foyer" ];
    go_current_room_id_test "initial state of house exits from east is illegal"
      "east" house_t (init_state house_t) "foyer";
    go_visited_test "initial state of house exits from east is illegal" "east"
      house_t (init_state house_t) [ "foyer" ];
    go_visited_test
      "go to bunker from grab_state_2 should have visited [bunker; hallway; \
       foyer]"
      "bunker" house_t grab_state_2
      [ "bunker"; "hallway"; "foyer" ];
  ]

let grab_test_illegal (name : string) (item : string) (room : House.t)
    (state : State.t) (expected_result : result) : test =
  name >:: fun _ -> assert_equal expected_result (grab item room state)

let grab_weight_test (name : string) (item : string) (room : House.t)
    (state : State.t) (expected_result : int) : test =
  name >:: fun _ ->
  match grab item room state with
  | Legal t -> assert_equal expected_result (weight t)
  | Illegal | Full | Taken -> assert_equal expected_result (weight state)

let grab_held_test (name : string) (item : string) (room : House.t)
    (state : State.t) (expected_result : string list) : test =
  name >:: fun _ ->
  match grab item room state with
  | Legal t ->
      assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
        expected_result (held t)
  | Illegal | Full | Taken ->
      assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
        expected_result (held state)

let grab_tests =
  [
    grab_test_illegal "initial state of house grab the item flower is illegal"
      "flower" house_t (init_state house_t) Illegal;
    grab_weight_test "initial state of house grab the item water has weight 1"
      "water" house_t (init_state house_t) 1;
    grab_held_test "initial state of house grab the item axe makes held [axe]"
      "axe" house_t (init_state house_t) [ "axe" ];
    grab_test_illegal "grab state 1 grab the item water is Taken" "water"
      house_t grab_state_1 Taken;
    grab_weight_test "go_state_1 of house grab the item soup has weight 2"
      "soup" house_t go_state_1 2;
    grab_held_test
      "go_state_1 of house grab the item food makes held [soup; water]" "soup"
      house_t go_state_1 [ "soup"; "water" ];
    grab_test_illegal "go state 2 grab first aid kit is Full" "first aid kit"
      house_t go_state_2 Full;
  ]

let deposit_test_illegal (name : string) (room : House.t) (state : State.t)
    (expected_result : result) : test =
  name >:: fun _ -> assert_equal expected_result (deposit room state)

let deposit_weight_test (name : string) (room : House.t) (state : State.t)
    (expected_result : int) : test =
  name >:: fun _ ->
  match deposit room state with
  | Legal t -> assert_equal expected_result (weight t)
  | Illegal | Full | Taken -> assert_equal expected_result (weight state)

let deposit_held_test (name : string) (room : House.t) (state : State.t)
    (expected_result : string list) : test =
  name >:: fun _ ->
  match deposit room state with
  | Legal t ->
      assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
        expected_result (held t)
  | Illegal | Full | Taken ->
      assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
        expected_result (held state)

let deposit_taken_test (name : string) (room : House.t) (state : State.t)
    (expected_result : string list) : test =
  name >:: fun _ ->
  match deposit room state with
  | Legal t ->
      assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
        expected_result (taken t)
  | Illegal | Full | Taken ->
      assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
        expected_result (taken state)

let deposit_tests =
  [
    deposit_test_illegal "initial state deposit is illegal" house_t
      (init_state house_t) Illegal;
    deposit_weight_test "go state 1 deposit would lead to weight 0" house_t
      go_state_1 0;
    deposit_weight_test "go state 1 deposit twice would lead to weight 0"
      house_t
      (match deposit house_t go_state_1 with
      | Legal t -> t
      | Illegal | Full | Taken -> failwith "not achieveable")
      0;
    deposit_held_test "go state 1 deposit would lead to held []" house_t
      go_state_1 [];
    deposit_taken_test "go state 1 deposit would lead to taken [foyer_water]"
      house_t go_state_1 [ "foyer_water" ];
  ]

let prepared_test (name : string) (state : State.t) (expected_result : bool) :
    test =
  name >:: fun _ -> assert_equal expected_result (prepared state)

let prepared_tests =
  [
    prepared_test "initial state would be unprepared" (init_state house_t) false;
    prepared_test "go to bunker from grab_state_2 should be prepared"
      (match go "bunker" house_t grab_state_2 with
      | Legal t -> t
      | Illegal | Full | Taken -> failwith "Not possible go_state_2")
      true;
  ]

let inventory_test_isempty (name : string) (state : Inventory.t)
    (expected_result : bool) : test =
  name >:: fun _ -> assert_equal expected_result (is_empty state)

let inventory_test_size (name : string) (state : Inventory.t)
    (expected_result : int) : test =
  name >:: fun _ ->
  assert_equal expected_result (size state) ~printer:Int.to_string

let inventory_test_member (name : string) (state : Inventory.t) (item : string)
    (expected_result : bool) : test =
  name >:: fun _ -> assert_equal expected_result (member state item)

let inventory_test_quantity (name : string) (state : Inventory.t)
    (item : string) (expected_result : int) : test =
  name >:: fun _ -> assert_equal expected_result (get_quantity state item)

let inventory_test_to_string (name : string) (state : Inventory.t)
    (expected_result : string) : test =
  name >:: fun _ ->
  assert_equal expected_result
    (Inventory.to_string state)
    ~printer:String.escaped

let one_item_inventory = insert empty "abc"
let two_item_inventory = insert one_item_inventory "def"
let three_item_inventory = insert two_item_inventory "g"
let double_def_inventory = insert two_item_inventory "def"
let double_def_remove = remove double_def_inventory "def"
let remove_inventory = remove three_item_inventory "g"
let remove_empty = remove empty "abc"

let multiple_insert =
  insert (insert (insert one_item_inventory "abc") "abc") "abc"

let multiple_insert_1 = insert multiple_insert "water"
let multiple_insert_remove = remove multiple_insert_1 "water"

let inventory_tests =
  [
    inventory_test_isempty "empty inventory is empty" empty true;
    inventory_test_size "empty inventory has size 0" empty 0;
    inventory_test_member "empty inventory member abc is false" empty "abc"
      false;
    inventory_test_isempty "one_item_inventory is not empty" one_item_inventory
      false;
    inventory_test_size "two_item_inventory has size 2" two_item_inventory 2;
    inventory_test_member "three_item_inventory member def is true"
      three_item_inventory "def" true;
    inventory_test_member "remove_inventory member g is false" remove_inventory
      "g" false;
    inventory_test_size "remove_inventory has size 2" remove_inventory 2;
    inventory_test_size "remove_empty has size 0" remove_empty 0;
    inventory_test_size "double_def_inventory has size 3" double_def_inventory 3;
    inventory_test_size "double_def_remove has size 2" double_def_remove 2;
    inventory_test_member "double_def_remove member def is true"
      double_def_remove "def" true;
    inventory_test_size "multiple_insert should have size 4" multiple_insert 4;
    inventory_test_member "multiple_insert should member abc" multiple_insert
      "abc" true;
    inventory_test_size "multiple_insert_1 should have size 5" multiple_insert_1
      5;
    inventory_test_member "multiple_insert_1 should member water"
      multiple_insert_1 "water" true;
    inventory_test_size "multiple_insert_remove should have size 4"
      multiple_insert_remove 4;
    inventory_test_member "multiple_insert_remove should not member water"
      multiple_insert_remove "water" false;
    inventory_test_quantity "three_item_inventory should have one item abc"
      three_item_inventory "abc" 1;
    inventory_test_quantity "double_def_inventory should have two item def"
      double_def_inventory "def" 2;
    inventory_test_quantity "double_def_remove should have one item def"
      double_def_remove "def" 1;
    inventory_test_quantity "one_item_inventory should have zero item def"
      one_item_inventory "def" 0;
    inventory_test_quantity "multiple_insert should have four item abc"
      multiple_insert "abc" 4;
    inventory_test_to_string "one_item_inventory to string should be 1 abc"
      one_item_inventory "1 abc";
    inventory_test_to_string
      "double_def_inventory to string should be 1 abc, 2 defs"
      double_def_inventory "1 abc, 2 defs";
    inventory_test_to_string "multiple_insert_1 to string should be 4 abcs"
      multiple_insert_1 "4 abcs";
  ]

let state_tests =
  List.flatten [ go_tests; grab_tests; deposit_tests; prepared_tests ]

let bunker_1 = init_bunker multiple_insert_1

let get_health_test (name : string) (state : Bunker.t) (expected_result : int) :
    test =
  name >:: fun _ -> assert_equal expected_result (get_health state)

let get_hunger_test (name : string) (state : Bunker.t) (expected_result : int) :
    test =
  name >:: fun _ -> assert_equal expected_result (get_hunger state)

let get_sanity_test (name : string) (state : Bunker.t) (expected_result : int) :
    test =
  name >:: fun _ -> assert_equal expected_result (get_sanity state)

let get_thirst_test (name : string) (state : Bunker.t) (expected_result : int) :
    test =
  name >:: fun _ -> assert_equal expected_result (get_thirst state)

let get_dead_test (name : string) (state : Bunker.t) (expected_result : bool) :
    test =
  name >:: fun _ -> assert_equal expected_result (get_dead state)

let item_present_test (name : string) (item : string) (state : Bunker.t)
    (expected_result : bool) : test =
  name >:: fun _ -> assert_equal expected_result (item_present item state)

let to_string_test (name : string) (state : Bunker.t) (expected_result : string)
    : test =
  name >:: fun _ ->
  assert_equal expected_result (to_string state) ~printer:String.escaped

let kill_exception (name : string) (state : Bunker.t) : test =
  name >:: fun _ -> assert_raises AlreadyDead (fun () -> kill state)

let bunker_tests =
  [
    get_health_test "bunker_1 has health 100" bunker_1 100;
    get_hunger_test "bunker_1 has hunger 0" bunker_1 0;
    get_sanity_test "bunker_1 has sanity 100" bunker_1 100;
    get_thirst_test "bunker_1 has thrist 0" bunker_1 0;
    get_dead_test "bunker_1 is not dead" bunker_1 false;
    item_present_test "bunker_1 has item water" "water" bunker_1 true;
    item_present_test "bunker_1 doesn't have item def" "def" bunker_1 false;
    item_present_test "add soup to bunker_1 has item soup" "soup"
      (add_item "soup" bunker_1) true;
    item_present_test "use water from bunker_1 doesn't have item water" "water"
      (use_item "water" bunker_1)
      false;
    to_string_test "bunker_1 to string is ..." bunker_1
      "Soup: 0, Water: 1\n\
       Hunger: 0, Thirst: 0, Health: 100, Sanity: 100\n\
       Inventory: 4 abcs";
    get_health_test "bunker_1 change health -30 has health 70"
      (change_health (-30) bunker_1)
      70;
    get_health_test "bunker_1 change health 30 has health 100"
      (change_health 30 bunker_1)
      100;
    get_dead_test "bunker_1 change health -130 is dead"
      (change_health (-130) bunker_1)
      true;
    get_dead_test "bunker_1 change health -30 is not dead"
      (change_health (-30) bunker_1)
      false;
    get_hunger_test "bunker_1 change hunger 30 has hunger 30"
      (change_hunger 30 bunker_1)
      30;
    get_hunger_test "bunker_1 change hunger -30 has hunger 0"
      (change_hunger (-30) bunker_1)
      0;
    get_dead_test "bunker_1 change hunger 130 is dead"
      (change_hunger 130 bunker_1)
      true;
    get_thirst_test "bunker_1 change thirst 30 has thirst 30"
      (change_thirst 30 bunker_1)
      30;
    get_thirst_test "bunker_1 change thirst -30 has thirst 0"
      (change_thirst (-30) bunker_1)
      0;
    get_dead_test "bunker_1 change thirst 130 is dead"
      (change_thirst 130 bunker_1)
      true;
    get_sanity_test "bunker_1 change sanity -30 has sanity 70"
      (change_sanity (-30) bunker_1)
      70;
    get_sanity_test "bunker_1 change sanity 30 has sanity 100"
      (change_sanity 30 bunker_1)
      100;
    get_dead_test "bunker_1 change sanity -130 is dead"
      (change_sanity (-130) bunker_1)
      true;
    kill_exception
      "kill bunker_1 change sanity -130 would raise AlreadyDead exception"
      (change_sanity (-130) bunker_1);
    get_dead_test "kill bunker_1 is dead" (kill bunker_1) true;
    get_dead_test "bunker_1 change sanity -130 change health -30 is dead"
      (change_health (-30) (change_sanity (-130) bunker_1))
      true;
    get_dead_test "bunker_1 change sanity -130 change hunger 30 is dead"
      (change_hunger 30 (change_sanity (-130) bunker_1))
      true;
    get_dead_test "bunker_1 change sanity -130 change thirst 30 is dead"
      (change_thirst 30 (change_sanity (-130) bunker_1))
      true;
    get_dead_test "bunker_1 change sanity -130 change sanity -30 is dead"
      (change_sanity (-30) (change_sanity (-130) bunker_1))
      true;
  ]

let suite =
  "test suite for Group Project"
  >::: List.flatten
         [
           adventure_tests;
           command_tests;
           state_tests;
           inventory_tests;
           events_tests;
           bunker_tests;
         ]

let _ = run_test_tt_main suite
