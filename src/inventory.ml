type item = string
type quantity = int
type t = (item * quantity) list

(** AF: The list [\[(i1, q1); (i2, q2); ...; (in, qn)\]] represents the map data
    type where each item in the player's inventory, represented by a string, is
    mapped to an quantity, represented by an float. RI: The list contains no
    duplicate string, meaning that there is not [s1], [s2] such that [s1 = s2].*)

let empty = []

let is_empty inv =
  match inv with
  | [] -> true
  | _ -> false

let size inv = List.fold_left ( + ) 0 (List.map snd inv)

let rec insert inv item =
  match inv with
  | [] -> [ (item, 1) ]
  | (i, q) :: t -> if item = i then (i, q + 1) :: t else (i, q) :: insert t item

let rec remove inv item =
  match inv with
  | [] -> []
  | (i, q) :: t ->
      if item = i then if q > 1 then (i, q - 1) :: t else t
      else (i, q) :: remove t item

let rec member inv item =
  match inv with
  | [] -> false
  | (i, _) :: t -> if item = i then true else member t item

let rec get_quantity inv item =
  match inv with
  | [] -> 0
  | (i, v) :: t -> if item = i then v else get_quantity t item

let rec to_string_helper inv =
  match inv with
  | [] -> []
  | (i, q) :: t ->
      if i <> "soup" && i <> "water" then
        if q <= 1 then (string_of_int q ^ " " ^ i) :: to_string_helper t
        else (string_of_int q ^ " " ^ i ^ "s") :: to_string_helper t
      else to_string_helper t

let to_string (inv : t) : string = String.concat ", " (to_string_helper inv)
