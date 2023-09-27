type object_phrase = string list

type command =
  | Go of object_phrase
  | Grab of object_phrase
  | Deposit
  | Help
  | Quit

exception Empty
exception Malformed

let parse str =
  match
    List.filter (fun s -> String.length s <> 0) (String.split_on_char ' ' str)
  with
  | [] -> raise Empty
  | "go" :: t -> if List.length t = 0 then raise Malformed else Go t
  | "grab" :: t -> if List.length t = 0 then raise Malformed else Grab t
  | "deposit" :: t -> if List.length t <> 0 then raise Malformed else Deposit
  | "help" :: t -> if List.length t <> 0 then raise Malformed else Help
  | "quit" :: t -> if List.length t <> 0 then raise Malformed else Quit
  | h :: t -> raise Malformed
