module Int_Set = Set.Make(Int)

type game = {
  winning: Int_Set.t;
  selected: Int_Set.t;
  num_matches: int
}

type 'a tree = 
| Leaf
| Node of 'a * 'a tree * 'a tree

let offset = 10

let read_lines channel = 
  let rec accumulator buf =
    try
      match input_line channel with
      | v -> accumulator (v :: buf)
      | exception End_of_file -> List.rev buf
    with exn ->
      close_in_noerr channel;
      raise exn
    in
    accumulator []

let read_file file = 
  let channel = open_in file in
    let close () = close_in channel in
      let read () = read_lines channel in
        Fun.protect ~finally:close read

let offset_start s = String.sub s offset ((String.length s) - 10)

let format_line line =
  let line_without_title = offset_start line in
    let split_by_category = String.split_on_char '|' line_without_title in
      let split_by_space_int = List.map (fun e -> List.filter_map int_of_string_opt (String.split_on_char ' ' e)) split_by_category in
        match split_by_space_int with
        | [x; y] | [x; y; _] -> 
          Some {
            winning = (Int_Set.of_list x); 
            selected = (Int_Set.of_list y); 
            num_matches = Int_Set.cardinal (Int_Set.inter (Int_Set.of_list x) (Int_Set.of_list y))
          }
        | _ -> None

let solve file = 
  let content = read_file file in
    let x = List.map format_line content in
      List.length x