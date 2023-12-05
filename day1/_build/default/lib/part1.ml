open Str
open String

let re = regexp "[0-9]"

let read_file file = 
  In_channel.with_open_text file In_channel.input_all

let string_of_char c = make 1 c

let find_first s = 
  match search_forward re s 0 with
  | v -> Some v
  | exception _ -> None

let find_last s last_idx =
    match search_backward re s last_idx with
    | v -> Some v
    | exception _ -> None

let extract_digits s =
    let last_index = length s - 1 in
      let first = find_first s in
        let last = find_last s last_index in
          match (first, last) with
          | (Some first, Some last) -> (string_of_char s.[first]) ^ (string_of_char s.[last])
          | _ -> "0"

let rec sum list =
  match list with
  | [] -> 0
  | [v] -> v
  | h :: r -> h + sum r

let solve file = 
  let string_list = split_on_char '\n' (read_file file) in
    let int_string_list = List.map extract_digits string_list in
      let int_list = List.map int_of_string int_string_list in
        (sum int_list)
