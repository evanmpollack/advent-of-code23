type game = {
  id: int;
  max_red: int;
  max_green: int;
  max_blue: int
}

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

let list_max l = List.fold_left max 0 l

let substring_from s start = String.sub s start ((String.length s) - start);;

let substring_to s endd = String.sub s 0 endd

let extract_game_id l = int_of_string (substring_from (List.hd l) 5);;

let extract_by_color l color = List.filter (fun e -> (substring_from e ((String.index_from e 0 ' ') + 1)) = color) l;;

let extract_counts l = 
  let int_strings = List.map (fun e -> substring_to e (String.index_from e 0 ' ')) l in 
    List.map int_of_string int_strings

let game_of_line line =
  let colon_split = List.map (fun e -> String.split_on_char ':' e) (String.split_on_char ';' line) in
    let comma_split = List.flatten (List.flatten (List.map (fun e -> List.map (fun ee -> String.split_on_char ',' ee) e) colon_split)) in 
      let trimmed_game_and_colors = List.map String.trim comma_split in
        let game_id = extract_game_id trimmed_game_and_colors in
          let reds = extract_by_color trimmed_game_and_colors "red" in
            let greens = extract_by_color trimmed_game_and_colors "green" in
              let blues = extract_by_color trimmed_game_and_colors "blue" in
                {
                  id = game_id;
                  max_red = list_max (extract_counts reds);
                  max_green = list_max (extract_counts greens);
                  max_blue = list_max (extract_counts blues)
                }

let rec sum l = 
  match l with
  | [] -> 0
  | [v] -> v
  | h :: r -> h + sum r

let solve file = 
  let content_list = read_file file in 
    let games = List.map game_of_line content_list in 
      let powers = List.map (fun g -> g.max_red * g.max_green * g.max_blue) games in
        sum powers