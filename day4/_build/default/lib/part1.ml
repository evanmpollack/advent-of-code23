module Int_Set = Set.Make(Int)

let title_offset = 10

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

let offset_start s = String.sub s title_offset ((String.length s) - 10)

let set_of_list l = List.fold_right Int_Set.add l Int_Set.empty

let set_tuple_of_first_two l =
  match l with
  | [x; y] | [x; y; _] -> Some (set_of_list x, set_of_list y)
  | _ -> None

let format_line line =
  let line_without_title = offset_start line in
    let split_by_category = String.split_on_char '|' line_without_title in
      let split_by_space = List.map (fun e -> List.filter_map int_of_string_opt (String.split_on_char ' ' e)) split_by_category in
        set_tuple_of_first_two split_by_space

let intersection tuple_opt = 
  let tuple = Option.get tuple_opt in
    Int_Set.inter (fst tuple) (snd tuple)

let rec sum l = 
  match l with
  | [] -> 0
  | h :: r -> h + sum r

let rec pow base exponent =
  match exponent with
  | 0 -> 1
  | 1 -> base
  | n -> let b = pow base (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else base)

let apply_weighting l = 
  let multiplier element = 
    match element with
    | 0 -> 0
    | 1 -> 1
    | _ -> pow 2 (element - 1)
  in
  List.map multiplier l

let solve file = 
  let content = read_file file in
    let formatted_content = List.map format_line content in
      let intersections = List.map intersection formatted_content in
        let intersection_sizes = List.map Int_Set.cardinal intersections in
          let weighted_scores = apply_weighting intersection_sizes in
            sum weighted_scores
