let read_lines channel =
  let rec accumulator buf =
    match input_line channel with
    | v -> accumulator (v :: buf)
    | exception End_of_file -> List.rev buf
  in
  accumulator []

let read_file file = 
  let channel = open_in file in
    let close () = close_in channel in 
      let read () = read_lines channel in
        Fun.protect ~finally:close read

let solve file = 
  List.iter print_endline (read_file file)

