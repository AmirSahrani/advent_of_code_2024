let read_lines filename =
  let f = open_in filename in
  let rec loop () =
    try
      let next = input_line f in
      next :: loop ()
    with End_of_file ->
      close_in f;
      []
  in
  loop ()

let check_right lst_str line pos =
  if pos < String.length (List.nth lst_str line) - 3 then
    String.sub (List.nth lst_str line) pos 4
  else ""

let check_left lst_str line pos =
  if pos >= 3 then
    Char.escaped (List.nth lst_str line).[pos]
    ^ Char.escaped (List.nth lst_str line).[pos - 1]
    ^ Char.escaped (List.nth lst_str line).[pos - 2]
    ^ Char.escaped (List.nth lst_str line).[pos - 3]
  else ""

let check_up lst_str line pos =
  if line >= 3 then
    Char.escaped (List.nth lst_str line).[pos]
    ^ Char.escaped (List.nth lst_str (line - 1)).[pos]
    ^ Char.escaped (List.nth lst_str (line - 2)).[pos]
    ^ Char.escaped (List.nth lst_str (line - 3)).[pos]
  else ""

let check_down lst_str line pos =
  if line < List.length lst_str - 3 then
    Char.escaped (List.nth lst_str line).[pos]
    ^ Char.escaped (List.nth lst_str (line + 1)).[pos]
    ^ Char.escaped (List.nth lst_str (line + 2)).[pos]
    ^ Char.escaped (List.nth lst_str (line + 3)).[pos]
  else ""

let check_diag_up_right lst_str line pos =
  if pos < String.length (List.nth lst_str line) - 3 && line - 3 >= 0 then (
    let out =
      Char.escaped (List.nth lst_str line).[pos]
      ^ Char.escaped (List.nth lst_str (line - 1)).[pos + 1]
      ^ Char.escaped (List.nth lst_str (line - 2)).[pos + 2]
      ^ Char.escaped (List.nth lst_str (line - 3)).[pos + 3]
    in
    if out = "XMAS" then print_string "U_R\n";
    out)
  else ""

let check_diag_up_left lst_str line pos =
  if pos >= 3 && line >= 3 then (
    let out =
      Char.escaped (List.nth lst_str line).[pos]
      ^ Char.escaped (List.nth lst_str (line - 1)).[pos - 1]
      ^ Char.escaped (List.nth lst_str (line - 2)).[pos - 2]
      ^ Char.escaped (List.nth lst_str (line - 3)).[pos - 3]
    in
    if out = "XMAS" then print_string "U_L\n";
    out)
  else ""

let check_diag_down_right lst_str line pos =
  if
    pos < String.length (List.nth lst_str line) - 3
    && line < List.length lst_str - 3
  then (
    let out =
      Char.escaped (List.nth lst_str line).[pos]
      ^ Char.escaped (List.nth lst_str (line + 1)).[pos + 1]
      ^ Char.escaped (List.nth lst_str (line + 2)).[pos + 2]
      ^ Char.escaped (List.nth lst_str (line + 3)).[pos + 3]
    in
    if out = "XMAS" then print_string "D_R\n";
    out)
  else ""

let check_diag_down_left lst_str line pos =
  if pos >= 3 && line < List.length lst_str - 3 then (
    let out =
      Char.escaped (List.nth lst_str line).[pos]
      ^ Char.escaped (List.nth lst_str (line + 1)).[pos - 1]
      ^ Char.escaped (List.nth lst_str (line + 2)).[pos - 2]
      ^ Char.escaped (List.nth lst_str (line + 3)).[pos - 3]
    in
    if out = "XMAS" then print_string "D_L\n";
    out)
  else ""

let find_num_xmas lst_str =
  let max_length = String.length (List.nth lst_str 0) in
  let max_lines = List.length lst_str in
  let rec count_occurances line_number line_pos count =
    try
      let new_count =
        if (List.nth lst_str line_number).[line_pos] = 'X' then
          count
          + (if check_left lst_str line_number line_pos = "XMAS" then 1 else 0)
          + (if check_right lst_str line_number line_pos = "XMAS" then 1 else 0)
          + (if check_up lst_str line_number line_pos = "XMAS" then 1 else 0)
          + (if check_down lst_str line_number line_pos = "XMAS" then 1 else 0)
          + (if check_diag_up_left lst_str line_number line_pos = "XMAS" then 1
             else 0)
          + (if check_diag_down_right lst_str line_number line_pos = "XMAS" then
               1
             else 0)
          + (if check_diag_down_left lst_str line_number line_pos = "XMAS" then
               1
             else 0)
          +
          if check_diag_up_right lst_str line_number line_pos = "XMAS" then 1
          else 0
        else count
      in
      if line_pos + 1 >= max_length then
        if line_number + 1 >= max_lines then new_count
        else count_occurances (line_number + 1) 0 new_count
      else count_occurances line_number (line_pos + 1) new_count
    with Failure _ -> count
  in
  count_occurances 0 0 0

let () =
  let data = read_lines "data/input" in
  let count = find_num_xmas data in
  print_int count
