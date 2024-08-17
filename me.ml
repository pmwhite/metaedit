module String_map = Map.Make (String)

exception Bug of Printexc.raw_backtrace * string

let bugf fmt =
  Printf.ksprintf
    (fun s ->
      raise_notrace (Bug (Printexc.get_raw_backtrace (), Printf.sprintf "BUG: %s" s)))
    fmt
;;

let insert_char_at ~char ~into ~at =
  let len = String.length into in
  if at < 0 || at > len
  then bugf "insert_char_at called with out-of-range index. at = %d, len = %d" at len;
  let before = String.sub into 0 at in
  let after = String.sub into at (len - at) in
  Printf.sprintf "%s%c%s" before char after
;;

let delete_char_at ~from_ ~at =
  let len = String.length from_ in
  if at < 0 || at >= len
  then bugf "delete_char_at called with out-of-range index. at = %d, len = %d" at len;
  let before = String.sub from_ 0 at in
  let after = String.sub from_ (at + 1) (len - at - 1) in
  Printf.sprintf "%s%s" before after
;;

let split_string_at s ~at =
  let len = String.length s in
  if at < 0 || at > len
  then bugf "split_string_at called with out-of-range index. at = %d, len = %d" at len;
  let before = String.sub s 0 at in
  let after = String.sub s at (len - at) in
  before, after
;;

let sub_array_before array index =
  let len = Array.length array in
  if index < 0 || index > len
  then
    bugf "sub_array_before called with out-of-range index. index = %d, len = %d" index len;
  Array.sub array 0 index
;;

let sub_array_to_end array index =
  let len = Array.length array in
  if index < 0 || index > len
  then
    bugf "sub_array_to_end called with out-of-range index. index = %d, len = %d" index len;
  Array.sub array index (len - index)
;;

external terminal_size : unit -> int * int = "metaedit_terminal_size"

type state =
  { mutable terminal_width : int
  ; mutable terminal_height : int
  ; mutable script_lines : string array
  ; mutable script_cursor_line : int
  ; mutable script_cursor_preferred_column : int
  }

let get_script_cursor_column state =
  Int.min
    state.script_cursor_preferred_column
    (String.length state.script_lines.(state.script_cursor_line))
;;

let insert_char char state =
  let into = state.script_lines.(state.script_cursor_line) in
  let column = get_script_cursor_column state in
  state.script_lines.(state.script_cursor_line) <- insert_char_at ~char ~into ~at:column;
  state.script_cursor_preferred_column <- column + 1
;;

let delete_char state =
  let from_ = state.script_lines.(state.script_cursor_line) in
  let column = get_script_cursor_column state in
  if column > 0
  then (
    state.script_lines.(state.script_cursor_line) <- delete_char_at ~from_ ~at:(column - 1);
    state.script_cursor_preferred_column <- column - 1)
  else if column = 0
  then
    if state.script_cursor_line > 0
    then (
      let above = sub_array_before state.script_lines (state.script_cursor_line - 1) in
      let below = sub_array_to_end state.script_lines (state.script_cursor_line + 1) in
      let line_before = state.script_lines.(state.script_cursor_line - 1) in
      let line = state.script_lines.(state.script_cursor_line) in
      state.script_lines <- Array.concat [ above; [| line_before ^ line |]; below ];
      state.script_cursor_line <- state.script_cursor_line - 1;
      state.script_cursor_preferred_column <- String.length line_before)
;;

let newline state =
  let line = state.script_lines.(state.script_cursor_line) in
  let column = get_script_cursor_column state in
  let before, after = split_string_at line ~at:column in
  let above = sub_array_before state.script_lines state.script_cursor_line in
  let below = sub_array_to_end state.script_lines (state.script_cursor_line + 1) in
  state.script_lines <- Array.concat [ above; [| before; after |]; below ];
  state.script_cursor_line <- state.script_cursor_line + 1;
  state.script_cursor_preferred_column <- 0
;;

let cursor_up state =
  if state.script_cursor_line > 0
  then state.script_cursor_line <- state.script_cursor_line - 1
;;

let cursor_down state =
  if state.script_cursor_line < Array.length state.script_lines - 1
  then state.script_cursor_line <- state.script_cursor_line + 1
;;

let cursor_right state =
  let line = state.script_lines.(state.script_cursor_line) in
  let column = get_script_cursor_column state in
  if column < String.length line
  then state.script_cursor_preferred_column <- column + 1
  else if state.script_cursor_line < Array.length state.script_lines - 1
  then (
    state.script_cursor_line <- state.script_cursor_line + 1;
    state.script_cursor_preferred_column <- 0)
;;

let cursor_left state =
  let column = get_script_cursor_column state in
  if column > 0
  then state.script_cursor_preferred_column <- column - 1
  else if state.script_cursor_line > 0
  then (
    let line = state.script_lines.(state.script_cursor_line) in
    state.script_cursor_line <- state.script_cursor_line - 1;
    state.script_cursor_preferred_column <- String.length line)
;;

let all_tc_bindings =
  List.concat
    [ (* Plain old character insertion *)
      [ " ", insert_char ' '
      ; "!", insert_char '!'
      ; "\"", insert_char '"'
      ; "#", insert_char '#'
      ; "$", insert_char '$'
      ; "%", insert_char '%'
      ; "&", insert_char '&'
      ; "'", insert_char '\''
      ; "(", insert_char '('
      ; ")", insert_char ')'
      ; "*", insert_char '*'
      ; "+", insert_char '+'
      ; ",", insert_char ','
      ; "-", insert_char '-'
      ; ".", insert_char '.'
      ; "/", insert_char '/'
      ; "0", insert_char '0'
      ; "1", insert_char '1'
      ; "2", insert_char '2'
      ; "3", insert_char '3'
      ; "4", insert_char '4'
      ; "5", insert_char '5'
      ; "6", insert_char '6'
      ; "7", insert_char '7'
      ; "8", insert_char '8'
      ; "9", insert_char '9'
      ; ":", insert_char ':'
      ; ";", insert_char ';'
      ; "<", insert_char '<'
      ; "=", insert_char '='
      ; ">", insert_char '>'
      ; "?", insert_char '?'
      ; "@", insert_char '@'
      ; "A", insert_char 'A'
      ; "B", insert_char 'B'
      ; "C", insert_char 'C'
      ; "D", insert_char 'D'
      ; "E", insert_char 'E'
      ; "F", insert_char 'F'
      ; "G", insert_char 'G'
      ; "H", insert_char 'H'
      ; "I", insert_char 'I'
      ; "J", insert_char 'J'
      ; "K", insert_char 'K'
      ; "L", insert_char 'L'
      ; "M", insert_char 'M'
      ; "N", insert_char 'N'
      ; "O", insert_char 'O'
      ; "P", insert_char 'P'
      ; "Q", insert_char 'Q'
      ; "R", insert_char 'R'
      ; "S", insert_char 'S'
      ; "T", insert_char 'T'
      ; "U", insert_char 'U'
      ; "V", insert_char 'V'
      ; "W", insert_char 'W'
      ; "X", insert_char 'X'
      ; "Y", insert_char 'Y'
      ; "Z", insert_char 'Z'
      ; "[", insert_char '['
      ; "\\", insert_char '\\'
      ; "]", insert_char ']'
      ; "^", insert_char '^'
      ; "_", insert_char '_'
      ; "`", insert_char '`'
      ; "a", insert_char 'a'
      ; "b", insert_char 'b'
      ; "c", insert_char 'c'
      ; "d", insert_char 'd'
      ; "e", insert_char 'e'
      ; "f", insert_char 'f'
      ; "g", insert_char 'g'
      ; "h", insert_char 'h'
      ; "i", insert_char 'i'
      ; "j", insert_char 'j'
      ; "k", insert_char 'k'
      ; "l", insert_char 'l'
      ; "m", insert_char 'm'
      ; "n", insert_char 'n'
      ; "o", insert_char 'o'
      ; "p", insert_char 'p'
      ; "q", insert_char 'q'
      ; "r", insert_char 'r'
      ; "s", insert_char 's'
      ; "t", insert_char 't'
      ; "u", insert_char 'u'
      ; "v", insert_char 'v'
      ; "w", insert_char 'w'
      ; "x", insert_char 'x'
      ; "y", insert_char 'y'
      ; "z", insert_char 'z'
      ; "{", insert_char '{'
      ; "|", insert_char '|'
      ; "}", insert_char '}'
      ; "~", insert_char '~'
      ]
    ; (* Control sequences *)
      [ "\x7f", delete_char
      ; "\n", newline
      ; "\x1b[A", cursor_up
      ; "\x1b[B", cursor_down
      ; "\x1b[C", cursor_right
      ; "\x1b[D", cursor_left
      ]
    ]
  |> List.to_seq
  |> String_map.of_seq
;;

let render state =
  Printf.printf "\x1b[0;0H\x1b[2J";
  let num_lines = Array.length state.script_lines in
  let lines_to_display = Int.min (state.terminal_height - 1) num_lines in
  let top = state.script_cursor_line - (lines_to_display / 2) in
  let top =
    if top + lines_to_display > num_lines then num_lines - lines_to_display else top
  in
  let top = if top < 0 then 0 else top in
  for i = 0 to lines_to_display - 1 do
    let line = state.script_lines.(top + i) in
    let len = String.length line in
    let line =
      if len > state.terminal_width then String.sub line 0 state.terminal_width else line
    in
    print_endline line
  done;
  let column = get_script_cursor_column state in
  Printf.printf "\x1b[%d;%dH" (state.script_cursor_line - top + 1) (column + 1);
  Out_channel.flush Out_channel.stdout
;;

let step_tc_bindings tc_bindings char =
  String_map.to_seq tc_bindings
  |> Seq.filter_map (fun (key, value) ->
    let len = String.length key in
    if len > 0 && Char.equal key.[0] char
    then Some (String.sub key 1 (len - 1), value)
    else None)
  |> String_map.of_seq
;;

let maybe_run_and_reset_bindings tc_bindings state =
  let num_tc_bindings = String_map.cardinal tc_bindings in
  if Int.equal num_tc_bindings 0
  then all_tc_bindings
  else if Int.equal num_tc_bindings 1
  then (
    match String_map.find_opt "" tc_bindings with
    | None -> tc_bindings
    | Some action ->
      action state;
      render state;
      all_tc_bindings)
  else tc_bindings
;;

let () =
  let terminal_width, terminal_height = terminal_size () in
  let argc = Array.length Sys.argv in
  let script_lines =
    if argc < 2
    then [| "" |]
    else if argc = 2
    then
      In_channel.with_open_text Sys.argv.(1) In_channel.input_all
      |> String.split_on_char '\n'
      |> Array.of_list
    else failwith "usage: me <file>"
  in
  let state =
    { terminal_width
    ; terminal_height
    ; script_lines
    ; script_cursor_line = 0
    ; script_cursor_preferred_column = 0
    }
  in
  let tc = Unix.tcgetattr Unix.stdin in
  Unix.tcsetattr Unix.stdout TCSANOW { tc with c_icanon = false; c_echo = false };
  let keep_going = ref true in
  let bytes = Bytes.create 1 in
  let rec loop old_tc_bindings =
    let new_tc_bindings =
      match Unix.select [ Unix.stdin ] [] [] 0.5 with
      | exception Unix.Unix_error (EINTR, _, _) -> old_tc_bindings
      | [], _, _ -> old_tc_bindings
      | _ :: _, _, _ ->
        let bytes_read = Unix.read Unix.stdin bytes 0 1 in
        assert (Int.equal bytes_read 1);
        let char = Bytes.get bytes 0 in
        let new_tc_bindings = step_tc_bindings old_tc_bindings char in
        let num_new_tc_bindings = String_map.cardinal new_tc_bindings in
        if Int.equal num_new_tc_bindings 0
        then (
          match String_map.find_opt "" old_tc_bindings with
          | None ->
            (* The character doesn't match any of the bindings, and there
               weren't any bindings that were already completed, so we just
               reset back to the initial set of bindings and ignore the
               character that just arrived. *)
            all_tc_bindings
          | Some action ->
            (* The character doesn't match any of the bindings, but there was
               already a binding that fully matched, so we execute it's action
               and then process the character relative to the full set of
               bindings. *)
            action state;
            render state;
            step_tc_bindings all_tc_bindings char)
        else new_tc_bindings
    in
    if !keep_going then loop (maybe_run_and_reset_bindings new_tc_bindings state)
  in
  let shutdown (_ : int) = keep_going := false in
  Sys.set_signal Sys.sigint (Signal_handle shutdown);
  Sys.set_signal Sys.sigterm (Signal_handle shutdown);
  Sys.set_signal Sys.sigquit (Signal_handle shutdown);
  let finally () =
    Unix.tcsetattr Unix.stdout TCSANOW tc;
    Printf.printf "\x1b[0;0H\x1b[2J";
    Out_channel.flush Out_channel.stdout
  in
  match
    render state;
    loop all_tc_bindings
  with
  | exception Bug (stacktrace, message) ->
    finally ();
    Format.eprintf "%s@." message;
    Printexc.print_raw_backtrace Out_channel.stderr stacktrace
  | exception exn ->
    finally ();
    raise exn
  | () -> finally ()
;;
