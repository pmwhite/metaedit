let ( = ) = Int.equal
let ( > ) (a : int) (b : int) = a > b

module String_map = Map.Make (String)

exception Bug of Printexc.raw_backtrace * string

let bugf fmt =
  Printf.ksprintf
    (fun s ->
      raise_notrace (Bug (Printexc.get_raw_backtrace (), Printf.sprintf "BUG: %s" s)))
    fmt
;;

let insert_char_at ~char ~at into =
  let len = String.length into in
  if at < 0 || at > len
  then bugf "insert_char_at called with out-of-range index. at = %d, len = %d" at len;
  let before = String.sub into 0 at in
  let after = String.sub into at (len - at) in
  Printf.sprintf "%s%c%s" before char after
;;

let delete_char_at ~at from =
  let len = String.length from in
  if at < 0 || at >= len
  then bugf "delete_char_at called with out-of-range index. at = %d, len = %d" at len;
  let before = String.sub from 0 at in
  let after = String.sub from (at + 1) (len - at - 1) in
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

module Editor = struct
  module Lines : sig
    type t

    val empty : t
    val singleton : string -> t
    val of_lines : string list -> t
    val concat : t list -> t
    val map_range : t -> int -> int -> f:(string list -> string list) -> t
    val map_line : t -> int -> f:(string -> string list) -> t
    val get : t -> int -> string
    val length : t -> int
  end = struct
    type t = string array

    let empty = [||]
    let singleton l = [| l |]
    let of_lines = Array.of_list
    let concat = Array.concat
    let length t = Array.length t

    let map_range t from to_ ~f =
      let len = length t in
      if from < 0 || from > len || to_ < 0 || to_ > len || from > to_
      then
        bugf
          "Lines.map_range: contract violated. from = %d, to_ = %d, len = %d"
          from
          to_
          len;
      let before = Array.sub t 0 from in
      let middle = Array.of_list (f (Array.to_list (Array.sub t from (to_ - from)))) in
      let after = Array.sub t to_ (len - to_) in
      Array.concat [ before; middle; after ]
    ;;

    let map_line t i ~f =
      let len = length t in
      if i < 0 || i >= len
      then bugf "Lines.map_line: contract violated. i = %d, len = %d" i len;
      map_range t i (i + 1) ~f:(ListLabels.concat_map ~f)
    ;;

    let get t i =
      let len = Array.length t in
      if i < 0 || i >= len
      then bugf "Lines.get: contract violated. i = %d, len = %d" i len;
      t.(i)
    ;;
  end

  type t =
    { lines : Lines.t
    ; cursor_line : int
    ; cursor_preferred_column : int
    }

  let cursor_line t = Lines.get t.lines t.cursor_line

  let get_cursor_column t =
    Int.min t.cursor_preferred_column (String.length (cursor_line t))
  ;;

  let insert_char char t =
    let column = get_cursor_column t in
    { t with
      lines =
        Lines.map_line t.lines t.cursor_line ~f:(fun line ->
          [ insert_char_at ~char ~at:column line ])
    ; cursor_preferred_column = column + 1
    }
  ;;

  let delete_char t =
    let column = get_cursor_column t in
    if column > 0
    then
      { t with
        lines =
          Lines.map_line t.lines t.cursor_line ~f:(fun line ->
            [ delete_char_at ~at:(column - 1) line ])
      ; cursor_preferred_column = column - 1
      }
    else (
      assert (column = 0);
      if t.cursor_line > 0
      then (
        let line_before = Lines.get t.lines (t.cursor_line - 1) in
        { lines =
            Lines.map_range
              t.lines
              (t.cursor_line - 1)
              (t.cursor_line + 1)
              ~f:(fun lines -> [ String.concat "" lines ])
        ; cursor_line = t.cursor_line - 1
        ; cursor_preferred_column = String.length line_before
        })
      else t)
  ;;

  let newline t =
    let column = get_cursor_column t in
    { lines =
        Lines.map_line t.lines t.cursor_line ~f:(fun line ->
          let before, after = split_string_at line ~at:column in
          [ before; after ])
    ; cursor_line = t.cursor_line + 1
    ; cursor_preferred_column = 0
    }
  ;;

  let cursor_up t =
    if t.cursor_line > 0 then { t with cursor_line = t.cursor_line - 1 } else t
  ;;

  let cursor_down t =
    if t.cursor_line < Lines.length t.lines - 1
    then { t with cursor_line = t.cursor_line + 1 }
    else t
  ;;

  let cursor_right t =
    let line = cursor_line t in
    let column = get_cursor_column t in
    if column < String.length line
    then { t with cursor_preferred_column = column + 1 }
    else if t.cursor_line < Lines.length t.lines - 1
    then { t with cursor_line = t.cursor_line + 1; cursor_preferred_column = 0 }
    else t
  ;;

  let cursor_left t =
    let column = get_cursor_column t in
    if column > 0
    then { t with cursor_preferred_column = column - 1 }
    else if t.cursor_line > 0
    then (
      let line = Lines.get t.lines (t.cursor_line - 1) in
      { t with
        cursor_line = t.cursor_line - 1
      ; cursor_preferred_column = String.length line
      })
    else t
  ;;
end

type workspace =
  { editors : Editor.t String_map.t
  ; focused : string
  }

type state =
  { mutable terminal_width : int
  ; mutable terminal_height : int
  ; mutable script : Editor.t
  ; mutable workspace : workspace
  }

module Parser : sig
  type 'a t

  val fail : 'a t
  val return : 'a -> 'a t
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val both : 'a t -> 'b t -> ('a * 'b) t
  val satisfies : (char -> bool) -> char t
  val or_ : 'a t -> 'a t -> 'a t
  val one_of : 'a t list -> 'a t
  val fix : ('a t -> 'a t) -> 'a t
  val fix2 : ('a t -> 'b t -> 'a t) -> ('a t -> 'b t -> 'b t) -> 'a t * 'b t
  val run : 'a t -> string -> 'a list

  module O : sig
    val return : 'a -> 'a t
    val ( <|> ) : 'a t -> 'a t -> 'a t
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
    val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
    val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
    val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
    val ( <* ) : 'a t -> 'b t -> 'a t
    val ( *> ) : 'a t -> 'b t -> 'b t
  end
end = struct
  type 'a t = char list -> ('a * char list) list

  let fail _ = []
  let return x cs = [ x, cs ]
  let map t ~f cs = ListLabels.map (t cs) ~f:(fun (x, cs) -> f x, cs)
  let bind t ~f cs = ListLabels.concat_map (t cs) ~f:(fun (x, cs) -> f x cs)
  let both a b = bind a ~f:(fun a -> map b ~f:(fun b -> a, b))

  let satisfies f cs =
    match cs with
    | [] -> []
    | c :: cs -> if f c then [ c, cs ] else []
  ;;

  let or_ a b cs = a cs @ b cs
  let one_of ts = ListLabels.fold_left ts ~init:fail ~f:or_
  let rec fix f cs = f (fix f) cs

  let rec fix2 f g =
    ( (fun cs ->
        let f', g' = fix2 f g in
        f f' g' cs)
    , fun cs ->
        let f', g' = fix2 f g in
        g f' g' cs )
  ;;

  let run t s =
    ListLabels.filter_map
      (t (List.of_seq (String.to_seq s)))
      ~f:(fun (x, cs) ->
        match cs with
        | [] -> Some x
        | _ :: _ -> None)
  ;;

  module O = struct
    let return = return
    let ( <|> ) = or_
    let ( let+ ) t f = map t ~f
    let ( and+ ) = both
    let ( let* ) t f = bind t ~f
    let ( and* ) = both
    let ( <$> ) f t = map t ~f
    let ( <*> ) f t = map (both f t) ~f:(fun (f, t) -> f t)
    let ( <* ) a b = map (both a b) ~f:fst
    let ( *> ) a b = map (both a b) ~f:snd
  end
end

module Syntax = struct
  open Parser.O

  module Expr = struct
    type t =
      | Int of int
      | String of string
      | Var of string
      | Call of
          { func : string
          ; args : t list
          }
  end

  module Stmt = struct
    type stmt =
      | Assign of
          { var : string
          ; expr : Expr.t
          }
      | Run of Expr.t
  end

  let many p = Parser.fix (fun ps -> return [] <|> (List.cons <$> p <*> ps))
  let many1 p = List.cons <$> p <*> many p
  let many1_sep_by p ~sep = List.cons <$> p <*> many (sep *> p)

  let lower_alpha_char =
    Parser.satisfies (function
      | 'a' .. 'z' -> true
      | _ -> false)
  ;;

  let white_char =
    Parser.satisfies (function
      | ' ' | '\t' | '\n' -> true
      | _ -> false)
  ;;

  let digit_char =
    Parser.satisfies (function
      | '0' .. '9' -> true
      | _ -> false)
  ;;

  let char c = Parser.satisfies (Char.equal c)
  let white = many white_char *> return ()
  let white1 = many white_char *> return ()

  let many_string p =
    let+ chars = many1 p in
    String.of_seq (List.to_seq chars)
  ;;

  let ident = many_string lower_alpha_char

  let int =
    let* s = many_string digit_char in
    match int_of_string_opt s with
    | Some s -> return s
    | None -> Parser.fail
  ;;

  let string =
    let string_item =
      let literal_char =
        Parser.satisfies (function
          | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
          | _ -> false)
      in
      let escaped c result =
        let+ () = char '\\' *> return ()
        and+ () = char c *> return () in
        result
      in
      literal_char <|> escaped '"' '"' <|> escaped '\\' '\\' <|> escaped 'n' '\n'
    in
    char '"' *> many_string string_item <* char '"'
  ;;

  let expr =
    Parser.fix (fun expr ->
      let var =
        let+ var = ident in
        Expr.Var var
      in
      let int =
        let+ int = int in
        Expr.Int int
      in
      let string =
        let+ string = string in
        Expr.String string
      in
      let factor = var <|> int <|> string <|> (char '(' *> expr <* char ')') in
      let call =
        let+ func = ident
        and+ () = white1
        and+ args = many1_sep_by expr ~sep:white1 in
        Expr.Call { func; args }
      in
      factor <|> call)
  ;;

  let stmt =
    let assign =
      let+ var = ident
      and+ () = white
      and+ () = char '=' *> return ()
      and+ () = white
      and+ expr = expr in
      Stmt.Assign { var; expr }
    in
    let run =
      let+ expr = expr in
      Stmt.Run expr
    in
    assign <|> run
  ;;
end

module Action = struct
  type 'a args =
    | Int : string -> int args
    | String : string -> string args
    | Map : 'a args * ('a -> 'b) -> 'b args
    | Both : 'a args * 'b args -> ('a * 'b) args

  type t =
    { name : string
    ; args : (unit -> workspace -> workspace) args
    }

  let create ~name args = { name; args }

  let help t =
    let rec words : type a. string list -> a args -> string list =
      fun acc args ->
      match args with
      | Int name -> Printf.sprintf "%s:int" name :: acc
      | String name -> Printf.sprintf "%s:string" name :: acc
      | Map (args, _) -> words acc args
      | Both (a, b) -> words (words acc b) a
    in
    let words = t.name :: words [] t.args in
    String.concat " " words
  ;;

  module O = struct
    let ( let+ ) x f = Map (x, f)
    let ( and+ ) a b = Both (a, b)
  end
end

let open_file =
  Action.create
    ~name:"openFile"
    (let open Action.O in
     let+ path = String "path"
     and+ editor_name = String "editorName" in
     fun () ->
       let editor =
         { Editor.lines =
             In_channel.with_open_text path In_channel.input_all
             |> String.split_on_char '\n'
             |> Editor.Lines.of_lines
         ; cursor_line = 0
         ; cursor_preferred_column = 0
         }
       in
       fun w -> { w with editors = String_map.add editor_name editor w.editors })
;;

let all_tc_bindings =
  let insert_char = Editor.insert_char in
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
      [ "\x7f", Editor.delete_char
      ; "\n", Editor.newline
      ; "\x1b[A", Editor.cursor_up
      ; "\x1b[B", Editor.cursor_down
      ; "\x1b[C", Editor.cursor_right
      ; "\x1b[D", Editor.cursor_left
      ]
    ]
  |> List.to_seq
  |> String_map.of_seq
;;

let render state =
  Printf.printf "\x1b[0;0H\x1b[2J";
  let num_lines = Editor.Lines.length state.script.lines in
  let lines_to_display = Int.min (state.terminal_height - 1) num_lines in
  let top = state.script.cursor_line - (lines_to_display / 2) in
  let top =
    if top + lines_to_display > num_lines then num_lines - lines_to_display else top
  in
  let top = if top < 0 then 0 else top in
  for i = 0 to lines_to_display - 1 do
    let line = Editor.Lines.get state.script.lines (top + i) in
    let len = String.length line in
    let line =
      if len > state.terminal_width then String.sub line 0 state.terminal_width else line
    in
    print_endline line
  done;
  let column = Editor.get_cursor_column state.script in
  Printf.printf "\x1b[%d;%dH" (state.script.cursor_line - top + 1) (column + 1);
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
      state.script <- action state.script;
      render state;
      all_tc_bindings)
  else tc_bindings
;;

let () =
  let terminal_width, terminal_height = terminal_size () in
  let argc = Array.length Sys.argv in
  let script_lines =
    if argc < 2
    then Editor.Lines.singleton ""
    else if argc = 2
    then
      In_channel.with_open_text Sys.argv.(1) In_channel.input_all
      |> String.split_on_char '\n'
      |> Editor.Lines.of_lines
    else failwith "usage: me <file>"
  in
  let state =
    { terminal_width
    ; terminal_height
    ; script = { lines = script_lines; cursor_line = 0; cursor_preferred_column = 0 }
    ; workspace =
        { editors =
            String_map.singleton
              "scratch"
              { Editor.lines = Editor.Lines.singleton ""
              ; cursor_line = 0
              ; cursor_preferred_column = 0
              }
        ; focused = "scratch"
        }
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
            state.script <- action state.script;
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
