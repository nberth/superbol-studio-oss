(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2026 OCamlPro SAS                                       *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

(** Conversion of source texts into a given source format. *)

module SL = Source_lines

type warning =
  {
    line: int;                                   (* line in the input text *)
    message: string;
  }

(* Target formats *)

type target =
  {
    name: string;                       (* as in >>SOURCE FORMAT IS <name> *)
    indicator: [`Fixed of int | `Free];   (* width of the sequence area *)
    width: int option;      (* width of the text area, if limited; it starts
                               with area A *)
    padded_literals: bool;  (* a continued literal extends up to the end of
                               the text area *)
    split_literals: [`Continuation | `Concatenation];
    (* how to split alphanumeric literals that do not fit: [`Continuation] on
       the next line(s) with a [-] indicator, or [`Concatenation] of several
       literals with [&] (when a right margin restricts the text area, and
       continuation is not possible there) *)
  }

let target (format: Cobol_config.source_format) =
  let name = match format with
    | SFFixed -> "FIXED" | SFCOBOL85 -> "COBOL85" | SFVariable -> "VARIABLE"
    | SFxCard -> "XCARD" | SFCOBOLX -> "COBOLX" | SFFree -> "FREE"
    | SFXOpen -> "XOPEN" | SFCRT -> "CRT" | SFTrm -> "TERMINAL"
  in
  match Cobol_preproc.Src_format.from_config format with
  | SF (FixedIndic, FixedWidth { cut_at_col; alphanum_padding; _ }) ->
      Ok { name; indicator = `Fixed 6;
           width = Some (cut_at_col - 7);
           padded_literals = alphanum_padding <> None;
           split_literals = `Continuation }
  | SF (CBLXIndic, FixedWidth { cut_at_col; alphanum_padding; _ }) ->
      Ok { name; indicator = `Fixed 0;
           width = Some (cut_at_col - 1);
           padded_literals = alphanum_padding <> None;
           split_literals = `Continuation }
  | SF (NoIndic, FreePaging) ->
      Ok { name; indicator = `Free; width = None; padded_literals = false;
           split_literals = `Concatenation }
  | SF ((XOpenIndic | CRTIndic | TrmIndic), _) ->
      Error (Printf.sprintf "conversion into %s format is not supported" name)

let target_of_name name =
  match Cobol_config.Options.format_of_string name with
  | SF format -> target format
  | Auto | exception Invalid_argument _ ->
      Error (Printf.sprintf "invalid target source format %S" name)

let min_right_margin = 72

(* 0-based column where the text area starts *)
let text_column target =
  match target.indicator with `Fixed seq_width -> seq_width + 1 | `Free -> 0

let with_right_margin right_margin target =
  if right_margin < min_right_margin then
    Error (Printf.sprintf "right margin %d is less than %d" right_margin
             min_right_margin)
  else
    let margin_width = right_margin - text_column target in
    match target.width with
    | Some width when width <= margin_width ->
        Ok target                                                (* no effect *)
    | _ ->
        Ok { target with
             width = Some margin_width;
             (* literals that are padded up to the end of the text area of the
                format cannot be continued at the margin *)
             split_literals = if target.padded_literals || target.indicator = `Free
               then `Concatenation else `Continuation }

let supported_target_names =
  [ "FIXED"; "COBOL85"; "VARIABLE"; "XCARD"; "COBOLX"; "FREE" ]

let area_b = 4                     (* first column of area B, from area A *)

(* Maximum indentation of the lines that continue a line that is too long,
   relatively to area A. *)
let max_continuation_indent = 32

let spaces n = String.make (max 0 n) ' '

(* Output lines, with [content] from area A *)

type kind =
  | Code
  | Comment of char                                          (* '*' or '/' *)
  | Debug
  | Continuation

type line = { seq: string; kind: kind; content: string }

let render target { seq; kind; content } =
  match target.indicator, kind with
  | _, Code when content = "" ->
      ""
  | `Fixed seq_width, _ ->
      let seq =
        if String.length seq = seq_width then seq else spaces seq_width in
      let indicator = match kind with
        | Code -> ' ' | Comment c -> c | Debug -> 'D'
        | Continuation -> '-'
      in
      seq ^ String.make 1 indicator ^ content
  | `Free, (Code | Continuation) ->
      content
  | `Free, Comment _ ->
      "*>" ^ content
  | `Free, Debug ->
      ">>D" ^ (if content <> "" && content.[0] = ' ' then "" else " ") ^ content

let blank_seq = ""

(* --- *)

(* Sequence number of the given source line, if it is to be kept. *)
let sequence_of ~keep_sequence_numbers (line: SL.line) =
  match line.format with
  | SF (FixedIndic, _) when keep_sequence_numbers &&
                            String.length line.text >= 6 &&
                            not (String.contains (String.sub line.text 0 6)
                                   '\t') ->
      String.sub line.text 0 6
  | _ ->
      blank_seq

let rstrip s =
  let rec aux i = if i > 0 && s.[i - 1] = ' ' then aux (pred i) else i in
  String.sub s 0 (aux (String.length s))

(* Text of the line from byte [first] (included) to byte [last] (excluded),
   with tabs replaced with spaces. *)
let expand src (line: SL.line) ~first ~last =
  let b = Buffer.create (last - first) in
  for i = first to min last (String.length line.text) - 1 do
    match line.text.[i] with
    | '\t' ->
        Buffer.add_string b
          (spaces (SL.column src line (i + 1) - SL.column src line i))
    | c ->
        Buffer.add_char b c
  done;
  Buffer.contents b

(* [wrap ~width text] splits [text] at spaces into pieces of at most [width]
   characters (if given). *)
let wrap ~width text =
  let rec aux acc text =
    let len = String.length text in
    match width with
    | None -> List.rev (text :: acc)
    | Some width when len <= width -> List.rev (text :: acc)
    | Some width ->
        let cut =
          match String.rindex_from_opt text width ' ' with
          | Some i when i > 0 -> i
          | _ -> width
        in
        aux (rstrip (String.sub text 0 cut) :: acc)
          (String.sub text cut (len - cut))
  in
  aux [] text

(* Comment lines for a comment (that may be a floating comment), whose text
   is cut at the end of the text area of the source line, if any. *)
let comment_lines ~target src ~seq (line: SL.line) (c: SL.comment) =
  let text = c.comment_text in
  let text =
    match SL.cut_column line.format with
    | None -> text
    | Some cut_col ->
        match SL.locate src line cut_col with
        | Char i | Tab { offset = i; _ } when i > c.comment_offset ->
            String.sub text 0 (min (String.length text) (i - c.comment_offset))
        | _ -> text
  in
  let len = String.length text in
  let indicator, content =
    match c.comment_kind with
    | `Floating when len >= 2 ->                            (* starts with *> *)
        '*', String.sub text 2 (len - 2)
    | `Line when len > 0 && (text.[0] = '*' || text.[0] = '/') ->
        text.[0], String.sub text 1 (len - 1)
    | _ ->
        '*', text
  in
  List.mapi (fun i content ->
      { seq = if i = 0 then seq else blank_seq; kind = Comment indicator;
        content })
    (wrap (rstrip content)
       ~width:(match target.indicator with             (* room for the "*>" *)
           | `Free -> Option.map (fun w -> w - 2) target.width
           | `Fixed _ -> target.width))

(* --- *)

(* Items of a code line: the text of each word, along with the number of
   columns that separate it from the previous one. *)
type item = { gap: int; text: string }

(* [literal_quote text] gives the position of the opening quote of [text] if
   it is an alphanumeric literal (possibly with a prefix like X or N), along
   with the quote character. *)
let literal_quote text =
  let len = String.length text in
  let rec aux i =
    if i >= len then None
    else match text.[i] with
      | '"' | '\'' as q when len > i + 1 && text.[len - 1] = q -> Some (i, q)
      | 'A'..'Z' | 'a'..'z' when i < 2 -> aux (succ i)
      | _ -> None
  in
  aux 0

(* [close_literal text] closes [text] if it is an alphanumeric literal
   that is not closed (e.g, that reaches the end of the text area of a
   source line, without being continued on the next line). *)
let close_literal ~warn ~lnum text =
  let len = String.length text in
  let rec quote_pos i =
    if i >= len || i > 2 then None
    else match text.[i] with
      | '"' | '\'' as q -> Some (i, q)
      | 'A'..'Z' | 'a'..'z' -> quote_pos (succ i)
      | _ -> None
  in
  match quote_pos 0 with
  | Some (i, q) when len = i + 1 || text.[len - 1] <> q ->
      warn lnum "unclosed alphanumeric literal closed";
      text ^ String.make 1 q
  | _ ->
      text

(* Lays out [items] from column [indent] (relatively to area A), splitting
   them into several lines if needed. *)
let layout ~target ?(kind = Code) ~seq ~lnum ~warn ~indent items =
  let base_kind = kind in
  let total =
    List.fold_left (fun acc { gap; text } -> acc + gap + String.length text)
      0 items
  in
  let width = match target.width, target.indicator, kind with
    | Some w, `Free, Debug -> Some (w - 4)           (* room for the ">>D " *)
    | w, _, _ -> w
  in
  let fits n = match width with None -> true | Some w -> n <= w in
  let avail col = match width with None -> max_int | Some w -> w - col in
  (* shift lines that are too long to the left, without leaving area B *)
  let indent =
    match width with
    | Some width when indent + total > width && indent >= area_b ->
        max area_b (width - total)
    | _ -> indent
  in
  let cont_indent = min max_continuation_indent (max area_b (indent + 3)) in
  let lines = ref [] in
  let flush ~kind b =
    lines := { seq = if !lines = [] then seq else blank_seq;
               kind; content = Buffer.contents b } :: !lines;
    Buffer.clear b
  in
  let b = Buffer.create 80 in
  let kind = ref kind in
  (* the content of the current line is in [b], whose length is therefore the
     column of the next character, relatively to area A *)
  let has_word = ref false in
  let new_line ~at =
    flush ~kind:!kind b;
    (* lines that continue a debugging line are debugging lines as well *)
    kind := (match base_kind with Debug -> Debug | _ -> Code);
    has_word := false;
    Buffer.add_string b (spaces at)
  in
  (* Places literal [text] (with opening quote [q] at [qpos]) from the current
     position, continuing it on as many lines as needed. *)
  let rec place_literal text ~qpos ~q =
    let avail = avail (Buffer.length b) in
    let len = String.length text in
    if len <= avail then
      Buffer.add_string b text
    else
      (* Avoid cutting right after a quote or a space (that would be a
         trailing space). *)
      let good_cut n = n > qpos + 1 && text.[n - 1] <> q && text.[n - 1] <> ' '
      in
      let shift, cut =
        if target.padded_literals then
          (* The literal extends up to the end of the text area on the line
             where it is cut: shift it to the right a bit if needed. *)
          let rec find_shift shift =
            if shift > 4 then None
            else if good_cut (avail - shift) then Some shift
            else find_shift (succ shift)
          in
          match find_shift 0 with
          | Some shift -> shift, avail - shift
          | None -> 0, avail
        else
          (* The literal may end anywhere on the line where it is cut *)
          let rec find_cut n =
            if n <= qpos + 1 then None
            else if good_cut n then Some n
            else find_cut (pred n)
          in
          match find_cut avail with
          | Some cut -> 0, cut
          | None -> 0, avail
      in
      if text.[cut - 1] = q then
        warn lnum "literal continued right after a quote";
      Buffer.add_string b (spaces shift);
      Buffer.add_string b (String.sub text 0 cut);
      new_line ~at:area_b;
      if base_kind = Debug then
        warn lnum "literal continued on a line that is not a debugging line";
      kind := Continuation;
      place_literal (String.make 1 q ^ String.sub text cut (len - cut))
        ~qpos:0 ~q
  in
  (* Places basic alphanumeric literal [text] (delimited with [q]) from the
     current position, splitting it into several literals concatenated with
     [&] as needed.  Pieces are never cut between two quotes that stand for
     one quote. *)
  let rec concat_literal text ~q =
    let len = String.length text in
    let avail = avail (Buffer.length b) in
    if len <= avail then
      Buffer.add_string b text
    else
      (* [cut] is the length of the body of the first piece: the piece and
         " &" must fit *)
      let max_body = avail - 4 in
      let body = String.sub text 1 (len - 2) in
      (* [last] is the last valid cut so far, and [last_space] the last one
         right after a space, that is preferred if not too far back *)
      let rec find_cut i last last_space =
        if i > max_body || i >= String.length body then
          if last_space > 0 && last_space >= last - 20 then last_space else last
        else
          let last, last_space =
            if i = 0 then last, last_space
            else i, if body.[i - 1] = ' ' then i else last_space
          in
          find_cut (if body.[i] = q then i + 2 else i + 1) last last_space
      in
      match find_cut 0 0 0 with
      | 0 when !has_word ->
          new_line ~at:cont_indent;
          concat_literal text ~q
      | 0 ->
          warn lnum "literal could not be split to fit within the right margin";
          Buffer.add_string b text
      | cut ->
          let q' = String.make 1 q in
          Buffer.add_string b (q' ^ String.sub body 0 cut ^ q' ^ " &");
          new_line ~at:cont_indent;
          has_word := true;
          concat_literal
            (q' ^ String.sub body cut (String.length body - cut) ^ q') ~q
  in
  Buffer.add_string b (spaces indent);
  List.iter begin fun { gap; text } ->
    let gap = if !has_word then gap else 0 in
    let col = Buffer.length b in
    let len = String.length text in
    if fits (col + gap + len) then begin
      Buffer.add_string b (spaces gap);
      Buffer.add_string b text
    end else begin match literal_quote text, target.split_literals with
      | Some (0, q), `Concatenation ->
          (* move the literal to the next line if it fits there; split it
             otherwise *)
          if !has_word && fits (cont_indent + len)
          then new_line ~at:cont_indent
          else if !has_word && not (fits (col + gap + 6))
          then new_line ~at:cont_indent
          else Buffer.add_string b (spaces gap);
          concat_literal text ~q
      | Some (qpos, q), `Continuation ->
          (* start the literal here if its quote and a character fit *)
          if fits (col + gap + qpos + 2)
          then Buffer.add_string b (spaces gap)
          else if !has_word then new_line ~at:cont_indent;
          place_literal text ~qpos ~q
      | Some _, `Concatenation | None, _ ->
          if !has_word then new_line ~at:cont_indent;
          if not (fits (Buffer.length b + len)) then
            warn lnum (Printf.sprintf "word %S extends beyond the text area"
                         text);
          Buffer.add_string b text
    end;
    has_word := true
  end items;
  flush ~kind:!kind b;
  List.rev !lines

(* --- *)

let code_lines ~target src ~seq ~warn (line: SL.line) =
  let text_col = SL.text_column line.format in
  let items, _ =
    List.fold_left begin fun (items, prev_end) (w: SL.word) ->
      let start_col = SL.column src line w.start_offset in
      let gap = match prev_end with None -> 0 | Some e -> start_col - e in
      match w.end_offset, w.text with
      | Some e, Some text ->
          let text = close_literal ~warn ~lnum:line.lnum text in
          { gap; text } :: items, Some (SL.column src line e)
      | Some e, None ->
          { gap; text = expand src line ~first:w.start_offset ~last:e }
          :: items, Some (SL.column src line e)
      | None, Some text ->
          { gap; text } :: items, None
      | None, None ->
          let e = String.length line.text in
          { gap; text = expand src line ~first:w.start_offset ~last:e }
          :: items, None
    end ([], None) line.words
  in
  let items = List.rev items in
  let first_col = match line.words with
    | w :: _ -> SL.column src line w.start_offset
    | [] -> text_col
  in
  match items with
  | { text; _ } :: _ when String.length text > 0 && text.[0] = '$' ->
      (* MF-style directive: from area A, on a single line (GnuCOBOL does not
         accept `$' in the indicator area) *)
      let content =
        String.concat "" @@
        List.mapi (fun i { gap; text } ->
            if i = 0 then text else spaces gap ^ text) items
      in
      [{ seq; kind = Code; content }]
  | { text; _ } :: ({ gap; _ } :: _ as items)
    when String.uppercase_ascii text = ">>D" ->
      (* debugging line in free-like formats *)
      layout ~target ~kind:Debug ~seq ~lnum:line.lnum ~warn
        ~indent:(max 0 (first_col + String.length text + gap - text_col))
        items
  | _ ->
      layout ~target ~seq ~lnum:line.lnum ~warn
        ~indent:(max 0 (first_col - text_col))
        items

(* Debugging line, recognized from its raw text. *)
let debug_line src ~seq (line: SL.line) =
  let text_col = SL.text_column line.format in
  let last =                        (* end of the text area of the source line *)
    match SL.cut_column line.format with
    | None -> String.length line.text
    | Some cut_col ->
        match SL.locate src line cut_col with
        | Char i | Tab { offset = i; _ } -> i
        | Short -> String.length line.text
  in
  let content ~from = rstrip (expand src line ~first:from ~last) in
  if SL.has_indicator line.format then
    match SL.locate src line (text_col - 1) with
    | Char i when line.text.[i] = 'D' || line.text.[i] = 'd' ->
        Some { seq; kind = Debug; content = content ~from:(i + 1) }
    | _ -> None
  else
    let len = String.length line.text in
    let rec first_non_blank i =
      if i < len && (line.text.[i] = ' ' || line.text.[i] = '\t')
      then first_non_blank (succ i) else i
    in
    let i = first_non_blank 0 in
    if i + 3 <= len &&
       String.uppercase_ascii (String.sub line.text i 3) = ">>D" &&
       (i + 3 = len || line.text.[i + 3] = ' ')
    then
      let from = min len (i + 3) in
      let from = first_non_blank from in
      let rel = max 0 (SL.column src line from - text_col) in
      Some { seq; kind = Debug; content = spaces rel ^ content ~from }
    else None

let is_blank s =
  String.for_all (function ' ' | '\t' -> true | _ -> false) s

let convert_line ~target src ~keep_sequence_numbers ~warn (line: SL.line) =
  let seq = sequence_of ~keep_sequence_numbers line in
  let comments, floating =
    List.partition (fun (c: SL.comment) -> c.comment_kind = `Line)
      line.comments
  in
  (* In FREE format, floating comments stay at the end of their code line;
     otherwise, they go on comment lines before the code. *)
  let keep_floating = target.indicator = `Free && line.words <> [] &&
                      not line.source_format_directive in
  let floating_lines =
    if keep_floating then []
    else List.concat_map (comment_lines ~target src ~seq:blank_seq line)
        floating
  in
  let main =
    if line.source_format_directive then begin
      warn line.lnum "source format directive commented out";
      let first = match line.words with
        | w :: _ -> w.start_offset
        | [] -> 0
      in
      [{ seq; kind = Comment '*';
         content = " " ^ rstrip (expand src line ~first
                                   ~last:(String.length line.text)) }]
    end else if line.words <> [] then
      code_lines ~target src ~seq ~warn line
    else if comments <> [] then
      List.concat_map (comment_lines ~target src ~seq line) comments
    else if line.continued || is_blank line.text then
      []                    (* continuation of a previous line, or blank line *)
    else match debug_line src ~seq line with
      | Some l ->
          let width = match target.indicator with
            | `Free -> Option.map (fun w -> w - 4) target.width
            | `Fixed _ -> target.width
          in
          if match width with
            | Some w -> String.length l.content > w
            | None -> false
          then warn line.lnum "debugging line extends beyond the text area";
          [l]
      | None when floating <> [] -> []
      | None ->
          (* e.g, text in the sequence area only *)
          warn line.lnum "line ignored";
          []
  in
  let main =
    if not keep_floating || floating = [] then main
    else match List.rev main with
      | last :: rev_main ->
          let last_end =
            List.fold_left (fun _ (w: SL.word) -> w.end_offset)
              None line.words
          in
          let comments =
            List.map (fun (c: SL.comment) ->
                let gap = match last_end with
                  | Some e ->
                      SL.column src line c.comment_offset - SL.column src line e
                  | None -> 1
                in
                spaces (max 1 gap) ^ rstrip c.comment_text) floating
          in
          let content = last.content ^ String.concat "" comments in
          if match target.width with
            | Some w -> String.length content > w
            | None -> false
          then (* move them on comment lines before the code *)
            List.concat_map (comment_lines ~target src ~seq:blank_seq line)
              floating @ main
          else List.rev ({ last with content } :: rev_main)
      | [] -> main
  in
  let blank = line.words = [] && line.comments = [] &&
              not line.continued && is_blank line.text in
  if blank then [{ seq = blank_seq; kind = Code; content = "" }]
  else floating_lines @ main

let eol_of_text text =
  match String.index_opt text '\n' with
  | Some i when i > 0 && text.[i - 1] = '\r' -> "\r\n"
  | _ -> "\n"

let convert ?(keep_sequence_numbers = false)
    ?(add_source_format_directive = false) ~target (src: SL.t) ~eol =
  let rev_warnings = ref [] in
  let warn line message = rev_warnings := { line; message } :: !rev_warnings in
  let lines =
    Array.to_list src.lines |>
    List.concat_map (convert_line ~target src ~keep_sequence_numbers ~warn)
  in
  let lines =
    if add_source_format_directive
    then { seq = blank_seq; kind = Code;
           content = ">>SOURCE FORMAT IS " ^ target.name } :: lines
    else lines
  in
  let text =
    String.concat eol @@ List.rev @@             (* there may be many lines *)
    List.rev_map (render target) lines
  in
  text, List.rev !rev_warnings
