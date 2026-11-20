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

open Cobol_common.Srcloc.INFIX

module SF = Cobol_preproc.Src_format
module Tab_stops = Cobol_common.Tab_stops

type word =
  {
    start_offset: int;
    end_offset: int option;
  }

type comment =
  {
    comment_offset: int;
    comment_kind: [`Line | `Floating];
    comment_text: string;
  }

type line =
  {
    lnum: int;
    text: string;
    format: SF.any;
    words: word list;
    comments: comment list;
    continued: bool;
    continues: bool;
  }

type t =
  {
    lines: line array;
    tab_stops: int list;
  }

let text_column: SF.any -> int = function
  | SF (FixedIndic, _) -> 7
  | SF (CBLXIndic, _) -> 1
  | SF ((NoIndic | XOpenIndic | CRTIndic | TrmIndic), _) -> 0

let cut_column: SF.any -> int option = function
  | SF (_, FixedWidth { cut_at_col; _ }) -> Some cut_at_col
  | SF (_, FreePaging) -> None

let has_indicator: SF.any -> bool = function
  | SF ((FixedIndic | CBLXIndic), _) -> true
  | SF ((NoIndic | XOpenIndic | CRTIndic | TrmIndic), _) -> false

(* Number of bytes of the byte order mark that may start the first line;
   they do not count as columns. *)
let bom_length { lnum; text; _ } =
  if lnum = 1 && String.starts_with ~prefix:"\xEF\xBB\xBF" text then 3 else 0

let column { tab_stops; _ } line offset =
  let skip = bom_length line in
  let text = String.sub line.text skip (String.length line.text - skip) in
  Tab_stops.column_at ~tab_stops text (max 0 (offset - skip)) - 1

let locate { tab_stops; _ } line col : Tab_stops.column_spot =
  let skip = bom_length line in
  let text = String.sub line.text skip (String.length line.text - skip) in
  match Tab_stops.locate_column ~tab_stops text (col + 1) with
  | Char i -> Char (i + skip)
  | Tab t -> Tab { t with offset = t.offset + skip }
  | Short -> Short

let split_lines contents =
  let strip_cr l =
    let len = String.length l in
    if len > 0 && l.[len - 1] = '\r' then String.sub l 0 (len - 1) else l
  in
  Array.map strip_cr @@ Array.of_list @@ String.split_on_char '\n' contents

(* Accumulator for a line, with words in reverse order. *)
type acc_line =
  {
    mutable rev_words: word list;
    mutable rev_comments: comment list;
    mutable is_continued: bool;
    mutable line_format: SF.any option;
  }

let scan ~(options: Cobol_preproc.Options.preproc_options) ~filename contents =
  let Cobol_preproc.Options.{ config; source_format; mf_comments;
                              platform; _ } = options in
  let tab_stops = config.tab_stops in
  let texts = split_lines contents in
  let nlines = Array.length texts in
  let acc =
    Array.init nlines
      (fun _ -> { rev_words = []; rev_comments = []; is_continued = false;
                  line_format = None })
  in
  let valid l = 1 <= l && l <= nlines in
  let add_word w =
    match Cobol_common.Srcloc.as_lexloc ~@w with
    | exception Invalid_argument _ ->      (* not a location in the input *)
        ()
    | s, e when valid s.pos_lnum ->
        (* Note: an alphanumeric literal that is not closed on a line of
           fixed width ends at the end of the text area, possibly beyond the
           last character of the line. *)
        let len = String.length texts.(s.pos_lnum - 1) in
        let start_offset = min len (s.pos_cnum - s.pos_bol) in
        let a = acc.(s.pos_lnum - 1) in
        if e.pos_lnum = s.pos_lnum then
          a.rev_words <- { start_offset;
                           end_offset = Some (min len (e.pos_cnum - e.pos_bol)) }
                         :: a.rev_words
        else begin
          a.rev_words <- { start_offset; end_offset = None } :: a.rev_words;
          for l = s.pos_lnum + 1 to min e.pos_lnum nlines do
            acc.(l - 1).is_continued <- true
          done
        end
    | _ ->
        ()
  in
  (* [l] is the number of the last line read: a new format applies from the
     line that follows. *)
  let _, rev_changes =
    Cobol_preproc.fold_source_lines ~platform ~source_format
      ~src_options:Cobol_preproc.Src_options.{ tab_stops; mf_comments }
      ~dialect:(Cobol_config.dialect config)
      ~on_change_of_source_format:(fun sf (l, changes) -> l, (l, sf) :: changes)
      ~on_comments:begin fun comments res ->
        List.iter begin fun Cobol_preproc.Text.{ comment_loc = (s, _);
                                                  comment_kind;
                                                  comment_contents } ->
          if valid s.pos_lnum then
            let a = acc.(s.pos_lnum - 1) in
            a.rev_comments <- { comment_offset = s.pos_cnum - s.pos_bol;
                                comment_kind;
                                comment_text = comment_contents }
                              :: a.rev_comments
        end comments;
        res
      end
      ~f:begin fun l text (_, changes) ->
        List.iter begin fun w -> match ~&w with
          | Cobol_preproc.Text.Eof -> ()
          | _ -> add_word w
        end text;
        l, changes
      end
      (Cobol_preproc.Input.string ~filename contents)
      (0, [])
  in
  let changes = List.rev rev_changes in
  List.iter begin fun (l, sf) ->
    if l < nlines then acc.(l).line_format <- Some sf
  end changes;
  let format = ref (match changes with
      | (_, sf) :: _ -> sf
      | [] -> SF.from_config Cobol_config.Types.SFFixed)
  in
  let lines =
    Array.mapi begin fun i text ->
      let a = acc.(i) in
      Option.iter (fun sf -> format := sf) a.line_format;
      let words = List.rev a.rev_words in
      { lnum = i + 1; text; format = !format; words;
        comments = List.rev a.rev_comments;
        continued = a.is_continued;
        continues = List.exists (fun w -> w.end_offset = None) words }
    end texts
  in
  let src = { lines; tab_stops } in
  (* Also consider lines with a continuation indicator, as the preprocessor
     may not consider them as such (e.g, when the continued literal does not
     start with a quote), along with the line they continue. *)
  let prev = ref None in
  Array.iteri begin fun i line ->
    let has_continuation_indicator =
      has_indicator line.format &&
      match locate src line (text_column line.format - 1) with
      | Char j -> line.text.[j] = '-'
      | Tab _ | Short -> false
    in
    if has_continuation_indicator then begin
      lines.(i) <- { line with continued = true };
      Option.iter (fun j -> lines.(j) <- { lines.(j) with continues = true })
        !prev
    end;
    if line.words <> [] || has_continuation_indicator then prev := Some i
  end lines;
  src
