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

open Types

let spaces n = String.make (max 0 n) ' '

(* Edit that keeps whatever follows the text area (e.g, columns 73 and above
   in fixed format) in place after the line has moved by [delta] columns.
   Only spaces that end the text area are removed, and nothing is done if
   only blanks follow the text area. *)
let margin_edit src ~cut_col ~min_offset (line: Source_lines.line) delta =
  let blank_from i =
    let rec aux i =
      i >= String.length line.text ||
      (match line.text.[i] with ' ' | '\t' -> aux (succ i) | _ -> false)
    in
    aux i
  in
  match Source_lines.locate src line cut_col with
  | Char cut when delta = 0 || blank_from cut ->
      []
  | Char cut when delta > 0 ->
      let rec count_spaces i =
        if i > min_offset && line.text.[i - 1] = ' '
        then count_spaces (pred i) else i
      in
      let start = max (count_spaces cut) (cut - delta) in
      if start < cut
      then [{ line_number = line.lnum; start_offset = start;
              end_offset = cut; new_text = "" }]
      else []
  | Char cut ->
      [{ line_number = line.lnum; start_offset = cut;
         end_offset = cut; new_text = spaces (- delta) }]
  | Tab _ | Short ->
      []

let line_edits (src: Source_lines.t) (edit: indent_record) =
  if edit.lnum < 1 || edit.lnum > Array.length src.lines then [] else
    let line = src.lines.(edit.lnum - 1) in
    match line.words with
    | [] ->
        []
    | { start_offset = first; _ } :: _ ->
        let text_col = Source_lines.text_column line.format in
        let new_indent = edit.base_col + edit.offset_modif - text_col in
        (* Replace everything from the start of the text area up to the first
           word.  A tab that spans the start of the text area is replaced with
           spaces, so the text before it stays aligned. *)
        let indent_edit start ~prefix =
          { line_number = line.lnum; start_offset = start; end_offset = first;
            new_text = spaces prefix ^ spaces new_indent }
        in
        let indent_edit =
          match Source_lines.locate src line text_col with
          | Char start when start <= first ->
              Some (indent_edit start ~prefix:0)
          | Tab { offset; start_col; _ } when offset <= first ->
              Some (indent_edit offset ~prefix:(text_col + 1 - start_col))
          | Char _ | Tab _ | Short ->
              None
        in
        match indent_edit, Source_lines.cut_column line.format with
        | None, _ ->
            []
        | Some indent_edit, None ->
            [indent_edit]
        | Some indent_edit, Some cut_col ->
            let last =
              List.fold_left begin fun acc Source_lines.{ end_offset; _ } ->
                Option.value end_offset ~default:acc
              end first line.words
            in
            indent_edit ::
            margin_edit src ~cut_col ~min_offset:last line
              (edit.offset_modif - edit.offset_orig)

let edits src indent_records =
  List.concat_map (line_edits src) indent_records

(* Applies edits that are sorted by line and offset, and do not overlap. *)
let apply contents edits =
  let b = Buffer.create (String.length contents + 1024) in
  let len = String.length contents in
  let next_bol bol =
    match String.index_from_opt contents bol '\n' with
    | Some i -> i + 1
    | None -> len
  in
  (* [bol] is the position in [contents] of the start of line [lnum], and
     [cur] is the position of the first character that is not copied yet. *)
  let rec iter ~cur ~bol ~lnum = function
    | [] ->
        Buffer.add_substring b contents cur (len - cur)
    | { line_number; _ } :: _ as edits when line_number > lnum ->
        if bol >= len
        then iter ~cur ~bol ~lnum []
        else iter ~cur ~bol:(next_bol bol) ~lnum:(succ lnum) edits
    | { line_number; start_offset; end_offset; new_text } :: edits ->
        let start = bol + start_offset and end_ = bol + end_offset in
        if line_number < lnum || start < cur || end_ > len
        then iter ~cur ~bol ~lnum edits                         (* unexpected *)
        else begin
          Buffer.add_substring b contents cur (start - cur);
          Buffer.add_string b new_text;
          iter ~cur:end_ ~bol ~lnum edits
        end
  in
  iter ~cur:0 ~bol:0 ~lnum:1 edits;
  Buffer.contents b
