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

type result = {
  toks : (Types.token * Types.token_descr) list ;
  edits : Types.indent_record list ;
}

(* Words of a comment-entry that continue on the next lines are indented by
   more than this, relatively to area A. *)
let comment_entry_min_indent = 4

(* Lexes the bytes [first, last) of the line, and returns the tokens in
   reverse order, along with their offset in the line. *)
let lex_slice text ~first ~last =
  if last <= first then [] else
    let slice = String.uppercase_ascii (String.sub text first (last - first)) in
    let lexbuf = Lexing.from_string slice in
    let rec iter rev =
      match Lexer.line lexbuf with
      | EOF -> rev
      | COMMENT _ -> iter rev
      | token ->
          let start = Lexing.lexeme_start lexbuf in
          let len = Lexing.lexeme_end lexbuf - start in
          iter ((token, first + start, len) :: rev)
    in
    iter []

(* Tokens of the line, in reverse order, with their offset in the line and
   length. *)
let line_revtokens (line: Source_lines.line) =
  let rec split_words first last = function
    | [] ->
        lex_slice line.text ~first ~last
    | { Source_lines.end_offset = Some e; _ } :: words ->
        split_words first e words
    | { start_offset; end_offset = None; _ } :: _ ->
        let len = String.length line.text - start_offset in
        (CHARS (String.uppercase_ascii (String.sub line.text start_offset len)),
         start_offset, len) ::
        lex_slice line.text ~first ~last
  in
  match line.words with
  | [] -> []
  | { start_offset; _ } :: _ as words -> split_words start_offset start_offset words

let tokenize ~config (src: Source_lines.t) =
  Lexer.init ();
  let maybe_comment_entry = ref false in
  let revtoks = ref [] and revedits = ref [] in
  Array.iter begin fun (line: Source_lines.line) ->
    match line.words with
    | [] ->
        ()                             (* blank, comment, or debugging line *)
    | { start_offset; _ } :: _ ->
        let text_col = Source_lines.text_column line.format in
        let first_col = Source_lines.column src line start_offset in
        if first_col < text_col then
          (* text before area A, e.g, in the indicator area: leave the line
             alone *)
          maybe_comment_entry := false
        else if !maybe_comment_entry &&
                first_col - text_col >= comment_entry_min_indent then
          ()                                  (* continued comment-entry *)
        else begin
          let base_col = text_col + config.whole_file_indent in
          let max_text_length =
            match Source_lines.cut_column line.format with
            | Some cut_col -> cut_col - base_col
            | None -> max_int / 2
          in
          let offset_orig = first_col - base_col in
          let edit = { lnum = line.lnum; offset_orig;
                       offset_modif = offset_orig;
                       base_col; max_text_length } in
          revedits := edit :: !revedits;
          let revtokens = line_revtokens line in
          let bol = not (line.continued || line.continues) in
          let toks =
            List.rev_map begin fun (token, offset, tok_length) ->
              let tok_indent = Source_lines.column src line offset - base_col in
              token, { tok_indent; tok_length; tok_edit = { bol; edit } }
            end revtokens
          in
          (* only the first token of a line may change its indentation *)
          let toks = match toks with
            | (token, descr) :: toks ->
                (token, descr) ::
                List.map (fun (token, descr) ->
                    token, { descr with tok_edit = { bol = false; edit } })
                  toks
            | [] -> []
          in
          revtoks := List.rev_append toks !revtoks;
          maybe_comment_entry :=
            Source_lines.has_indicator line.format &&
            match revtokens with
            | (INFORMATION _, _, _) :: _
            | (DOT, _, _) :: (PROGRAM_ID, _, _) :: _ -> true
            | _ -> false
        end
  end src.lines;
  { toks = List.rev !revtoks; edits = List.rev !revedits }

let positioned_tokens (src: Source_lines.t) =
  Lexer.init ();
  List.concat_map begin fun (line: Source_lines.line) ->
    List.rev_map (fun (token, offset, _) -> token, line.lnum, offset)
      (line_revtokens line)
  end (Array.to_list src.lines)
