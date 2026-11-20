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

type result = {
  toks : (Types.token * Types.token_descr) list ;
  edits : Types.indent_record list ;
}

(** Tokens for {!Engine.indent_tokens}, with the records of the lines whose
    indentation may change. *)
val tokenize : config:Types.config -> Source_lines.t -> result

(** Tokens of all the lines, in order, along with their line number (1-based)
    and byte offset in the line.  A word that continues on later lines (e.g, a
    continued alphanumeric literal) gives a [CHARS] token with the
    (upper-cased) text up to the end of its first line. *)
val positioned_tokens : Source_lines.t -> (Types.token * int * int) list
