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

(** Format-independent view of the lines of a source text, obtained from
    {!Cobol_preproc.fold_source_lines}. *)

(** Source text word, with byte offsets in the line where it starts. *)
type word =
  {
    start_offset: int;
    end_offset: int option;  (** [None] if the word continues on a later line
                                 (e.g, continued alphanumeric literal) *)
    text: string option;   (** text of the word on a single line, for words
                               whose raw text may not give it exactly:
                               alphanumeric literals (that may not be closed
                               before the end of the text area), and words
                               that continue on a later line *)
  }

(** Comment, with the byte offset in the line where it starts (on its marker,
    or the indicator of a comment line). *)
type comment =
  {
    comment_offset: int;
    comment_kind: [`Line | `Floating];
    comment_text: string;                     (** including the marker *)
  }

(** Debugging line that is not compiled, with the byte offset in the line
    where it starts (on its [D] indicator, or [>>D] marker). *)
type debug =
  {
    debug_offset: int;
    debug_kind: [`Indicator | `Floating];
    debug_text: string;                (** including the indicator or marker *)
  }

type line =
  {
    lnum: int;                                                 (** 1-based *)
    text: string;                    (** raw contents, without end-of-line *)
    format: Cobol_preproc.Src_format.any;    (** source format of the line *)
    words: word list;   (** words that start on the line, in order; empty for
                            blank, comment, and debugging lines *)
    comments: comment list;        (** comments that start on the line *)
    debug: debug option;      (** debugging line that starts on the line *)
    continued: bool;    (** the line continues a previous line: some word
                            that starts on a previous line ends on, or spans,
                            this line, or the line has a continuation
                            indicator *)
    continues: bool;    (** the line is continued on a later line *)
    source_format_directive: bool;  (** the line holds a compiler directive
                                        that changes the source format *)
  }

type t =
  {
    lines: line array;                        (** line [n] is at index [n-1] *)
    tab_stops: int list;
  }

val scan
  : options: Cobol_preproc.Options.preproc_options
  -> filename: string
  -> string
  -> t

(** 0-based visual column of the byte at the given offset in the line.  A
    byte order mark that starts the first line does not count. *)
val column: t -> line -> int -> int

(** Locates the given 0-based visual column in the line (see
    {!Cobol_common.Tab_stops.locate_column}); columns of tabs are 1-based. *)
val locate: t -> line -> int -> Cobol_common.Tab_stops.column_spot

(** 0-based visual column where the text area (area A) starts. *)
val text_column: Cobol_preproc.Src_format.any -> int

(** 0-based visual column where the text area ends (exclusive), if any. *)
val cut_column: Cobol_preproc.Src_format.any -> int option

(** Holds for formats with an indicator area. *)
val has_indicator: Cobol_preproc.Src_format.any -> bool
