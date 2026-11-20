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

type warning =
  {
    line: int;                         (** line in the input text (1-based) *)
    message: string;
  }

(** Target source format *)
type target

(** [target format] describes [format] as a target of the conversion.
    Supported formats are FIXED, COBOL85, VARIABLE, XCARD, COBOLX, and FREE;
    gives an error message for other formats. *)
val target: Cobol_config.source_format -> (target, string) result

(** Same as {!target}, for a format given by its name (case insensitive). *)
val target_of_name: string -> (target, string) result

(** Names of the supported target formats. *)
val supported_target_names: string list

(** [convert ?keep_sequence_numbers ?add_source_format_directive ~target src
    ~eol] gives the text of [src] in the [target] format, with lines separated
    by [eol], along with warnings about the conversion.

    - Each line of [src] gives one line, keeping the column of its first word
      relatively to area A, unless it is too long for the text area of
      [target]: it is then moved to the left (without leaving area B), and
      then split between words; an alphanumeric literal that does not fit is
      continued on the next line(s), with a [-] indicator.
    - Comment lines, floating comments, and debugging lines are written as
      [target] requires: with an indicator ([*], [/], [D]), or with [*>] and
      [>>D] markers in FREE format.  In formats with an indicator, floating
      comments are moved to comment lines just before the line they were on;
      they stay at the end of their line in FREE format.
    - Directives that change the source format are commented out; if
      [add_source_format_directive] holds ([false] by default), a
      [>>SOURCE FORMAT IS] directive for [target] is inserted as first line.
      Note that, with Micro Focus dialects, [FREE] is read as the X/Open
      free format.
    - The sequence area, if any, is blank, unless [keep_sequence_numbers]
      holds and the line was in a format with a sequence area already;
      nothing is written after the text area. *)
val convert
  : ?keep_sequence_numbers: bool
  -> ?add_source_format_directive: bool
  -> target: target
  -> Source_lines.t
  -> eol: string
  -> string * warning list

(** End-of-line sequence of the first line of the given text ("\r\n" or
    "\n"). *)
val eol_of_text: string -> string
