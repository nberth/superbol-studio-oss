(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2024 OCamlPro SAS                                       *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

(** [indent ~options ~config ~filename ?contents ?range ()] computes the
    indentation of the lines of [contents] (read from [filename] if not
    given), restricted to the lines in [range].  Source formats, including
    changes of format via compiler directives, and tab stops are taken from
    the preprocessor [options]; [config] gives indentation options (see
    {!Config.load}).

    Returns the records of the lines whose indentation changes, and the
    corresponding edits, sorted by line and offset.  If [output] is given,
    the edits are applied and the result is written to that file, or to the
    standard output if [output] is ["-"]. *)
val indent :
  options:Cobol_preproc.Options.preproc_options ->
  config:Types.unparsed_config ->
  filename:string ->
  ?verbose:bool ->
  ?output:string ->
  ?contents:string ->
  ?range:Types.range ->
  unit ->
  Types.indent_record list * Types.line_edit list

(** Applies edits that are sorted by line and offset, and do not overlap. *)
val apply_edits : string -> Types.line_edit list -> string
