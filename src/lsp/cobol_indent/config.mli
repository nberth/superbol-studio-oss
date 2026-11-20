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

val fixed_format : Types.source_format

val source_format :
  Cobol_config.Types.source_format_spec -> Types.source_format


(* [load ~source_format ~unparsed ~filename ()] starts from the default
   configuration with [source_format], applies the options in [unparsed]
   (e.g, from the [indent] table of the project configuration; unknown
   options are ignored), and then the contents of all ".superbol-indent"
   files in the directories that contain [filename], from the outermost to the
   innermost.

   Syntax of .superbol-indent:

   * # on first char for indent
   * format = fixed
   * arg-offset = 3
   * inner-offset = 3

   The format is ignored by {!Main.indent}, that takes it from the
   preprocessor options instead.
 *)
val load :
  platform:Cobol_common.Types.platform ->
  source_format:Types.source_format ->
  ?unparsed:Types.unparsed_config ->
  filename:string -> unit -> Types.config

val to_string : Types.config -> string

val generate : ?config:Types.config -> ?only_comment:bool -> string -> unit
