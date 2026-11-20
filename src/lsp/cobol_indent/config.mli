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

(* [load ~unparsed ~filename ()] starts from the default configuration,
   applies the options in [unparsed] (e.g, from the [indent] table of the
   project configuration; unknown options are ignored), and then the contents
   of all ".superbol-indent" files in the directories that contain
   [filename], from the outermost to the innermost.

   Syntax of .superbol-indent:

   * # on first char for indent
   * arg-offset = 3
   * inner-offset = 3

   A [format] option is accepted for backward compatibility, but ignored: the
   source format is taken from the preprocessor options instead.
 *)
val load :
  platform:Cobol_common.Types.platform ->
  ?unparsed:Types.unparsed_config ->
  filename:string -> unit -> Types.config

val to_string : Types.config -> string

val generate : ?config:Types.config -> ?only_comment:bool -> string -> unit
