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

(** Arguments of commands that analyze files within their projects, on top of
    {!Common_args}. *)

(** [get ()] is the arguments, along with a function that applies them to a
    setup ({!Project_commands.default_setup} by default). *)
val get
  : unit
  -> (?setup:Project_commands.setup -> unit -> Project_commands.setup)
     * Ezcmd.V2.EZCMD.TYPES.arg_list

(** [encoding ()] is the [--encoding] argument (part of those {!get} gives),
    along with a function that gives the encoding it designates, if supported,
    once arguments are parsed. *)
val encoding
  : unit
  -> (unit -> Cobol_common.Text_encoding.t option)
     * Ezcmd.V2.EZCMD.TYPES.arg_list

(** [client_settings ~doc ()] is the [--client-settings] argument, for
    settings of the [superbol.cobol] section of an LSP client given as a JSON
    object (described with [doc]), along with a function that gives its value
    once arguments are parsed. *)
val client_settings
  : doc:string
  -> unit
  -> (unit -> (string * Yojson.Safe.t) list option)
     * Ezcmd.V2.EZCMD.TYPES.arg_list
