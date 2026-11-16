(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*                                                                        *)
(*  Copyright (c) 2026 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This source code is licensed under the MIT license found in the       *)
(*  LICENSE.md file in the root directory of this source tree.            *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

(** Tools that language models may invoke, e.g. in agent mode.  Every tool
    registered here must be declared in the [languageModelTools] contribution
    point of the extension manifest, with the same name. *)

(** Name of the tool that gives the outline of a COBOL source file. *)
val outline_tool_name: string

(** [register instance] registers every tool, unless the editor lacks the
    corresponding API. *)
val register: Superbol_instance.t -> unit
