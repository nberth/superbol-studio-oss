(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2026 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

(** Tools for language models (e.g. AI agents), built on what the language
    server knows of the workspace.  Clients expose them, e.g. as language
    model tools in VS Code, or through an MCP server; their outputs are text
    meant for language models. *)

(** Definition of a tool, as clients expose it. *)
type tool =
  {
    name: string;                         (** unique name of the tool *)
    display_name: string;
    reference_name: string;           (** to refer to the tool in prompts *)
    icon: string;                        (** product icon, e.g. [$(warning)] *)
    user_description: string;            (** description for users *)
    model_description: string;           (** description for models *)
    input_schema: Yojson.Safe.t;        (** JSON schema of an object *)
  }

(** Every tool. *)
val definitions: tool list

type result =
  {
    text: string;
    is_error: bool;   (** whether [text] explains why there is no result *)
  }

(** [call registry ~name ~arguments] runs the tool [name] with the given
    arguments (as described by its input schema).  Tools that work on a given
    file (outline and control flow) need that file to be open. *)
val call
  : Lsp_server.t
  -> name: string
  -> arguments: (string * Yojson.Safe.t) list
  -> result
