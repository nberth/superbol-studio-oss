(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2026 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

(** The stdio transport of MCP: one JSON-RPC message per line, in UTF-8.
    Unlike in LSP, messages have no headers. *)

(** [read ic] reads the next message, skipping empty lines; [None] at the end
    of the input, and [Some (Error msg)] on a line that does not hold a valid
    message. *)
val read: in_channel -> (Jsonrpc.Packet.t, string) result option

(** [write oc packet] writes [packet] on a single line, and flushes [oc]. *)
val write: out_channel -> Jsonrpc.Packet.t -> unit
