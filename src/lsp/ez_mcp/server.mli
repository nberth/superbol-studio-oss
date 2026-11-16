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

(** A server that provides tools.

    The server negotiates the protocol version with [initialize] for versions
    before 2026-07-28, and per request (from the [_meta] field of their
    parameters) afterwards.  It handles every request synchronously, in the
    order they come. *)

open Types

type tool =
  {
    tool: Tool.t;
    call: (string * Json.t) list -> Tools_call.Result.t;
    (** Computes the result of a call, given its arguments.  Errors that the
        language model may act upon (e.g. invalid arguments) should be
        reported with [is_error = Some true], not raised: exceptions turn into
        results with a generic error message. *)
  }

type config =
  {
    server_info: Implementation.t;
    instructions: string option;
    (** How to use the server, which clients may give to language models. *)
    tools: tool list;
  }

type t

val create: ?log:(string -> unit) -> config -> t

(** [handle_packet server packet] is the reply to [packet], if any. *)
val handle_packet: t -> Jsonrpc.Packet.t -> Jsonrpc.Packet.t option

(** [run server ic oc] serves the requests read from [ic] until the end of
    the input, replying on [oc].  Nothing else may be written on [oc]: log
    elsewhere, e.g. on [stderr]. *)
val run: t -> in_channel -> out_channel -> unit
