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

(** Requests from clients to servers, typed with the type of their result, in
    the style of [Lsp.Client_request]. *)

open Types

type _ t =
  | Initialize : Initialize.Params.t -> Initialize.Result.t t
  | Ping : unit t
  | Discover : Discover.Params.t -> Discover.Result.t t
  | Tools_list : Tools_list.Params.t -> Tools_list.Result.t t
  | Tools_call : Tools_call.Params.t -> Tools_call.Result.t t
  | Unknown_request : string * Json.t option -> Json.t t
      (** method and parameters of any other request *)

type packed = E : 'r t -> packed

val method_: _ t -> string

(** [_meta] field of the parameters of the request; empty for requests of
    protocol versions before 2026-07-28. *)
val meta: _ t -> Request_meta.t

(** Decodes a request; fails with a message on invalid parameters. *)
val of_jsonrpc: Jsonrpc.Request.t -> (packed, string) result

val to_jsonrpc: id:Jsonrpc.Id.t -> _ t -> Jsonrpc.Request.t

(** [yojson_of_result ~version request result] encodes [result] for a request
    of protocol version [version].  [server_info] is given in the [_meta] field
    of results of version 2026-07-28 on. *)
val yojson_of_result
  : version:Protocol_version.t
  -> ?server_info:Implementation.t
  -> 'r t -> 'r -> Json.t

val result_of_yojson: 'r t -> Json.t -> 'r
