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

(** Notifications from clients to servers. *)

type t =
  | Initialized
  | Cancelled of { request_id: Jsonrpc.Id.t; reason: string option }
  | Unknown_notification of Jsonrpc.Notification.t

let of_jsonrpc (n: Jsonrpc.Notification.t) =
  let params = (n.params :> Json.t option) in
  match n.method_, params with
  | "notifications/initialized", _ ->
      Initialized
  | "notifications/cancelled", Some params ->
      (try
         Cancelled { request_id = Jsonrpc.Id.t_of_yojson
                         (Json.field "requestId" params);
                     reason = Json.string_opt "reason" params }
       with Jsonrpc.Json.Of_json _ -> Unknown_notification n)
  | _ ->
      Unknown_notification n
