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

open Types

type _ t =
  | Initialize : Initialize.Params.t -> Initialize.Result.t t
  | Ping : unit t
  | Discover : Discover.Params.t -> Discover.Result.t t
  | Tools_list : Tools_list.Params.t -> Tools_list.Result.t t
  | Tools_call : Tools_call.Params.t -> Tools_call.Result.t t
  | Unknown_request : string * Json.t option -> Json.t t

type packed = E : 'r t -> packed

let method_ : type r. r t -> string = function
  | Initialize _ -> "initialize"
  | Ping -> "ping"
  | Discover _ -> "server/discover"
  | Tools_list _ -> "tools/list"
  | Tools_call _ -> "tools/call"
  | Unknown_request (meth, _) -> meth

let meta : type r. r t -> Request_meta.t = function
  | Initialize _ | Ping -> Request_meta.empty
  | Discover { meta } -> meta
  | Tools_list { meta; _ } -> meta
  | Tools_call { meta; _ } -> meta
  | Unknown_request (_, params) -> Request_meta.of_params params

let of_jsonrpc ({ method_; params; _ }: Jsonrpc.Request.t) =
  let params = (params :> Json.t option) in
  let params_or_empty = Option.value params ~default:(`Assoc []) in
  try
    Ok begin match method_ with
      | "initialize" ->
          E (Initialize (Initialize.Params.t_of_yojson params_or_empty))
      | "ping" ->
          E Ping
      | "server/discover" ->
          E (Discover (Discover.Params.t_of_yojson params_or_empty))
      | "tools/list" ->
          E (Tools_list (Tools_list.Params.t_of_yojson params_or_empty))
      | "tools/call" ->
          E (Tools_call (Tools_call.Params.t_of_yojson params_or_empty))
      | meth ->
          E (Unknown_request (meth, params))
    end
  with Jsonrpc.Json.Of_json (msg, _) ->
    Error (Printf.sprintf "Invalid parameters for %s: %s" method_ msg)

let params : type r. r t -> Json.t option = function
  | Initialize p -> Some (Initialize.Params.yojson_of_t p)
  | Ping -> None
  | Discover p -> Some (Discover.Params.yojson_of_t p)
  | Tools_list p -> Some (Tools_list.Params.yojson_of_t p)
  | Tools_call p -> Some (Tools_call.Params.yojson_of_t p)
  | Unknown_request (_, p) -> p

let to_jsonrpc ~id r =
  let params = match params r with
    | Some (`Assoc _ | `List _ as p) -> Some p
    | _ -> None
  in
  Jsonrpc.Request.create ~id ~method_:(method_ r) ?params ()

let yojson_of_result
  : type r. version:_ -> ?server_info:_ -> r t -> r -> Json.t =
  fun ~version ?server_info req result ->
  match req with
  | Initialize _ -> Initialize.Result.yojson_of_t result
  | Ping -> Empty_result.yojson_of_t ~version ?server_info result
  | Discover _ -> Discover.Result.yojson_of_t ~version ?server_info result
  | Tools_list _ -> Tools_list.Result.yojson_of_t ~version ?server_info result
  | Tools_call _ -> Tools_call.Result.yojson_of_t ~version ?server_info result
  | Unknown_request _ -> result

let result_of_yojson : type r. r t -> Json.t -> r = fun req json ->
  match req with
  | Initialize _ -> Initialize.Result.t_of_yojson json
  | Ping -> ()
  | Discover _ -> Discover.Result.t_of_yojson json
  | Tools_list _ -> Tools_list.Result.t_of_yojson json
  | Tools_call _ -> Tools_call.Result.t_of_yojson json
  | Unknown_request _ -> json
