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

type tool =
  {
    tool: Tool.t;
    call: (string * Json.t) list -> Tools_call.Result.t;
  }

type config =
  {
    server_info: Implementation.t;
    instructions: string option;
    tools: tool list;
  }

type t =
  {
    config: config;
    log: string -> unit;
    mutable session_version: Protocol_version.t option;
    (** version negotiated with [initialize], if any *)
  }

let create ?(log = ignore) config =
  { config; log; session_version = None }

module Error = Jsonrpc.Response.Error

let unsupported_version_code = -32022

let capabilities : Server_capabilities.t =
  { tools = Some { list_changed = Some false } }

(** Version of a request: the one it gives, if any, or else the one of the
    session. *)
let version_for server (meta: Request_meta.t) =
  match meta.protocol_version, server.session_version with
  | Some v, _ when Protocol_version.is_supported v ->
      Ok v
  | Some v, _ ->
      Error (Error.make ()
               ~code:(Other unsupported_version_code)
               ~message:("Unsupported protocol version: " ^ v)
               ~data:(`Assoc [
                   "requested", `String v;
                   "supported", `List (List.map (fun v -> `String v)
                                         Protocol_version.supported)
                 ]))
  | None, Some v ->
      Ok v
  | None, None ->
      Ok Protocol_version.latest_with_initialization

let call_tool server (params: Tools_call.Params.t) =
  match
    List.find_opt (fun { tool; _ } -> tool.Tool.name = params.name)
      server.config.tools
  with
  | None ->
      Error (Error.make () ~code:InvalidParams
               ~message:("Unknown tool: " ^ params.name))
  | Some { call; _ } ->
      Ok (try call params.arguments with
          | Jsonrpc.Json.Of_json (msg, _) ->
              Tools_call.Result.text ~is_error:true ("Invalid arguments: " ^ msg)
          | e ->
              server.log ("Tool " ^ params.name ^ " failed: " ^
                          Printexc.to_string e);
              Tools_call.Result.text ~is_error:true
                ("Internal error of tool " ^ params.name))

let handle
  : type r. t -> version:Protocol_version.t -> r Client_request.t ->
    (r, Error.t) result =
  fun server ~version -> function
  | Initialize params ->
      let protocol_version =
        if Protocol_version.is_supported params.protocol_version &&
           not (Protocol_version.is_stateless params.protocol_version)
        then params.protocol_version
        else Protocol_version.latest_with_initialization
      in
      server.session_version <- Some protocol_version;
      Ok { protocol_version; capabilities;
           server_info = server.config.server_info;
           instructions = server.config.instructions }
  | Ping ->
      Ok ()
  | Discover _ ->
      ignore version;
      Ok { supported_versions = Protocol_version.supported; capabilities;
           instructions = server.config.instructions;
           cache = Cache.default }
  | Tools_list _ ->
      Ok { tools = List.map (fun { tool; _ } -> tool) server.config.tools;
           next_cursor = None; cache = Cache.default }
  | Tools_call params ->
      call_tool server params
  | Unknown_request (meth, _) ->
      Error (Error.make () ~code:MethodNotFound
               ~message:("Method not found: " ^ meth))

let handle_request server (req: Jsonrpc.Request.t) : Jsonrpc.Response.t =
  match Client_request.of_jsonrpc req with
  | Error message ->
      Jsonrpc.Response.error req.id (Error.make () ~code:InvalidParams ~message)
  | Ok (E request) ->
      match version_for server (Client_request.meta request) with
      | Error e ->
          Jsonrpc.Response.error req.id e
      | Ok version ->
          match handle server ~version request with
          | Ok result ->
              let version = match request with
                | Initialize _ ->
                    Option.value server.session_version ~default:version
                | _ -> version
              in
              Jsonrpc.Response.ok req.id @@
              Client_request.yojson_of_result ~version request result
                ~server_info:server.config.server_info
          | Error e ->
              Jsonrpc.Response.error req.id e
          | exception e ->
              Jsonrpc.Response.error req.id @@
              Error.make () ~code:InternalError
                ~message:(Printexc.to_string e)

let handle_notification server (n: Jsonrpc.Notification.t) =
  match Client_notification.of_jsonrpc n with
  | Initialized | Cancelled _ ->
      ()           (* requests are handled synchronously: nothing to cancel *)
  | Unknown_notification { method_; _ } ->
      server.log ("Ignored notification: " ^ method_)

let handle_packet server : Jsonrpc.Packet.t -> Jsonrpc.Packet.t option =
  function
  | Request req ->
      Some (Response (handle_request server req))
  | Notification n ->
      handle_notification server n;
      None
  | Batch_call calls ->
      (* Batches were part of version 2025-03-26 only. *)
      let responses =
        List.filter_map (function
            | `Request req -> Some (handle_request server req)
            | `Notification n -> handle_notification server n; None) calls
      in
      if responses = [] then None else Some (Batch_response responses)
  | Response _ | Batch_response _ ->
      None                       (* the server sends no request: ignore *)

let run server ic oc =
  let rec loop () =
    match Io.read ic with
    | None ->
        ()
    | Some (Error msg) ->
        server.log msg;
        loop ()
    | Some (Ok packet) ->
        Option.iter (Io.write oc) (handle_packet server packet);
        loop ()
  in
  loop ()
