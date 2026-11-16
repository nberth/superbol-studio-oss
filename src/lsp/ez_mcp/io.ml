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

let rec read ic =
  match In_channel.input_line ic with
  | None ->
      None
  | Some line when String.trim line = "" ->
      read ic
  | Some line ->
      Some begin
        try Ok (Jsonrpc.Packet.t_of_yojson (Yojson.Safe.from_string line))
        with
        | Yojson.Json_error msg -> Error ("Invalid JSON: " ^ msg)
        | Jsonrpc.Json.Of_json (msg, _) -> Error ("Invalid message: " ^ msg)
      end

let write oc packet =
  (* [Yojson.Safe.to_string] escapes newlines in strings: the message holds on
     one line. *)
  output_string oc (Yojson.Safe.to_string (Jsonrpc.Packet.yojson_of_t packet));
  output_char oc '\n';
  flush oc
