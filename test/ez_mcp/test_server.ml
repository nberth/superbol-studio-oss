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

(** Tests of the MCP server, on sessions given as the lines a client would
    send, for protocol versions that start with [initialize] and for those
    that do not. *)

open Ez_mcp
open Ez_mcp.Types

let echo_tool : Server.tool =
  {
    tool = Tool.create () ~name:"echo" ~description:"Echoes its input."
        ~input_schema:(`Assoc [
            "type", `String "object";
            "properties", `Assoc ["text", `Assoc ["type", `String "string"]];
            "required", `List [`String "text"];
          ])
        ~annotations:(Tool_annotations.create () ~read_only_hint:true);
    call = (fun args ->
        Tools_call.Result.text (Json.string (Json.field "text" (`Assoc args))));
  }

let failing_tool : Server.tool =
  {
    tool = Tool.create () ~name:"fail" ~input_schema:(`Assoc [
        "type", `String "object" ]);
    call = (fun _ -> failwith "boom");
  }

let server () =
  Server.create ~log:(Printf.printf "LOG: %s\n") {
    server_info = Implementation.create () ~name:"test" ~version:"1.0";
    instructions = Some "Use the tools.";
    tools = [ echo_tool; failing_tool ];
  }

(** Feeds each line to [server], and prints the replies. *)
let session server lines =
  List.iter begin fun line ->
    match Jsonrpc.Packet.t_of_yojson (Yojson.Safe.from_string line) with
    | packet ->
        Option.iter (fun reply ->
            print_endline @@ Yojson.Safe.to_string @@
            Jsonrpc.Packet.yojson_of_t reply)
          (Server.handle_packet server packet)
    | exception e ->
        Printf.printf "INVALID: %s\n" (Printexc.to_string e)
  end lines

let%expect_test "session started with initialize" =
  session (server ()) [
    {|{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"client","version":"0.1"}}}|};
    {|{"jsonrpc":"2.0","method":"notifications/initialized"}|};
    {|{"jsonrpc":"2.0","id":2,"method":"ping"}|};
    {|{"jsonrpc":"2.0","id":3,"method":"tools/list"}|};
    {|{"jsonrpc":"2.0","id":4,"method":"tools/call","params":{"name":"echo","arguments":{"text":"hello"}}}|};
    {|{"jsonrpc":"2.0","id":5,"method":"tools/call","params":{"name":"echo","arguments":{}}}|};
    {|{"jsonrpc":"2.0","id":6,"method":"tools/call","params":{"name":"fail"}}|};
    {|{"jsonrpc":"2.0","id":7,"method":"tools/call","params":{"name":"nope"}}|};
    {|{"jsonrpc":"2.0","id":8,"method":"tools/call","params":{}}|};
    {|{"jsonrpc":"2.0","id":9,"method":"prompts/list"}|};
    {|{"jsonrpc":"2.0","method":"notifications/roots/list_changed"}|};
  ];
  [%expect {|
    {"id":1,"jsonrpc":"2.0","result":{"protocolVersion":"2025-06-18","capabilities":{"tools":{"listChanged":false}},"serverInfo":{"name":"test","version":"1.0"},"instructions":"Use the tools."}}
    {"id":2,"jsonrpc":"2.0","result":{}}
    {"id":3,"jsonrpc":"2.0","result":{"tools":[{"name":"echo","description":"Echoes its input.","inputSchema":{"type":"object","properties":{"text":{"type":"string"}},"required":["text"]},"annotations":{"readOnlyHint":true}},{"name":"fail","inputSchema":{"type":"object"}}]}}
    {"id":4,"jsonrpc":"2.0","result":{"content":[{"type":"text","text":"hello"}]}}
    {"id":5,"jsonrpc":"2.0","result":{"content":[{"type":"text","text":"Invalid arguments: missing field text"}],"isError":true}}
    LOG: Tool fail failed: Failure("boom")
    {"id":6,"jsonrpc":"2.0","result":{"content":[{"type":"text","text":"Internal error of tool fail"}],"isError":true}}
    {"id":7,"jsonrpc":"2.0","error":{"code":-32602,"message":"Unknown tool: nope"}}
    {"id":8,"jsonrpc":"2.0","error":{"code":-32602,"message":"Invalid parameters for tools/call: missing field name"}}
    {"id":9,"jsonrpc":"2.0","error":{"code":-32601,"message":"Method not found: prompts/list"}}
    LOG: Ignored notification: notifications/roots/list_changed |}]

let%expect_test "initialize with an unknown version" =
  session (server ()) [
    {|{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2099-01-01","capabilities":{},"clientInfo":{"name":"client","version":"0.1"}}}|};
  ];
  [%expect {| {"id":1,"jsonrpc":"2.0","result":{"protocolVersion":"2025-11-25","capabilities":{"tools":{"listChanged":false}},"serverInfo":{"name":"test","version":"1.0"},"instructions":"Use the tools."}} |}]

let meta = {|"_meta":{"io.modelcontextprotocol/protocolVersion":"2026-07-28","io.modelcontextprotocol/clientCapabilities":{}}|}

let%expect_test "stateless session" =
  session (server ()) [
    {|{"jsonrpc":"2.0","id":1,"method":"server/discover","params":{|} ^ meta ^ {|}}|};
    {|{"jsonrpc":"2.0","id":2,"method":"tools/list","params":{|} ^ meta ^ {|}}|};
    {|{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{|} ^ meta ^ {|,"name":"echo","arguments":{"text":"hi"}}}|};
    {|{"jsonrpc":"2.0","id":4,"method":"tools/list","params":{"_meta":{"io.modelcontextprotocol/protocolVersion":"2099-01-01"}}}|};
  ];
  [%expect {|
    {"id":1,"jsonrpc":"2.0","result":{"supportedVersions":["2026-07-28","2025-11-25","2025-06-18","2025-03-26","2024-11-05"],"capabilities":{"tools":{"listChanged":false}},"instructions":"Use the tools.","cacheScope":"private","ttlMs":0,"resultType":"complete","_meta":{"io.modelcontextprotocol/serverInfo":{"name":"test","version":"1.0"}}}}
    {"id":2,"jsonrpc":"2.0","result":{"tools":[{"name":"echo","description":"Echoes its input.","inputSchema":{"type":"object","properties":{"text":{"type":"string"}},"required":["text"]},"annotations":{"readOnlyHint":true}},{"name":"fail","inputSchema":{"type":"object"}}],"cacheScope":"private","ttlMs":0,"resultType":"complete","_meta":{"io.modelcontextprotocol/serverInfo":{"name":"test","version":"1.0"}}}}
    {"id":3,"jsonrpc":"2.0","result":{"content":[{"type":"text","text":"hi"}],"resultType":"complete","_meta":{"io.modelcontextprotocol/serverInfo":{"name":"test","version":"1.0"}}}}
    {"id":4,"jsonrpc":"2.0","error":{"data":{"requested":"2099-01-01","supported":["2026-07-28","2025-11-25","2025-06-18","2025-03-26","2024-11-05"]},"code":-32022,"message":"Unsupported protocol version: 2099-01-01"}} |}]
