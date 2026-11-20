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

(** A session with the MCP server of [superbol mcp]. *)

open Lsp_testing

let prog = {cobol|
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYROLL.
       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM CALC-TAX
           STOP RUN.
       CALC-TAX.
           DISPLAY "TAX".
|cobol}

(** Feeds each line to [server], and prints the text of results, or the
    replies themselves. *)
let session server lines =
  List.iter begin fun line ->
    let packet = Jsonrpc.Packet.t_of_yojson (Yojson.Safe.from_string line) in
    match Ez_mcp.Server.handle_packet server packet with
    | Some (Response { result = Ok json; _ }) ->
        let open Yojson.Safe.Util in
        begin match member "content" json, member "tools" json with
          | _, _ when member "protocolVersion" json <> `Null ->
              Printf.printf "initialized: %s, %s\n"
                (to_string @@ member "protocolVersion" json)
                (to_string @@ member "name" @@ member "serverInfo" json)
          | `List (text :: _), _ ->
              Printf.printf "%s%s\n"
                (if member "isError" json = `Bool true then "(error) " else "")
                (to_string @@ member "text" text)
          | _, `List tools ->
              List.iter (fun t ->
                  Printf.printf "tool %s, requires %s\n"
                    (to_string @@ member "name" t)
                    (Yojson.Safe.to_string @@
                     member "required" @@ member "inputSchema" t)) tools
          | _ ->
              print_endline (Yojson.Safe.to_string json)
        end
    | Some reply ->
        print_endline @@ Yojson.Safe.to_string @@
        Jsonrpc.Packet.yojson_of_t reply
    | None ->
        ()
  end lines

let%expect_test "mcp session" =
  let { end_with_postproc; projdir }, _ = make_lsp_project () in
  Ez_file.V1.EzFile.write_file (Filename.concat projdir "payroll.cob") prog;
  Cobol_lsp.IO.set_output (Messages_to Format.std_formatter);
  Fun.protect ~finally:(fun () -> Cobol_lsp.IO.set_output Client) begin fun () ->
    let server =
      Superbol_free_lib.Command_mcp.mcp_server ~workspace:projdir
        ~log:print_endline
        { enable_caching = false; force_syntax_diagnostics = true;
          storage_directory = None }
    in
    let call id name args =
      Printf.sprintf {|{"jsonrpc":"2.0","id":%d,"method":"tools/call","params":{"name":"%s","arguments":%s}}|}
        id name args
    in
    let file = Printf.sprintf {|{"filePath":"%s/payroll.cob"}|} projdir in
    session server [
      {|{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"test","version":"0"}}}|};
      {|{"jsonrpc":"2.0","method":"notifications/initialized"}|};
      {|{"jsonrpc":"2.0","id":2,"method":"tools/list"}|};
      call 3 "superbol_cobol_outline" file;
      call 4 "superbol_cobol_control_flow" file;
      call 5 "superbol_cobol_control_flow" "{}";
    ]
  end;
  end_with_postproc [%expect.output];
  [%expect {|
    initialized: 2025-06-18, superbol
    tool superbol_cobol_outline, requires ["filePath"]
    tool superbol_cobol_find_symbol, requires ["query"]
    tool superbol_cobol_diagnostics, requires null
    tool superbol_cobol_project_settings, requires null
    tool superbol_cobol_control_flow, requires ["filePath"]
    Outline of payroll.cob (lines are 1-based; indentation shows nesting):
    PROGRAM-ID. PAYROLL  [L2-9]
      PROCEDURE DIVISION  [L4-9]
        MAIN-PARA  [L5-7]
        CALC-TAX  [L8-9]
    Control flow graph of PAYROLL in payroll.cob.  For each node (paragraph, section, or entry point): the nodes it performs (PERFORM), goes to (GO TO), and falls through to (continuing with the next node in sequence); lines are 1-based.
    Entry point: falls through to MAIN-PARA
    MAIN-PARA [L5]: performs CALC-TAX
    CALC-TAX [L8]: no successor (returns to its performer, or ends the run)
    (error) No file was given (`filePath`). |}]
