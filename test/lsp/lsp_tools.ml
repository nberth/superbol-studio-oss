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

(** Tests of the tools for language models. *)

open Lsp_testing

module TOOLS = Cobol_lsp.Tools

let prog = {cobol|
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYROLL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-REC.
          05 WS-AMOUNT   PIC 9(5)V99.
          05 WS-STATUS   PIC X.
             88 WS-OK    VALUE "Y".
       77 WS-COUNT       PIC 9(3) VALUE 0.
       PROCEDURE DIVISION.
       MAIN-SECTION SECTION.
       MAIN-PARA.
           PERFORM INIT-PARA
           PERFORM CALC-TAX
           IF WS-OK
              GO TO END-PARA
           END-IF.
       NEXT-PARA.
           ADD 1 TO WS-COUNT.
       END-PARA.
           STOP RUN.
       WORK-SECTION SECTION.
       INIT-PARA.
           MOVE 0 TO WS-AMOUNT.
       CALC-TAX.
           COMPUTE WS-AMOUNT = WS-AMOUNT * 2.
|cobol}

let broken = {cobol|
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BROKEN.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 X PIC 9(3.
       PROCEDURE DIVISION.
           STOP RUN.
|cobol}

(** Calls the tool [name] with [arguments], and prints its result; gives the
    resulting server. *)
let call ?open_files server name arguments =
  let TOOLS.{ text; is_error }, server =
    TOOLS.call ?open_files server ~name ~arguments in
  Printf.printf "### %s%s\n%s\n" name (if is_error then " (error)" else "") text;
  server

let file projdir name = "filePath", `String (Filename.concat projdir name)

let%expect_test "tools on open documents" =
  let { end_with_postproc; projdir }, server = make_lsp_project () in
  let server, _ = add_cobol_doc server ~projdir "payroll.cob" prog in
  let server, _ = add_cobol_doc server ~projdir "broken.cob" broken in
  let server = call server "superbol_cobol_outline" [file projdir "payroll.cob"] in
  let server = call server "superbol_cobol_outline"
      [file projdir "payroll.cob"; "maxDepth", `Int 3] in
  let server = call server "superbol_cobol_find_symbol"
      ["query", `String "ws"; "limit", `Int 3] in
  let server = call server "superbol_cobol_diagnostics" [] in
  let server = call server "superbol_cobol_diagnostics"
      [file projdir "payroll.cob"] in
  let server = call server "superbol_cobol_project_settings"
      [file projdir "payroll.cob"] in
  let server = call server "superbol_cobol_control_flow"
      [file projdir "payroll.cob"] in
  let server = call server "superbol_cobol_control_flow"
      [file projdir "payroll.cob"; "name", `String "WORK-SECTION (PAYROLL)"] in
  let server = call server "superbol_cobol_control_flow"
      [file projdir "payroll.cob"; "name", `String "NOPE"] in
  let server = call server "superbol_cobol_outline" [] in
  let server = call server "superbol_cobol_find_symbol" ["query", `Int 3] in
  let _server = call server "nope" [] in
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/payroll.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    {"params":{"diagnostics":[{"message":"Missing <picture character string> .","range":{"end":{"character":19,"line":5},"start":{"character":19,"line":5}},"severity":4},{"message":"Invalid syntax","range":{"end":{"character":20,"line":5},"start":{"character":19,"line":5}},"severity":1}],"uri":"file://__rootdir__/broken.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    ### superbol_cobol_outline
    Outline of payroll.cob (lines are 1-based; indentation shows nesting):
    PROGRAM-ID. PAYROLL  [L2-27]
      DATA DIVISION  [L4-10]
        WORKING-STORAGE SECTION  [L5-10]
          01 WS-REC  [L6-9]
            05 WS-AMOUNT  [L7]
            05 WS-STATUS  [L8-9]
              88 WS-OK  [L9]
          77 WS-COUNT  [L10]
      PROCEDURE DIVISION  [L11-27]
        MAIN-SECTION SECTION  [L12-22]
          MAIN-PARA  [L13-18]
          NEXT-PARA  [L19-20]
          END-PARA  [L21-22]
        WORK-SECTION SECTION  [L23-27]
          INIT-PARA  [L24-25]
          CALC-TAX  [L26-27]
    ### superbol_cobol_outline
    Outline of payroll.cob (lines are 1-based; indentation shows nesting):
    PROGRAM-ID. PAYROLL  [L2-27]
      DATA DIVISION  [L4-10]
        WORKING-STORAGE SECTION  [L5-10]  (5 nested symbols hidden)
      PROCEDURE DIVISION  [L11-27]
        MAIN-SECTION SECTION  [L12-22]  (3 nested symbols hidden)
        WORK-SECTION SECTION  [L23-27]  (2 nested symbols hidden)
    ### superbol_cobol_find_symbol
    Symbols matching "ws", best matches first (lines are 1-based):
    data item WS-AMOUNT (in PAYROLL): payroll.cob [L7]
    data item WS-COUNT (in PAYROLL): payroll.cob [L10]
    condition-name WS-OK (in PAYROLL): payroll.cob [L9]
    ... more matches omitted; refine the query, or raise `limit`.
    ### superbol_cobol_diagnostics
    SuperBOL reports 1 error and 0 warnings in the workspace (lines and columns are 1-based):
    broken.cob:6:20: error: Invalid syntax
    broken.cob:6:20: hint: Missing <picture character string> .
    ### superbol_cobol_diagnostics
    SuperBOL reports no problems in payroll.cob.
    {"params":{"message":"Getting configuration for project of file://__rootdir__/payroll.cob","type":3},"method":"window/logMessage","jsonrpc":"2.0"}
    ### superbol_cobol_project_settings
    SuperBOL settings of the project that holds payroll.cob:
    cobol.dialect: "default"
    cobol.sourceFormat: "auto"
    cobol.sourceFormatByExtension: {}
    cobol.copybooks: [{"dir":"."}]
    cobol.copyexts: ["cpy","cbl","cob"]
    cobol.excludeDirs: ["_build","_opam","node_modules"]
    cobol.mfComment: false
    ### superbol_cobol_control_flow
    Control flow graph of PAYROLL in payroll.cob.  For each node (paragraph, section, or entry point): the nodes it performs (PERFORM), goes to (GO TO), and falls through to (continuing with the next node in sequence); lines are 1-based.
    Entry point: falls through to MAIN-PARA IN MAIN-SECTION
    MAIN-PARA IN MAIN-SECTION [L13]: performs INIT-PARA IN WORK-SECTION, CALC-TAX IN WORK-SECTION; goes to END-PARA IN MAIN-SECTION; falls through to NEXT-PARA IN MAIN-SECTION
    NEXT-PARA IN MAIN-SECTION [L19]: falls through to END-PARA IN MAIN-SECTION
    END-PARA IN MAIN-SECTION [L21]: no successor (returns to its performer, or ends the run)
    INIT-PARA IN WORK-SECTION [L24]: falls through to CALC-TAX IN WORK-SECTION
    CALC-TAX IN WORK-SECTION [L26]: no successor (returns to its performer, or ends the run)
    Other graphs available in the same file (with `name`):
    - MAIN-SECTION (PAYROLL)
    - WORK-SECTION (PAYROLL)
    ### superbol_cobol_control_flow
    Control flow graph of WORK-SECTION (PAYROLL) in payroll.cob.  For each node (paragraph, section, or entry point): the nodes it performs (PERFORM), goes to (GO TO), and falls through to (continuing with the next node in sequence); lines are 1-based.
    WORK-SECTION: falls through to INIT-PARA
    INIT-PARA [L24]: falls through to CALC-TAX
    CALC-TAX [L26]: no successor (returns to its performer, or ends the run)
    Other graphs available in the same file (with `name`):
    - PAYROLL
    - MAIN-SECTION (PAYROLL)
    ### superbol_cobol_control_flow (error)
    No control flow graph named "NOPE" in payroll.cob; available graphs are:
    - PAYROLL
    - MAIN-SECTION (PAYROLL)
    - WORK-SECTION (PAYROLL)
    ### superbol_cobol_outline (error)
    No file was given (`filePath`).
    ### superbol_cobol_find_symbol (error)
    `query` must be a string.
    ### nope (error)
    Unknown tool: nope |}]

let%expect_test "tools on documents that are not open" =
  let { end_with_postproc; projdir }, server = make_lsp_project () in
  let server, _ = add_cobol_doc server ~projdir "payroll.cob" prog in
  let server =
    LSP.Server.did_close { textDocument = { uri = Lsp.Uri.of_path @@
                                            Filename.concat projdir
                                              "payroll.cob" } } server in
  let server = call server "superbol_cobol_outline"
      [file projdir "payroll.cob"; "maxDepth", `Int 2] in
  let server = call ~open_files:true server "superbol_cobol_outline"
      [file projdir "payroll.cob"; "maxDepth", `Int 2] in
  (* The document was closed again. *)
  let server = call server "superbol_cobol_control_flow"
      [file projdir "payroll.cob"] in
  let _server = call ~open_files:true server "superbol_cobol_outline"
      [file projdir "missing.cob"] in
  end_with_postproc [%expect.output];
  [%expect {|
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/payroll.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    ### superbol_cobol_outline (error)
    payroll.cob is not open in the editor.
    {"params":{"diagnostics":[],"uri":"file://__rootdir__/payroll.cob"},"method":"textDocument/publishDiagnostics","jsonrpc":"2.0"}
    ### superbol_cobol_outline
    Outline of payroll.cob (lines are 1-based; indentation shows nesting):
    PROGRAM-ID. PAYROLL  [L2-27]
      DATA DIVISION  [L4-10]  (6 nested symbols hidden)
      PROCEDURE DIVISION  [L11-27]  (7 nested symbols hidden)
    ### superbol_cobol_control_flow (error)
    payroll.cob is not open in the editor.
    ### superbol_cobol_outline (error)
    There is no file __rootdir__/missing.cob. |}]

let%expect_test "definitions without editor" =
  List.iter begin fun TOOLS.{ name; input_schema; _ } ->
    Printf.printf "%s: %s\n" name
      (Yojson.Safe.to_string @@ Yojson.Safe.Util.member "required" input_schema)
  end (TOOLS.definitions ~with_editor:false);
  [%expect {|
    superbol_cobol_outline: ["filePath"]
    superbol_cobol_find_symbol: ["query"]
    superbol_cobol_diagnostics: null
    superbol_cobol_project_settings: null
    superbol_cobol_control_flow: ["filePath"] |}]
